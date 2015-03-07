;;;; cl-datalog.lisp

(in-package #:cl-datalog)

(defvar *aux-rules-package* (find-package '#:aux-rules))

;; Non-keyword symbols beginning with #\? are vars
(defun varp (thing)
  "Return t if THING is a variable, nil if it is a constant."
  (and (symbolp thing)
       (not (keywordp thing))
       (eql (elt (symbol-name thing) 0) #\?)))

;; Rules are a list of the form (conclusion . hypotheses), where
;; conclusion and each hypothesis is a list of the form (relation
;; &rest vars-and-constants). A rule with no hypotheses and all
;; constant values in the head is a fact, a rule with a nil conclusion
;; is a query, and anything else is a predicate.
(defun rulep (thing)
  "Return t if THING is a valid datalog rule."
  (and (listp thing)
       (>= (length thing) 1)
       (every #'listp thing)))

(deftype datalog-rule () '(satisfies rulep))

(defun make-rule (conclusion &rest hypotheses)
  (cons conclusion hypotheses))

(defun rule-conclusion (rule)
  (check-type rule datalog-rule)
  (car rule))

(defun rule-hypotheses (rule)
  (check-type rule datalog-rule)
  (cdr rule))

(defun factp (rule)
  "Return t if RULE is a fact, nil otherwise."
  (check-type rule datalog-rule)
  (let ((conclusion (rule-conclusion rule)))
    (and (null (rule-hypotheses rule))
         (not (null conclusion))
         (notany #'varp (cdr conclusion)))))

(defun queryp (rule)
  "Return t if RULE is a query, nil otherwise."
  (check-type rule datalog-rule)
  (and (null (rule-conclusion rule))
       (not (null (rule-hypotheses rule)))))

(defun form-vars (form)
  "Return a list of the variables in the query or hypothesis FORM."
  (remove-if-not #'varp (cdr form)))

;; TODO: negation will affect this (specifically the MEMBER check)
(defun vars-used-p (rule)
  (check-type rule datalog-rule)
  (let ((head-vars (form-vars (rule-conclusion rule)))
        (hypotheses (rule-hypotheses rule)))
    ;; There is at least one hypothesis that uses each var in the head
    (every (lambda (head-var)
             (some (lambda (hypothesis)
                     (member head-var (form-vars hypothesis)))
                   hypotheses))
           head-vars)))

;; TODO: valid var usage should probably part of the datalog-rule type...
(defun predicatep (rule)
  "Return t if RULE is a predicate, i.e. a valid datalog rule that is neither a fact nor a query."
  (check-type rule datalog-rule)
  (and (not (or (factp rule)
                (queryp rule)))
       (vars-used-p rule)))

;; Code for rewriting rules to the simplified form in the paper
(defun var-forms (rule)
  "Return a hash-table mapping variables to the hypotheses of RULE they appear in."
  (let ((var-forms (make-hash-table :test 'eq)))
    (loop for form in (rule-hypotheses rule)
       do (loop for var in (form-vars form)
             do (let ((forms (gethash var var-forms)))
                  ;; Only insert the current rule once (note
                  ;; trickiness when the same rule appears twice).
                  (unless (eq (car forms) form)
                    (push form (gethash var var-forms))))))
    var-forms))

(defun wildcard-vars! (var-forms head-vars)
  "Return the hash-table VAR-FORMS modified to contain only vars occurring in exactly one hypothesis and not in HEAD-VARS."
  (maphash (lambda (var forms)
             (if (and (null (cdr forms))
                      (not (member var head-vars :test #'eq)))
                 ;; Remove the outer list
                 (setf (gethash var var-forms) (car forms))
                 (remhash var var-forms)))
           var-forms)
  var-forms)

(defun make-rule-name (&optional (name "RULE"))
  (declare (special *aux-rules-package*))
  (let ((v (gensym name)))
    (intern (symbol-name v) (find-package *aux-rules-package*))))

;; TODO: gotta check the head vars in here somewhere.
(defun wildcard-rewrites (single-hypothesis-vars)
  ;; TODO: fix this docstring
  "Return a list of single-hypothesis rules that eliminate the wildcard vars from the rules in SINGLE-HYPOTHESIS-VARS (as produced by SINGLE-HYPOTHESIS-VARS!)."
  (let ((new-rules (make-hash-table :test 'eq
                                    :size (hash-table-size single-hypothesis-vars))))
    (maphash (lambda (var hyp)
               (let ((new-rule (gethash hyp new-rules
                                        (cons (make-rule-name "WILDCARD-RULE")
                                              (form-vars hyp)))))
                 ;; Remove the var from the new form
                 (setf (gethash hyp new-rules)
                       (cons (car new-rule)
                             (remove var (cdr new-rule) :test #'eq)))))
             single-hypothesis-vars)
    new-rules))

(defun rewrite-wildcard-rule (rule)
  (let* ((replacements (wildcard-rewrites (wildcard-vars! (var-forms rule)
                                                         (form-vars (rule-conclusion rule)))))
         (rewritten-rule (apply #'make-rule
                                (rule-conclusion rule)
                                (mapcar (lambda (hypothesis)
                                          (gethash hypothesis replacements hypothesis))
                                        (rule-hypotheses rule))))
         (new-rules (loop for hyp being the hash-keys of replacements
                          for new-head being the hash-values of replacements
                          collect (make-rule new-head hyp))))
    (cons rewritten-rule new-rules)))

(defun rewrite-wildcard-rules (rules)
  "Return a new set of rules such that wildcard vars only occur in rules with exactly one hypothesis."
  (loop for r in rules
     nconc (rewrite-wildcard-rule r)))

;; Code for rewriting rules to at most two hypotheses
;; NOTE: Assumes no wildcard vars in large hypothesis blocks, use
;; after REWRITE-WILDCARD-RULES

(defun body-var-list (hypotheses)
  "Return a list of (unique) variables used in HYPOTHESES, assuming no wildcard vars."
  (let ((var-set (map-set:make-map-set)))
    (loop for hyp in hypotheses
       do (loop for var in (form-vars hyp)
             do (map-set:ms-insert var-set var)))
    (map-set:ms-map 'list #'identity var-set)))

(defun rewrite-to-duples (rule)
  (check-type rule datalog-rule)
  (labels ((rewrite-hyps (hyps)
            (if (or (null hyps)
                    (null (cdr hyps))
                    (null (cddr hyps)))
                ;; <= 2 hypotheses
                (values hyps '())
                ;; Otherwise, rewrite the rest of them and go from
                ;; there
                (multiple-value-bind (new-hyps new-rules) (rewrite-hyps (cdr hyps))
                  (let* ((tail-vars (body-var-list new-hyps))
                         (tail-rule-name (make-rule-name "DUPLE-RULE"))
                         (new-head (cons tail-rule-name tail-vars))
                         (tail-rule (apply #'make-rule new-head new-hyps)))
                    (values (list (car hyps) new-head)
                            (cons tail-rule new-rules)))))))
    (multiple-value-bind (new-hyps new-rules)
        (rewrite-hyps (rule-hypotheses rule))
      (cons (apply #'make-rule (rule-conclusion rule) new-hyps)
            new-rules))))

(defun rewrite-long-rules (rules)
  (loop for rule in rules nconc (rewrite-to-duples rule)))

(defun normalize-rules (rules)
  (rewrite-long-rules (rewrite-wildcard-rules rules)))

;; Code for doing bottom-up evaluation

;; This is faster for forms with lots of vars, but it conses more and
;; even at 80 variables per form, the speed difference isn't
;; measurable until you run it 1000 times.
;; (defun shared-vars (form1 form2)
;;   (let ((form1-set (map-set:make-map-set))
;;         (shared-vars '()))
;;     (loop for var in (form-vars form1)
;;        do (map-set:ms-insert form1-set var))
;;     (loop for var in (form-vars form2)
;;        if (map-set:ms-member-p form1-set var)
;;        do (push var shared-vars))
;;     shared-vars))

;; FIXME: use of set functions can alter the order of arguments (this
;; is actually ok in practice, because the order is altered consistently).
(defun shared-vars (form1 form2)
  (intersection (remove-duplicates (form-vars form1) :test #'eq)
                (remove-duplicates (form-vars form2) :test #'eq)))

(defun unshared-vars (form1 form2)
  "Return the list of variables that FORM1 does not share with FORM2."
  (set-difference (remove-duplicates (form-vars form1) :test #'eq)
                  (remove-duplicates (form-vars form2) :test #'eq)))

(defun wildcard-vars (conclusion hypothesis)
  (set-difference (remove-duplicates (form-vars hypothesis))
                  (shared-vars conclusion hypothesis)))

(defun unifying-match-form (form &optional head)
  "Return an OPTIMA pattern that will match FORM with the appropriate unification. If supplied, HEAD will be used to determine wildcard vars."
  (let* ((wildcard-vars (if head (wildcard-vars head form) nil))
         (vars '()))
    (cons (car form)
          (loop for p in (cdr form)
                if (varp p)
                collect (cond
                         ((member p wildcard-vars) '_)
                         ((member p vars)
                          (let ((new-var (gensym "?VAR")))
                            ;; EQUAL is probably not sufficient here
                            `(guard ,new-var (equal ,new-var ,p))))
                         (t (push p vars)
                            p))
                else
             collect p))))

(defun satisfiesp (fact query)
  (let ((vars '()))
    (when (not (= (length fact) (length query)))
      (return-from satisfiesp nil))
    (loop for a1 in fact
       for a2 in query
       do (cond
            ((varp a2) (let ((prev (assoc a2 vars)))
                         (cond
                           ;; TODO: element check, will need
                           ;; modification for custom equality checks.
                           ((and prev (not (equalp (cdr prev) a1)))
                            (return nil))
                           ((null prev) (push (cons a2 a1) vars)))))
            ;; TODO: another element check
            (t (when (not (equalp a1 a2))
                 (return nil))))
       finally (return t))))

(defun process-fact-form (rule)
  (let* ((conclusion (rule-conclusion rule))
         (hypotheses (rule-hypotheses rule))
         (first-rule (first hypotheses))
         (second-rule (second hypotheses))
         (shared-vars (shared-vars first-rule second-rule))
         (x2s-var (gensym "X2S-"))
         (x1s-var (gensym "X1S-"))
         (rule1-map-var (gensym "P1YSX1S-"))
         (rule2-map-var (gensym "P2YSX2S-"))
         (fact-var (gensym "FACT")))
    `(lambda (,fact-var ,rule1-map-var ,rule2-map-var)
       ,@(cond
         ;; Just one hypothesis
         ((endp (cdr hypotheses))
          `((declare (ignore ,rule1-map-var ,rule2-map-var))
            (optima:match ,fact-var
              ((list ,@(unifying-match-form (first hypotheses) conclusion))
               (values (list (list ,@conclusion)) nil nil)))))
         ;; Two hypotheses (see rule normalization)
         (t `((optima:match ,fact-var
                ((list ,@(unifying-match-form first-rule)) ;; we may need
                 ;; wildcards for unification
                 ,(let ((p2-unshared-vars (unshared-vars second-rule first-rule))
                        (conclusion-form `(list ,@conclusion)))
                       `(values (map-fact-set 'list
                                              (lambda (,x2s-var )
                                                ,@(if (endp p2-unshared-vars)
                                                      (list `(declare (ignore ,x2s-var))
                                                            conclusion-form)
                                                      `((optima:match ,x2s-var
                                                          ((list ,@p2-unshared-vars)
                                                           ,conclusion-form)))))
                                              (get-binding-set ,rule2-map-var (list ,@shared-vars)))
                                (cons (list ,@shared-vars) (list ,@(unshared-vars first-rule second-rule)))
                                nil)))
                ;; Second rule match
                ((list ,@(unifying-match-form second-rule)) ;; may need
                 ;; wildcards for unification.
                 ,(let ((p1-unshared-vars (unshared-vars first-rule second-rule))
                        (conclusion-form `(list ,@conclusion)))
                       `(values (map-fact-set 'list
                                              (lambda (,x1s-var)
                                                ,@(if (endp p1-unshared-vars)
                                                      (list `(declare (ignore ,x1s-var))
                                                            conclusion-form)
                                                      `((optima:match ,x1s-var
                                                          ((list ,@p1-unshared-vars)
                                                           ,conclusion-form)))))
                                              (get-binding-set ,rule1-map-var (list ,@shared-vars)))
                                nil
                                (cons (list ,@shared-vars) (list ,@(unshared-vars second-rule first-rule)))))))))))))

(defun rule-func (rule)
  (compile nil (process-fact-form rule)))

(defun make-binding-map ()
  (make-hash-table :test 'equalp))

(defun make-fact-set ()
  (make-hash-table :test 'equalp))

(defun empty-fact-set-p (set)
  (= (hash-table-count set) 0))

(defun insert-entry! (set val)
  (setf (gethash val set) t))

(defun memberp (set val)
  (nth-value 1 (gethash val set)))

(defun pop-entry! (set)
  (with-hash-table-iterator (entry set)
    (multiple-value-bind (entryp key val) (entry)
      (declare (ignore val))
      (if entryp
          (progn
            (remhash key set)
            (values key t))
        (values nil nil)))))

(defun fact-set (&rest facts)
  (let ((set (make-fact-set)))
    (dolist (f facts)
      (insert-entry! set f))
    set))

(defun get-binding-set (map vals)
  (multiple-value-bind (set foundp) (gethash vals map)
    (if foundp
        set
        (make-fact-set))))

(defun map-fact-set (result-type thunk set)
  (map result-type thunk
       (alexandria:hash-table-keys set)))

(defun make-binding-map ()
  (make-hash-table :test 'equalp))

(defun insert-rule-binding! (binding-map ys xs)
  (multiple-value-bind (vals foundp) (gethash ys binding-map)
    ;; VALS is a set (hash-table).
    (cond
      ((not foundp) (let ((m (make-hash-table :test 'equalp)))
                      (setf (gethash xs m) t)
                      (setf (gethash ys binding-map) m)))
      (t (setf (gethash xs vals) t))))
  (values))

;; This is really only useful for debugging purposes.
(defun process-facts! (rule-func facts)
  (let ((p1 (make-hash-table :test 'equalp))
        (p2 (make-hash-table :test 'equalp))
        (results '()))
    ;; TODO: Could save some space by popping from facts instead of just iterating.
    (loop while (not (empty-fact-set-p facts))
       do (let ((fact (pop-entry! facts)))
            (multiple-value-bind (new-facts p1-binding p2-binding)
                (funcall rule-func fact p1 p2)
              (when p1-binding
                (insert-rule-binding! p1 (car p1-binding) (cdr p1-binding)))
              (when p2-binding
                (insert-rule-binding! p2 (car p2-binding) (cdr p2-binding)))
              (dolist (f new-facts)
                (insert-entry! facts f)))
            (push fact results)))
    results))

(defun processor (rules)
  (let ((rule-funcs (mapcar #'rule-func rules)))
    (lambda (facts)
      (let ((rule-maps (mapcar (lambda (rule)
                                 (if (endp (cdr (rule-hypotheses rule)))
                                     ;; Single-hypothesis rule
                                     '(nil . nil)
                                     (cons (make-binding-map)
                                           (make-binding-map))))
                               rules))
            (results '()))
        (loop while (not (empty-fact-set-p facts))
           do (let ((fact (pop-entry! facts)))
                (loop for thunk in rule-funcs
                   for (p1-map . p2-map) in rule-maps
                   do (multiple-value-bind (new-facts p1-binding p2-binding)
                          (funcall thunk fact p1-map p2-map)
                        (when p1-binding
                          (insert-rule-binding! p1-map (car p1-binding) (cdr p1-binding)))
                        (when p2-binding
                          (insert-rule-binding! p2-map (car p2-binding) (cdr p2-binding)))
                        (dolist (f new-facts)
                          (insert-entry! facts f))))
                (push fact results)))
        results))))

;;;; Stuff for subsumptive demand transformation.

;;; Rule annotation
(defun exemplar-fact (query)
  "Return a fact that satisfies QUERY, and which will only satisfy
another query q2 if q2 is a superset of QUERY."
  (let ((map '()))
    (cons (car query)
          ;; We need to use unique numbers for the vars, so start past
          ;; the biggest number we've got.
          (loop with i = (1+ (loop for i in query
                                when (numberp i)
                                maximizing (ceiling i)))
             for a in (cdr query)
             collect (cond
                       ((not (varp a)) a)
                       (t (let ((prev (assoc a map)))
                            (if prev
                                (cdr prev)
                                (prog2
                                    (push (cons a i) map)
                                    i
                                  (incf i))))))))))

(defun variantp (q1 q2)
  "Returns T is Q1 is a variant of Q2, that is if all facts that satisfy Q1 also satisfy Q2."
  (satisfiesp (exemplar-fact q1) q2))

(defun defining-rules (query rules)
  "Return the subset of RULES that define QUERY."
  ;; Note: VARIANTP is too strict here.
  ;; TODO: We should rule out definite mismatches based on
  ;; e.g. constants, too.
  (remove-if-not (lambda (r)
                   (let ((conclusion (rule-conclusion r)))
                     ;; TODO: element check, may need to be customized.
                     (and (equalp (first conclusion) (first query))
                          (= (length conclusion) (length query)))))
                 rules))

;; subsumptive-demand patterns used for rule annotation
(defstruct pattern
  (query)
  (position)
  (rule)
  (guaranteedp)
  (bind-pattern))

(defun subsumesp (p1 p2)
  "Returns T if the s-demand pattern P1 subsumes the s-demand-pattern P2, NIL otherwise."
  (check-type p1 pattern)
  (check-type p2 pattern)
  (flet ((mismatchp (bp1 bp2)
           ;; Find an element that is bound in p1 and not bound in p2.
           (loop for b1 in bp1
              for b2 in bp2
              when (and (eq b1 :bound)
                        (not (eq b2 :bound)))
              return nil
              finally (return t))))
    (let ((p1-hypotheses (rule-hypotheses (pattern-rule p1)))
          (p2-hypotheses (rule-hypotheses (pattern-rule p2))))
      (and (pattern-guaranteedp p1)
           (not (mismatchp (pattern-bind-pattern p1)
                           (pattern-bind-pattern p2)))
           ;; Make sure the hypotheses to the left of the n1th
           ;; hypothesis in p1 is a subset of the hypotheses to the
           ;; left of the n2th hypothesis in p2.
           (subsetp (subseq p1-hypotheses 0 (1+ (pattern-position p1)))
                    (subseq p2-hypotheses 0 (1+ (pattern-position p2)))
                    ;; TODO: Needs to be changed for custom equality checks.
                    :test #'equalp)))))

(defun arg-binding (arg left-hypotheses conclusion conclusion-bind-pattern)
  (let ((boundp (or (not (varp arg)) ; constants are bound
                    ;; ARG is bound if it appears in a hypothesis to its left...
                    (position-if (lambda (h)
                                   (position arg (cdr h)))
                                 left-hypotheses)
                    ;; ... or if it is bound in the conclusion ARG.
                    (loop for item in (cdr conclusion)
                       for bound in conclusion-bind-pattern
                       when (and (eq item arg)
                                 (eq bound :bound))
                       return t))))
    (if boundp
        :bound
        :free)))

(defun bind-pattern (query rule rule-binding-pattern)
  "Return the binding pattern (not the demand-pattern) for the hypothesis QUERY in RULE, where RULE-BINDING-PATTERN is the binding pattern for RULE."
  (let* ((hypotheses (rule-hypotheses rule))
         (tail (member query hypotheses))
         (left-hypotheses (if (endp tail) nil (ldiff hypotheses tail)))
         (conclusion (rule-conclusion rule)))
    (mapcar (lambda (a)
              (arg-binding a left-hypotheses conclusion rule-binding-pattern))
            (cdr query))))

(defun sub-patterns (pattern computed-patterns rules)
  (check-type pattern pattern)
  (dolist (rule (defining-rules (pattern-query pattern) rules))
    (loop for hypothesis in (rule-hypotheses rule)
          for i from 0
          ;; We only care about IDB predicates.
          when (defining-rules hypothesis rules)
          ;; TODO: the way we determine new-guaranteed determines the
          ;; type of scheduling this simulates. Use a policy to
          ;; determine this.
          do (let* ((new-pattern (make-pattern :query hypothesis
                                               :position i
                                               :rule rule
                                               :guaranteedp (and (pattern-guaranteedp pattern)
                                                                 (= i 0))
                                               :bind-pattern (bind-pattern hypothesis
                                                                           rule
                                                                           (pattern-bind-pattern pattern)))))
               ;; TODO: this seems correct (if the current pattern
               ;; subsumes other ones, remove them), but it's not in
               ;; the original paper (actually it might be: this
               ;; sounds like it accomplishes the same thing as the
               ;; Subsumptive-Optimization transformation).
               (setf computed-patterns
                     ;; REMOVE-IF does not necessarily return the
                     ;; original list, which affects the termination
                     ;; in DEMAND-PATTERNS.
                     (if (find-if (lambda (p) (subsumesp new-pattern p))
                                  computed-patterns)
                         (remove-if (lambda (p)
                                      (subsumesp new-pattern p))
                                    computed-patterns)
                         computed-patterns))

               ;; Add it, unless it's subsumed by an existing query
               (pushnew new-pattern
                        computed-patterns
                        :test (lambda (new old)
                                (or (equalp new old)
                                    (subsumesp old new)))))))
  computed-patterns)

(defun demand-patterns (query rules)
  (flet ((new-patterns (patterns)
           (loop for p in patterns
              do (setf patterns (sub-patterns p patterns rules)))
           patterns))
    (let* ((bind-pattern (mapcar (lambda (a)
                                   (if (varp a)
                                       :free
                                       :bound))
                                 (cdr query)))
           (initial-pattern (make-pattern :query query
                                          :position 0
                                          :rule (make-rule nil)
                                          :guaranteedp t
                                          :bind-pattern bind-pattern)))
      ;; Compute new sub-patterns until we don't get any new ones.
      ;; FIXME: that seems like an awfully expensive termination test.
      (loop with patterns = (list initial-pattern)
         for new-patterns = (new-patterns patterns)
         for i below 2 ; FIXME: for debugging purposes
         until (eq new-patterns patterns)
         do (setf patterns new-patterns)
         finally (return patterns)))))

(defun subsuming-bind-patterns (pattern)
  "Return a list of bind-patterns that properly subsume PATTERN."
  (let ((results '())
        ;; Number of properly subsuming patterns is the number of
        ;; permutations that can be made by flipping one or more
        ;; :BOUND elements to :FREE, which is 1- the number of values
        ;; that can be represented by an n-bit number, where n is the
        ;; number of :BOUND elements in PATTERN (note that the "0"
        ;; value is just PATTERN, which does not properly subsume
        ;; itself).
        (count (1- (expt 2 (count :bound pattern))))
        (state (copy-list pattern)))
    (dotimes (i count)
      ;; Note: this is the same algorithm as binary counting, but
      ;; ignoring "digits" that are :FREE in PATTERN.
      (loop for c on state
         for e in pattern
         do (cond
              ;; Flip the first :BOUND we find that's :BOUND in the
              ;; original and save the result.
              ((and (eq e :bound)
                    (eq (car c) :bound))
               (setf (car c) :free)
               (push (copy-list state) results)
               (return))
              ;; Flip any :FREE arguments we find along the way, if
              ;; they're eligible for flipping.
              ((and (eq e :bound)
                    (eq (car c) :free))
               (setf (car c) :bound)))))
    results))

(defun bind-string (bind-pattern)
  "Return a string representing the bind-pattern BIND-PATTERN."
  (map 'string (lambda (b)
                 (ecase b
                   (:bound #\B)
                   (:free #\F)))
       bind-pattern))

(defun demand-predicate-symbol (predicate bind-pattern)
  (let ((bind-string (bind-string bind-pattern)))
    (intern (format nil "DEMAND-~A-~A" predicate bind-string)
            *aux-rules-package*)))

(defun demand-pattern-predicate-symbol (demand-pattern)
  (check-type demand-pattern pattern)
  (let ((predicate (first (pattern-query demand-pattern))))
    (demand-predicate-symbol predicate (pattern-bind-pattern demand-pattern))))

(defun pattern-subsumption-rules (demand-pattern rules)
  (flet ((bound-arguments (conclusion bind-pattern)
           (loop for c in (rest conclusion)
              for b in bind-pattern
              when (eq b :bound)
              collect c)))
    (let* ((query (pattern-query demand-pattern))
           (bind-pattern (pattern-bind-pattern demand-pattern))
           (rules (defining-rules query rules))
           ;; Toplevel predicates.
           (new-rules (mapcar (lambda (r)
                                (let ((demand-hypothesis (cons (demand-pattern-predicate-symbol demand-pattern)
                                                               (bound-arguments (rule-conclusion r)
                                                                                bind-pattern))))
                                  (cons (rule-conclusion r)
                                        (cons demand-hypothesis
                                              (rule-hypotheses r)))))
                              rules)))
      ;; New demand predicates.
      (values new-rules
              (loop for rule in new-rules
                 for hypotheses = (rule-hypotheses rule)
                 appending (loop for h-cell on (rest hypotheses) ; ignore the demand hypothesis
                              for h = (car h-cell)
                              ;; We only care about hypotheses for IDB predicates here.
                              when (defining-rules h new-rules)
                              collect (let* ((initial-hypotheses (ldiff hypotheses h-cell))
                                             ;; This hypothesis' bind pattern.
                                             (h-bind-pattern (bind-pattern h rule bind-pattern))
                                             (h-bound-args (bound-arguments h h-bind-pattern))
                                             (h-predicate (first h))
                                             (conclusion (cons (demand-predicate-symbol h-predicate h-bind-pattern)
                                                               h-bound-args))
                                             (subsuming-hypotheses (mapcar (lambda (p)
                                                                             (cons (demand-predicate-symbol h-predicate
                                                                                                            p)
                                                                                   h-bound-args))
                                                                           (subsuming-bind-patterns h-bind-pattern))))
                                        (make-rule conclusion
                                                   (append initial-hypotheses
                                                           (mapcar (lambda (h)
                                                                     (list :not h))
                                                                   subsuming-hypotheses))))))))))


(defun subsumption-rules (demand-patterns)
  (dolist (p demand-patterns)

    )
  )

;;; Test Data
;; rel(?x, ?y) :- imm(?x, ?y).
;; rel(?x, ?y) :- imm(?u, ?v), rel(?u, ?x), rel(?v, ?y).

;; '(((:rel ?x ?y) (:imm ?x ?y))
;;   ((:rel ?x ?y) (:imm ?u ?v)
;;                 (:rel ?u ?x)
;;                 (:rel ?v ?y)))

;; '(:rel c ?y)


  ;; (defun annotated-rules (query rules)
  ;;   (let ((patterns (demand-patterns query rules))
  ;;         (predicate-map (make-hash-table :test 'equalp)))
  ;;     ;; Assign an annotated predicate for each pattern
  ;;     (dolist (p patterns)
  ;;       (let ((key (cons (pattern-query p) (pattern-bind-pattern p))))
  ;;         (unless (gethash key predicate-map)
  ;;           (setf (gethash key predicate-map) (gensym ""))
  ;;           )
  ;;         )
  ;;       (setf (gethash (cons (pattern-query p) (pattern-bind-pattern p)) predicate-map))
  ;;       )
  ;;     (dolist (p patterns)

  ;;       )
  ;;     )
  ;;   )
