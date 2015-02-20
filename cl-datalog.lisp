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
