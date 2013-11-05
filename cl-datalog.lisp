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
