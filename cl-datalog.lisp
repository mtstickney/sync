;;;; cl-datalog.lisp

(in-package #:cl-datalog)

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

(defun rule-conslusion (rule)
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

(defun predicatep (rule)
  "Return t if RULE is a predicate, i.e. a valid datalog rule that is neither a fact nor a query."
  (check-type rule datalog-rule)
  (not (or (factp rule)
           (queryp rule))))
