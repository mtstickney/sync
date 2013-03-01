;;;; db-pkg.lisp

(in-package #:db-pkg)

;; Abstract representation of an r-code file
(defstruct file
  (tables :initarg :tables :reader tables))

;; TODO: maybe the key here is to treat provided files and db entries
;; in the same way. (note: can't quite get away with this, since file
;; conflicts partly depend on db entries. May still have shared stuff, tho).

;; TODO: PROVIDES is broken for two pkgs where each adds a new field
;; to the same table. May need supersedes for db-envs.
(defstruct pkg
  (build-env :initarg :build-env :accessor build-env)
  (files :initarg :files :accessor files)
  (provides :initarg :provides :accessor provides)
  (supersede-list :initarg :supersedes :accessor supersede-lst))

;; Map build-env feature entries (e.g. :bug-1326) to a list of tables
;; that are part of that env
(defvar *env-registry* '())

;; Experimental: is this actually true? If not, do we have a problem
;; with envs including tables from the packages build env (instead of
;; just those that are PROVIDEd by the package)
(defun env-tables (env)
  (provides (find-package env)))

;; db deps are the tables linked into r-code files
(defun db-deps (file-lst)
  (loop for f in file-lst appending (tables f)))

(defun union* (&rest sets)
  (reduce #'union sets :initial-value '()))

;; TODO: performance issue, set ops will be slow on 120 tables...
(defun computed-db-env (pkg)
  "Return a list of the build environment components that are actually used by files in PKG."
  ;; This sort of filtering should happen at package creation time,
  ;; no?
  ;; Does this change based on supersedes? If so, how do we
  ;; re-calculate?
  ;; Sounds like maybe db-env needs to be calculated per-system, not per-package
  (let ((db-deps (remove-duplicates (db-deps (files pkg))
                                    :test #'equalp)))
    (remove-if-not (lambda (env)
                     (intersection (env-tables env) db-deps))
                   (build-env pkg))))

(defun system-pkg-list
    (sort *pkgs* #'> :key #'pkg-install-order))

(defun system-db-env ())

;;; "db-pkg" goes here. Hacks and glory await!

