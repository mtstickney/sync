;;;; install-gen.lisp

(in-package #:install-gen)

;;; "install-gen" goes here. Hacks and glory await!

(defvar *defs* (make-hash-table :test 'equal))
(defvar *effects* (make-hash-table))

(define-condition missing-file ()
  ((path :initarg :path
         :reader missing-file-path)
   (type :initarg :type
         :reader missing-file-type)
   (use :initarg :use
        :initform nil
        :reader missing-file-use))
  (:documentation "Condition signalled when a required file is missing.")
  (:report (lambda (condition stream)
             (format stream "Required ~A ~S is missing"
                     (missing-file-type condition)
                     (missing-file-path condition))
             (if (missing-file-use condition)
                 (format stream ", ~A." (missing-file-use condition))
                 (format stream "."))
             (values))))

(define-condition abl-error ()
  ((errors :initarg :errors
           :reader abl-error-errors))
  (:documentation "Condition signalled to report errors from a callout to ABL.")
  (:report (lambda (condition stream)
             (format stream "~@<ABL returned with errors:~@:_~I~{~<~A: ~:_~I~W~@:_~:>~}~:>"
                     (abl-error-errors condition)))))

(defun require-file (path type &optional use)
  (check-type path (or string pathname))
  (unless (probe-file path)
    (error 'missing-file-error
           :path path
           :type type
           :use use)))

(defun path-getter (path type)
  (check-type path (or string pathname))
  (assert (cl-fad:pathname-relative-p path) (path)
          "Path ~S is not relative" path)
  (macrolet ((fetcher (base-symb)
                      `(lambda ()
                         (declare (special ,base-symb))
                         (check-type ,base-symb (or string pathname))
                         (merge-pathnames path ,base-symb))))
             ;; TODO: PROBE-FILE :src and :aux (but then we can't
             ;; build the code without having all the resources...)
             (ecase type
               (:src (fetcher *src-dir*))
               (:dest (fetcher *dest-dir*))
               (:aux (fetcher *res-dir*))
               (:gen (fetcher *res-dir*)))))

;; TODO: Maybe want a higher-level "ignore-missing" restart for
;; multiple files
(defun remove-file (path)
  (declare (special *ignore-missing-deletes*))
  (check-type path (or string pathname))
  (restart-case (delete-file path)
    (skip-file ()
      :report "Ignore the error and skip the file."
      (return-from remove-file (values))))
  (values))

(defun tree-diff-applicator (cmd path)
  (check-type cmd keyword)
  (check-type path (or pathname string))
  (let ((src-pather (path-getter path :src))
        (dest-pather (path-getter path :dest)))
    (ecase cmd
      (:+ (lambda () (cl-fad:copy-file (funcall src-pather)
                                       (funcall dest-pather)
                                       :overwrite t)))
      (:- (lambda () (remove-file (funcall dest-pather)))))))

(defun tree-application (tree-spec)
  (check-type tree-spec list)
  (let ((applicators (mapcar (lambda (file-spec)
                               (apply #'tree-diff-applicator file-spec))
                             tree-spec)))
    ;; TODO: Add a top-level tree application restart
    (lambda ()
      (loop for app in applicators
         do (funcall app)))))

(defun st-file-applicator (file db-name)
  (check-type file (or pathname string))
  (check-type file (or pathname string))
  (let ((st-pather (path-getter file :aux)))
    (lambda ()
      (declare (special *progress-dir* *dest-dir*))
      (let* ((prostrct-path (merge-pathnames "bin/_dbutil.exe" *progress-dir*))
             (st-path (funcall st-pather))
             (db-path (merge-pathnames (cl-fad:pathname-as-file db-name)
                                       *dest-dir*))
             (db-file-path (merge-pathnames db-path (make-pathname :type "db"))))
        (require-file prostrct-path "Progress binary" "can't run PROSTRCT")
        (require-file st-path "structure file")
        (require-file db-file-path "database file")
        ;; Just have to assume this succeeds, because Progress doesn't
        ;; understand return codes.
        (external-program:run prostrct-path (list "prostrct" "add"
                                                  db-path
                                                  st-path))))))

(defun df-file-applicator (file db-file)
  (check-type file (or pathname string))
  (check-type db-file (or pathname string))
  (let ((pather (path-getter file :aux))
        (load-pather (path-getter "sys/load_df.r" :aux))
        (db-pather (path-getter db-file :dest)))
    (lambda ()
      (declare (special *dest-dir*))
      (let ((df-path (funcall pather))
            (db-file (funcall db-pather))
            (load-proc (funcall load-pather)))
        (require-file df-path "definition file")
        (require-file load-proc "system procedure")
        (require-file db-file "database file")
        (run-abl load-proc db-file df-path)))))

;; TODO: Should the sys/ dir have its own :sys type?
(defun data-file-loader (file db-file)
  (check-type file (or pathname string))
  (check-type db-file (or pathname string))
  (let ((pather (path-getter file :aux))
        (load-pather (path-getter "sys/load_d.r" :aux))
        (db-pather (path-getter db-file :dest)))
    (lambda ()
      (declare (special *dest-dir*))
      (let ((d-path (funcall pather))
            (db-file (funcall db-pather))
            (load-proc (funcall load-pather)))
        (require-file load-proc "system procedure")
        (require-file db-file "database file")
        (require-file d-path "data file")
        (run-abl load-proc db-file d-path)))))

;; TODO: make sure to handle EOF errors
(defun run-abl (proc system-args &rest args)
  (declare (special *progress-dir*))
  (check-type *progress-dir* (or string pathname))
  (labels ((prowin-args (proc system-args args)
             (concatenate 'list
                          (list "-p" proc
                                "-param" (format nil "~{~A~^,~}" args))
                          system-args)))
    (let* ((prowin-path (let ((path (merge-pathnames "bin/prowin32.exe"
                                                     *progress-dir*)))
                          (require-file path "Progress binary" "unable to run ABL procedure")
                          path))
           (result-str (with-output-to-string (fh)
                         (external-program:run
                          prowin-path
                          (prowin-args proc system-args args)))))
      ;; TODO: Use CL-SECURE-READ for this
      (let ((result (read-from-string result-str)))
        ;; TODO: c'mon now, can't recover from this
        (check-type result list)
        ;; TODO: Neither is this.
        (ecase (car result)
          (:data (apply #'values (cdr result)))
          (:error (error 'abl-error :errors (cdr result))))))))

(defmacro defeffect (name args &body body)
  (check-type args list)
  `(setf (gethash ,name *effects*)
         (lambda ,args ,@body)))

(defeffect :db-structure (&rest st-files)
  (let ((applicators (map nil #'st-file-applicator st-files)))
    (lambda ()
      (map nil #'funcall applicators))))

(defeffect :db-defs (&rest df-files)
  (let ((applicators (map nil #'df-file-applicator df-files)))
    (lambda ()
      (map nil #'funcall applicators))))

(defeffect :db-binary-data (&rest d-files)
  (let ((applicators (map nil #'data-file-loader d-files)))
    (lambda ()
      (map nil #'funcall applicators))))

;; TODO: Later
;; (defeffect :set-reg-key (key val)
;;   (etypecase key
;;     (string )
;;     )
;;   )

;; (defeffect :backup-if-space ()
;; )

(definstaller cmax-5
    (:version "5.0.1.1")
  (:product "CompassMax")
  (:src-dir "res/")
  ;(:check-sources t) ; defaults to t, used to check resource paths
  ;when building
  (:tree (:+ "client/include/blargl.r")
         (:- "client/procedure/NotAThing.r"))
  (:pre-effects :backup-if-space)
  (:post-effects (:db-structure "dbase/addarea.st")
                 (:db-defs "df/mailingevent.df"
                           "df/area2.df"
                           "df/thingy.df")
                 (:db-binary-data "defaults.d")
                 (:db-text-data "table-data.txt") ; What's the
                                        ; extension again?
                 ;; Maybe don't want this
                 (:set-reg-key "HCOM\\Foo\\MS\\Blurb\\MTG\\Compass\\Version" "5.0.1.1")
                 ;; Tree copy with a special destination. Hmmm...
                 (:add-or-update-shortcut (merge-pathnames "CompassMax 4.3.2.6.lnk" (desktop-dir)))))
