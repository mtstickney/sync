(defpackage #:clomp.types
  (:use #:cl)
  (:export #:range-error
           #:+utf16-encoding+
           #:guid
           #:bool
           #:dword
           #:word
           #:hresult
           #:dispid
           ;; Enums
           #:error-code
           #:dispid-constant
           #:class-context
           #:register-flags
           #:disp-params
           #:args
           #:named-arg-ids
           #:arg-count
           #:named-arg-count
           #:excep-info)
  ;; Variant reference objects.
  (:export #:variant-ref
           #:make-variant-ref
           #:variant-ptr
           #:deref)
  ;; Automation types
  (:export #:automation-type
           #:bstr
           #:*default-currency-normalization-policy*
           #:normalize-currency-to-foreign
           #:currency
           #:variant-bool
           #:scode
           #:variant
           #:variant-type))

(in-package #:clomp.types)

;;; Useful type definitions.

;; FIXME: find a way to avoid copying these things all over the place.
(cffi:define-foreign-type guid ()
  ()
  (:actual-type :pointer)
  (:simple-parser guid))

(defmethod cffi:translate-into-foreign-memory (value (type guid) pointer)
  (assert (= (length value) 16) (value)
          "GUID arrays must be of length 16.")
  ;; Note: the GUID structure uses a DWORD and two SHORTs followed by
  ;; the remainder of the bytes. These first three fields ARE subject
  ;; to endianness, but the curly-brace string representation is
  ;; always big-endian.
  #+big-endian
  (loop for byte across value
     for i from 0
     do (setf (cffi:mem-aref pointer :uchar i)
              (aref value i)))
  #+little-endian
  (progn
    ;; Endian-reversed DWORD
    (loop for i from 0 to 3
       do (setf (cffi:mem-aref pointer :uchar
                               #+little-endian (- 3 i)
                               #+big-endian i)
                (aref value i)))
    ;; Two endian-reversed shorts
    (setf (cffi:mem-aref pointer :uchar 4) (aref value 5)
          (cffi:mem-aref pointer :uchar 5) (aref value 4)
          (cffi:mem-aref pointer :uchar 6) (aref value 7)
          (cffi:mem-aref pointer :uchar 7) (aref value 6))
    ;; Remaining 8 bytes are a non-endian array.
    (loop for i from 8 to 15
       do (setf (cffi:mem-aref pointer :uchar i)
                (aref value i))))
  (values))

(defmethod cffi:translate-to-foreign (value (type guid))
  (let ((obj (cffi:foreign-alloc :uchar :count 16)))
    (cffi:translate-into-foreign-memory value type obj)
    obj))

(defmethod cffi:translate-from-foreign (value (type guid))
  (let ((obj (make-array 16 :element-type '(unsigned-byte 8))))
    #+big-endian
    (dotimes (i 16)
      (setf (aref obj i)
            (cffi:mem-aref value :uchar i)))
    #+little-endian
    (progn
      ;; Endian-reversed DWORD
      (loop for i from 0 to 3
         do (setf (aref obj i) (cffi:mem-aref value :uchar (- 3 i))))
      ;; Two endian-reversed SHORTs
      (setf (aref obj 4) (cffi:mem-aref value :uchar 5)
            (aref obj 5) (cffi:mem-aref value :uchar 4)
            (aref obj 6) (cffi:mem-aref value :uchar 7)
            (aref obj 7) (cffi:mem-aref value :uchar 6))
      ;; Array of remaining 8 bytes (non-endian).
      (loop for i from 8 to 15
         do (setf (aref obj i) (cffi:mem-aref value :uchar i))))
    obj))

(defmethod cffi:free-translated-object (value (type guid) param)
  (declare (ignore param))
  (cffi:foreign-free value))

(cffi:define-foreign-type bool ()
  ()
  (:actual-type :int)
  (:simple-parser bool))

(defmethod cffi:translate-to-foreign (value (type bool))
  (check-type value (or integer boolean))
  (etypecase value
    (integer value)
    (boolean (if value 1 0))))

(defmethod cffi:translate-from-foreign (value (type bool))
  (not (zerop value)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod cffi:expand-to-foreign (value (type bool))
    `(if ,value 1 0))

  (defmethod cffi:expand-from-foreign (value (type bool))
    `(not (zerop ,value))))

(cffi:defctype hresult :ulong)
(cffi:defctype dword :ulong)
(cffi:defctype word :ushort)
(cffi:defctype dispid :long)
;; TODO: add accessors for the fields in https://msdn.microsoft.com/en-us/library/office/cc815308.aspx
(cffi:defctype scode hresult)

(cffi:defcenum (error-code hresult)
  (:no-error 0)
  (:error-abort #x80004004)
  (:error-access-denied #x80070005)
  (:error-failure #x80004005)
  (:error-invalid-handle #x80070006)
  (:error-invalid-arg #x80070057)
  (:error-no-interface #x80004002)
  (:error-not-implemented #x80004001)
  (:error-out-of-mem #x8007000E)
  (:error-invalid-pointer #x80004003)
  (:error-unexpected-failure #x8000FFFF)
  (:error-aggregation-not-supported #x80040110))

(cffi:defcenum (dispid-constant dispid)
  (:dispid-unknown-interface #x80020001)
  (:dispid-member-not-found #x80020003)
  (:dispid-param-not-found #x80020004)
  (:dispid-type-mismatch #x80020005)
  (:dispid-unknown-name #x80020006)
  (:dispid-no-named-args #x80020007)
  (:dispid-bad-var-type #x80020008)
  (:dispid-exception #x80020009)
  (:dispid-overflow #x8002000A)
  (:dispid-bad-index #x8002000B)
  (:dispid-unknown-locale #x8002000C)
  (:dispid-array-is-locked #x8002000D)
  (:dispid-bad-param-count #x8002000E)
  (:dispid-param-not-optional #x8002000F)
  (:dispid-bad-callee #x80020010)
  (:dispid-not-a-collection #x80020011)
  (:dispid-division-by-zero #x80020012)
  (:dispid-buffer-too-small #x80020013)
  (:dispid-value 0)
  (:dispid-unknown -1)
  (:dispid-property-put -3)
  (:dispid-newenum -4)
  (:dispid-evaluate -5)
  (:dispid-constructor -6)
  (:dispid-destructor -7)
  (:dispid-collect -8))

(cffi:defbitfield (class-context dword)
  (:inproc-server #x01)
  (:inproc-handler #x02)
  (:local-server #x04)
  (:inproc-server16 #x08)
  (:remote-server #x10)
  (:inproc-handler16 #x20)
  (:reserved1 #x40)
  (:reserved2 #x80)
  (:reserved3 #x100)
  (:reserved4 #x200)
  (:no-code-download #x400)
  (:reserved5 #x800)
  (:no-custom-marshall #x1000)
  (:enable-code-download #x2000)
  (:no-failure-log #x4000)
  (:disable-aaa #x8000)
  (:enable-aaa #x10000)
  (:from-default-context #x20000)
  (:activate-32-bit-server #x40000)
  (:activate-64-bit-server #x80000)
  (:enable-cloaking #x100000)
  (:appcontainer #x400000)
  (:activate-aaa-as-iu #x800000)
  (:ps-dll #x80000000))

(cffi:defbitfield (register-flags dword)
  (:single-use #x00)
  (:multiple-use #x01)
  (:multi-use-separate #x02)
  (:suspended #x04)
  (:surrogate #x08))

(cffi:defcenum (invoke-type word)
  (:method #x01)
  (:property-get #x02)
  (:property-put #x04)
  (:property-put-ref #x08))

(define-condition range-error (error)
  ((value :initarg :val :accessor range-val)
   (lower-bound :initarg :lower :accessor range-lower)
   (upper-bound :initarg :upper :accessor range-upper))
  (:report (lambda (c s)
             (format s "Range error: The value ~S is outside of the range [~S, ~S]."
                     (range-val c)
                     (range-lower c)
                     (range-upper c)))))

(defclass automation-type () ())

(defparameter +utf16-encoding+
  #+little-endian :utf-16le
  #+big-endian :utf-16be)

(cffi:define-foreign-type bstr ()
  ()
  (:actual-type :pointer)
  (:simple-parser bstr))

(defmethod cffi:translate-to-foreign (value (type bstr))
  (check-type value string)
  (let* ((str-len (babel:string-size-in-octets value :encoding +utf16-encoding+))
         (nul-len (babel:string-size-in-octets #.(format nil "~C" #\Nul) :encoding +utf16-encoding+))
         (buf (cffi:foreign-alloc :uchar :count (+ str-len nul-len 4)))
         (str-ptr (cffi:inc-pointer buf 4)))
    (setf (cffi:mem-aref buf :uint32) str-len)
    (cffi:lisp-string-to-foreign value str-ptr (+ str-len nul-len) :encoding +utf16-encoding+)
    str-ptr))

(defmethod cffi:translate-from-foreign (value (type bstr))
  (check-type value cffi:foreign-pointer)
  (cffi:foreign-string-to-lisp value :encoding +utf16-encoding+))

(defmethod cffi:free-translated-object (value (type bstr) param)
  (declare (ignore param))
  (let ((ptr (cffi:make-pointer (- (cffi:pointer-address value) 4))))
    (cffi:foreign-free ptr)))

(defmethod cffi:expand-to-foreign-dyn (value var body (type bstr))
  (let ((value-var (gensym "VALUE"))
        (str-len-var (gensym "STR-LEN"))
        (nul-len-var (gensym "NUL-LEN"))
        (buf-var (gensym "BUF")))
    `(let* ((,value-var ,value)
            (,str-len-var (babel:string-size-in-octets ,value-var :encoding +utf16-encoding+))
            (,nul-len-var (babel:string-size-in-octets ,(format nil "~C" #\Nul) :encoding +utf16-encoding+)))
       (cffi:with-foreign-object (,buf-var :uchar (+ ,str-len-var ,nul-len-var 4))
         (let ((,var (cffi:inc-pointer ,buf-var 4)))
           (setf (cffi:mem-aref ,buf-var :uint32) ,str-len-var)
           (cffi:lisp-string-to-foreign ,value-var ,var (+ ,str-len-var ,nul-len-var) :encoding +utf16-encoding+)
           ,@body)))))

(defvar *default-currency-normalization-policy* :round
  "The default policy used to normalize values for conversion to the CURRENCY foreign type.")

(defgeneric normalize-currency-to-foreign (policy value)
  (:documentation "Normalize VALUE to to be representable as a CURRENCY value according to POLICY.

CURRENCY values are a fixed-point representation that can store
numbers in the range of 922337203685477.5807 to
-922337203685477.5808. VALUE should be converted to be within this
range, either as a rational or float. Note that since CURRENCY is a
64-bit integer internally, returning a float here may cause slight
rounding or truncation issues even after normalization."))

(defmethod normalize-currency-to-foreign ((policy (eql :round)) value)
  (check-type value real)
  (let ((parts (round value 1/10000)))
    (unless (<= -9223372036854775808 parts 9223372036854775807)
      (error 'range-error
             :val value
             :lower -922337203685477.5808
             :upper 922337203685477.5807))
    (/ parts 10000)))

(cffi:define-foreign-type currency ()
  ()
  (:actual-type :int64)
  (:simple-parser currency))

(defmethod cffi:translate-to-foreign (value (type currency))
  (check-type value real)
  (let ((normalized (normalize-currency-to-foreign *default-currency-normalization-policy*
                                                   value)))
    (nth-value 0 (truncate (* normalized 10000)))))

(defmethod cffi:translate-from-foreign (value (type currency))
  (/ value 10000))

(defmethod cffi:expand-to-foreign (value (type currency))
  `(truncate (* 10000 (normalize-currency-to-foreign *default-currency-normalization-policy*
                                                     ,value))))

(defmethod cffi:expand-from-foreign (value (type currency))
  `(/ ,value 10000))

;; VARIANT_BOOL uses a different truthy value than regular BOOL does.
(cffi:define-foreign-type variant-bool (bool)
  ()
  (:actual-type :short)
  (:simple-parser variant-bool))

(defmethod cffi:translate-to-foreign (value (type variant-bool))
  (if value #xFFFF 0))

(defmethod cffi:expand-to-foreign (value (type variant-bool))
  `(if value #xFFFF 0))

(cffi:defctype vartype :ushort)

(cffi:defcenum (variant-bare-type vartype)
  (:empty #x00)
  (:null #x01)
  (:int16 #x02)
  (:int32 #x03)
  (:float #x04)
  (:double-float #x05)
  (:currency #x06)
  (:bstr #x07)
  (:idispatch #x09)
  (:error #x0A) ; hresult
  (:bool #x0B)
  (:variant #x0C)
  (:iunknown #x0D)
  (:decimal #x0E)
  (:char #x10)
  (:uchar #x11)
  (:ushort #x12)
  (:uint32 #x13)
  (:int64 #x14)
  (:uint64 #x15)
  (:int #x16)
  (:uint #x17)
  (:void #x18)
  (:hresult #x19)
  (:pointer #x1A)
  (:safearray #x1B)
  (:c-array #x1C)
  (:user-defined #x1D)
  (:c-string #x1E)
  (:c-wide-string #x1F)
  (:record #x24) ; BRECORD, probably a struct
  (:pointer-int #x25) ; signed pointer -- wat.
  (:pointer-uint #x26))

(cffi:defbitfield (variant-type-flag vartype)
  (:array #x2000) ; a SAFEARRAY
  (:byref #x4000) ; a flag for pointer-to-* from the previous items
  )

(cffi:define-foreign-type variant-type ()
  ()
  (:actual-type vartype)
  (:simple-parser variant-type))

(defmethod cffi:translate-from-foreign (value (type variant-type))
  (let* ((flags (cffi:convert-from-foreign value 'variant-type-flag))
         ;; Remove the extracted flag bits (but avoid turning it into
         ;; a signed value).
         (type-val (ldb (byte (* 8 (cffi:foreign-type-size 'vartype)) 0)
                        ;; Note that we know all of FLAGS bits are set
                        ;; in VALUE, so here LOGXOR == LOGXOR(value,
                        ;; LOGAND(value, flags))
                        (logxor (cffi:convert-to-foreign flags 'variant-type-flag) value))))
    (cons (cffi:convert-from-foreign type-val 'variant-bare-type)
          flags)))

(defmethod cffi:translate-to-foreign (value (type variant-type))
  (logxor (cffi:convert-to-foreign (first value) 'variant-bare-type)
          (cffi:convert-to-foreign (rest value) 'variant-type-flag)))

(defmethod cffi:expand-from-foreign (value (type variant-type))
  (let ((value-var (gensym "VALUE"))
        (flags-var (gensym "VALUE"))
        (type-val-var (gensym "TYPE-VAL")))
    `(let* ((,value-var ,value)
            (,flags-var (cffi:convert-from-foreign ,value-var 'variant-type-flag))
            (,type-val-var (logxor (cffi:convert-to-foreign ,flags-var 'variant-type-flag)
                                    ,value-var)))
       (cons (cffi:convert-from-foreign ,type-val-var 'variant-bare-type)
             ,flags-var))))

(defmethod cffi:expand-to-foreign (value (type variant-type))
  (let ((value-var (gensym "VALUE")))
    `(let ((,value-var ,value))
       (logxor (cffi:convert-to-foreign (first ,value-var) 'variant-bare-type)
               (cffi:convert-to-foreign (rest ,value-var) 'variant-type-flag)))))

;; FIXME: this needs actual conversion code.
(cffi:defctype date :double)

;; FIXME: this needs actual conversion code.
(cffi:defcstruct record-struct
  (record :pointer)
  (record-info :pointer))

(cffi:defcunion variant-element
  (llval :long-long)
  (lval :long)
  (bval :uchar)
  (ival :short)
  (fltval :float)
  (dblval :double)
  ;; Why are there two of these?
  (boolval variant-bool)
  (bool variant-bool)
  (scode scode)
  (cyval currency)
  (date date)
  (bstrval bstr)
  (pointer :pointer)
  (hresultval hresult)
  (cval :char)
  (uival :ushort)
  (ulval :ulong)
  (ullval :unsigned-long-long)
  (intval :int)
  (uintval :uint)
  (record (:struct record-struct)))
#|
union {
        LONGLONG            llVal;
        LONG                lVal;
        BYTE                bVal;
        SHORT               iVal;
        FLOAT               fltVal;
        DOUBLE              dblVal;
        VARIANT_BOOL        boolVal;
        _VARIANT_BOOL       bool;
        SCODE               scode;
        CY                  cyVal;
        DATE                date;
        BSTR                bstrVal;
        IUnknown            *punkVal;
        IDispatch           *pdispVal;
        SAFEARRAY           *parray;
        BYTE                *pbVal;
        SHORT               *piVal;
        LONG                *plVal;
        LONGLONG            *pllVal;
        FLOAT               *pfltVal;
        DOUBLE              *pdblVal;
        VARIANT_BOOL        *pboolVal;
        _VARIANT_BOOL       *pbool;
        SCODE               *pscode;
        CY                  *pcyVal;
        DATE                *pdate;
        BSTR                *pbstrVal;
        IUnknown            **ppunkVal;
        IDispatch           **ppdispVal;
        SAFEARRAY           **pparray;
        VARIANT             *pvarVal;
        PVOID               byref;
        CHAR                cVal;
        USHORT              uiVal;
        ULONG               ulVal;
        ULONGLONG           ullVal;
        INT                 intVal;
        UINT                uintVal;
        DECIMAL             *pdecVal;
        CHAR                *pcVal;
        USHORT              *puiVal;
        ULONG               *pulVal;
        ULONGLONG           *pullVal;
        INT                 *pintVal;
        UINT                *puintVal;
        struct __tagBRECORD {
          PVOID       pvRecord;
          IRecordInfo *pRecInfo;
        } __VARIANT_NAME_4;
} __VARIANT_NAME_3;
|#

(cffi:defcstruct variant-struct
  (type variant-type)
  (reserved :ushort :count 3)
  (element (:union variant-element)))

;; TODO: finish this off
(cffi:define-foreign-type variant ()
  ((value-type :initarg :val-type :accessor variant-value-type))
  (:actual-type :struct variant-struct))

(cffi:define-parse-method variant (&optional type)
  (make-instance 'variant :val-type type))

(defun variant-inner-type (variant)
  (check-type variant cffi:foreign-pointer)
  (cffi:foreign-slot-value variant '(:struct variant-struct) 'type))

(defun variant-flags (variant)
  (check-type variant cffi:foreign-pointer)
  (rest (cffi:foreign-slot-value variant '(:struct variant-struct) 'type)))

(defun variant-ref-p (variant)
  (check-type variant cffi:foreign-pointer)
  (member :byref (variant-flags variant)))

(defun variant-array-p (variant)
  (check-type variant cffi:foreign-pointer)
  (member :array (variant-flags variant)))

(defun variant-data-type (variant)
  (check-type variant cffi:foreign-pointer)
  (first (variant-inner-type variant)))

(defvar *in-ref-p* nil)

(defclass variant-ref ()
  ((variant :initarg :variant :reader variant-ptr)
   (type :initarg :type :accessor variant-type)))

(defun make-variant-ref (ptr)
  (check-type ptr cffi:foreign-pointer)
  (unless (variant-ref-p ptr)
    (error "Variant ~S is not a by-ref variant." ptr))

  (make-instance 'variant-ref
                 :variant ptr
                 :type (make-instance 'variant :val-type (variant-inner-type ptr))))

(defgeneric deref (ref)
  (:method ((ref variant-ref))
    ;; Don't return another VARIANT-REF.
    (let ((*in-ref-p* t))
      (cffi:convert-from-foreign (variant-ptr ref) 'variant))))

(defgeneric (setf deref) (val ref type)
  (:method (val (ref variant-ref) type)
    (check-type type (or keyword list))
    (let ((inner-type (if (keywordp type)
                          (list type)
                          type)))
      (setf (variant-type ref) `(variant ,inner-type))
      (cffi:convert-into-foreign-memory val (variant-type ref) (variant-ptr ref)))
    val))

(defun variant-slot-pointer (ptr slot)
  (check-type ptr cffi:foreign-pointer)
  (check-type slot symbol)
  (cffi:foreign-slot-pointer (cffi:foreign-slot-pointer ptr '(:struct variant-struct) 'element)
                             '(:union variant-element)
                             slot))


;; (cffi:defcunion variant-element
;;   (llval :long-long)
;;   (lval :long)
;;   (bval :uchar)
;;   (ival :short)
;;   (fltval :float)
;;   (dblval :double)
;;   ;; Why are there two of these?
;;   (boolval variant-bool)
;;   (bool variant-bool)
;;   (scode scode)
;;   (cyval currency)
;;   (date date)
;;   (bstrval bstr)
;;   (pointer :pointer)
;;   (hresultval hresult)
;;   (cval :char)
;;   (uival :ushort)
;;   (ulval :ulong)
;;   (ullval :unsigned-long-long)
;;   (intval :int)
;;   (uintval :uint)
;;   (record (:struct record-struct)))

(defun variant-data-pointer (variant)
  (check-type variant cffi:foreign-pointer)
  (cond
    ((variant-ref-p variant) (variant-slot-pointer variant 'pointer))
    ;; SAFEARRAY of the inner type.
    ((variant-array-p variant) (variant-slot-pointer variant 'pointer))
    (t (ecase (variant-data-type variant)
         (:empty (error "Variant is uninitialized, cannot retrieve data."))
         ;; :null will be handled elsewhere.
         (:int16 (variant-slot-pointer variant 'ival))
         (:int32 (variant-slot-pointer variant 'lval))
         (:float (variant-slot-pointer variant 'fltval))
         (:double-float (variant-slot-pointer variant 'dblval))
         (:currency (variant-slot-pointer variant 'cyval))
         (:bstr (variant-slot-pointer variant 'bstrval))
         (:error (variant-slot-pointer variant 'hresultval))
         (:hresult (variant-slot-pointer variant 'hresultval))
         (:bool (variant-slot-pointer variant 'boolval))
         (:char (variant-slot-pointer variant 'cval))
         (:uchar (variant-slot-pointer variant 'bval))
         (:ushort (variant-slot-pointer variant 'uival))
         (:uint32 (variant-slot-pointer variant 'ulval))
         (:int64 (variant-slot-pointer variant 'llval))
         (:uint64 (variant-slot-pointer variant 'ullval))
         (:void (variant-slot-pointer variant 'pointer))
         (:idispatch (variant-slot-pointer variant 'pointer))
         (:iunknown (variant-slot-pointer variant 'pointer))
         (:variant (variant-slot-pointer variant 'pointer))
         (:decimal (variant-slot-pointer variant 'pointer))
         (:record (variant-slot-pointer variant 'record))))))

(defun variant-foreign-type (variant)
  (check-type variant cffi:foreign-pointer)
  (ecase (variant-data-type variant)
    (:empty nil) ; Finally, a use for the NIL type.
    ;; :null will be handled elsewhere.
    (:int16 :int16)
    (:int32 :int32)
    (:float :float)
    (:double-float :double-float)
    (:currency 'currency)
    (:bstr 'bstr)
    (:error 'hresult)
    (:hresult 'hresult)
    (:bool 'variant-bool)
    (:char :char)
    (:uchar :uchar)
    (:ushort :ushort)
    (:uint32 :uint32)
    (:int64 :int64)
    (:uint64 :uint64)
    (:void :pointer)
    (:idispatch :pointer)
    (:iunknown :pointer)
    (:variant 'variant)
    ;; TODO: this needs a type
    (:decimal (progn (warn "Conversion of DECIMAL variants is not yet supported.")
                     :pointer))
    (:record (progn (warn "Conversion of BRECORD variants is not yet supported.")
                    '(:struct record-struct)))))

(defmethod cffi:translate-from-foreign (value (type variant))
  (cond
    ((and (variant-ref-p value) (not *in-ref-p*))
     (make-variant-ref value))
    ((variant-array-p value)
     (error "Variant arrays are not currently supported."))
    ((eq (variant-data-type value) :null)
     nil)
    (t (let ((data-pointer (if (variant-ref-p value)
                               (cffi:mem-aref (variant-data-pointer value) :pointer)
                               (variant-data-pointer value)))
             (foreign-type (variant-foreign-type value)))
         (cffi:mem-aref data-pointer foreign-type)))))

;; TODO: should have some extension point for providing foreign types
;; for conversion.

(defun initialize-variant! (variant &key (type :empty) flags)
  (check-type variant cffi:foreign-pointer)
  (let ((reserved-ptr (cffi:foreign-slot-value variant '(:struct variant-struct) 'reserved))
        (record-pointer (cffi:foreign-slot-pointer
                         (cffi:foreign-slot-pointer variant '(:struct variant-struct) 'element)
                         '(:union variant-element)
                         'record))
        (variant-type (cons type flags)))
    (setf (cffi:foreign-slot-value variant '(:struct variant-struct) 'type) variant-type)
    (dotimes (i 3)
      (setf (cffi:mem-aref reserved-ptr 'dword i) 0))
    ;; We're making assumptions about the record-struct being the
    ;; largest element type here.
    (dolist (slot '(record record-info))
      (setf (cffi:foreign-slot-value record-pointer '(:struct record-struct) slot)
            (cffi:null-pointer)))))

(defmethod cffi:translate-into-foreign-memory (value (type variant) pointer)
  (let ((inner-type (variant-value-type type)))
    (initialize-variant! pointer)
    (setf (cffi:foreign-slot-value pointer '(:struct variant-struct) 'type) inner-type)
    (let* ((slot-pointer (variant-data-pointer pointer))
           (foreign-type (variant-foreign-type pointer))
           (val-ptr (if (variant-ref-p pointer)
                        (cffi:foreign-alloc foreign-type)
                        slot-pointer)))
      (if (variant-ref-p pointer)
          (progn
            (setf (cffi:mem-aref val-ptr foreign-type) value)
            (format *debug-io* "set val~%")
            ;; (cffi:convert-into-foreign-memory value foreign-type val-ptr)
            (setf (cffi:mem-aref slot-pointer :pointer) val-ptr))
          (setf (cffi:mem-aref slot-pointer foreign-type) value))
      (values))))

(defmethod cffi:translate-to-foreign (value (type variant))
  (let ((obj (cffi:foreign-alloc type)))
    (cffi:translate-into-foreign-memory value type obj)
    obj))

;; TODO: add free-translated-object to free non-pointer variants.

(cffi:defcstruct disp-params
  (args (:pointer variant))
  ;; array of DISPIDs for named arguments.
  (named-arg-ids (:pointer dispid))
  (arg-count :uint)
  (named-arg-count :uint))

(cffi:defcstruct excep-info
  (error-code word)
  (reserved word)
  (error-source bstr)
  (error-description bstr) ; use NULL for none
  (help-file-path bstr) ; use NULL for none
  (help-context dword)
  (scode scode) ; alternative to ERROR-CODE, don't use it.
  )
