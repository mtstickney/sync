(in-package #:cl-persist)

(defun vector-expand (item vec max-size)
  (check-type vec vector)
  ;; Double the size with a special case for 0 instead of using 2n+1
  ;; (yields power-of-two sized arrays, which we want).
  (flet ((next-size (n)
           (if (= n 0)
               1
               (* 2 n))))
    ;; If the vector is already full, we signal an error:
    (when (>= (length vec)
              max-size)
      (error "Vector is full, cannot push item"))
    (let* ((allocated (array-total-size vec))
           ;; Re-allocate if necessary
           (new-vec (if (>= (length vec) allocated)
                         (adjust-array vec (min max-size
                                                (next-size allocated)))
                         vec)))
      (vector-push item new-vec)
      ;; Return the new array so it can be stored somewhere
      ;; (adjust-array may not happen in-place)
      new-vec)))

;; To get the child of a node in the tree corresponding to a
;; particular key, we must first extract an index for this particular
;; level of the tree.
(defun key-partition (key partition partition-bits)
  (check-type key unsigned-byte)
  (check-type partition (integer 1))
  (check-type partition-bits (integer 1))
  ;; If PARTITION-BITS is the number of bits for a partition index, the portion
  ;; of the key we want is the (1- partition)th group of PARTITION-BITS bits,
  ;; from least significant to most. First we construct a mask of
  ;; PARTITION-BITS bits:
  (logand (1- (ash 1 partition-bits))
          ;; Right-shifting the key by (1- height) groups of NODE-BITS
          ;; bits and ANDing with the mask yields our index:
          (ash key (- (* partition-bits (1- partition))))))
