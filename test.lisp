(require 'sort-utils)
(in-package :sort-utils)

(defun random-unsigned-array (&key
                                (length 100)
                                (max 100))
  (let* ((result (make-array length :element-type 'integer)))
    (loop
       for i below length
       do (setf (aref result i)
                (random (1+ max))))
    result))

(defun check-sort (array &optional (predicate #'<=))
  (every #'identity
         (map 'list
              predicate
              array
              (subseq array 1))))
