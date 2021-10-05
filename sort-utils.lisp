(in-package :sort-utils)

(declaim (optimize (speed 3)
                   (space 2)))

(defun radix-sort! (array divisor)
  "Implements the LSB radix sort.  Array argument is modified and
contains the sorted elements at the end of the computation."
  (declare (array array))
  (declare (integer divisor))
  (let* ((len (length array))
         (counts (make-array divisor
                             :element-type 'integer
                             :initial-element 0))
         (buf1 array)
         (buf2 (make-array len
                           :element-type 'integer
                           :initial-element 0))
         (max -1)
         (div 1))
    (labels ((resetbins ()
               (loop
                  for i below divisor
                  do (setf (aref counts i) 0)))
             (firstbin () ;; special first-pass count function
               (loop
                  for i below len
                  for x = (aref buf1 i)
                  do
                    (when (> x max)
                      (setf max x))
                  ;; This isn't necessary since div=1:
                  ;; 
                  ;; (incf (aref counts (mod (truncate x div)
                  ;;                         divisor)))
                  ;; So we can just do this:
                    (incf (aref counts (mod x divisor)))))
             (bin ()
               (loop
                  for i below len
                  for x = (aref buf1 i)
                  do
                    (incf (aref counts (mod (truncate x div)
                                            divisor)))))
             (prefixsum ()
               (let* ((sum (aref counts 0)))
                 (loop
                    for i from 1 below divisor
                    do
                      (setf sum
                            (incf (aref counts i)
                                  sum)))))
             (unpack ()
               (loop
                  for i below len
                  for j = (- len i 1)
                  for x = (aref buf1 j)
                  do
                    (symbol-macrolet ((count
                                       (aref counts
                                             (mod (truncate x div)
                                                  divisor))))
                      (setf (aref buf2
                                  (decf count))
                            x)))))
      ;; first pass
      (firstbin)
      (prefixsum)
      (unpack)
      (rotatef buf1 buf2)
      (setf div (* div divisor))
      ;; rest
      (loop
         while (<= div max)
         do
           (resetbins)
           (bin)
           (prefixsum)
           (unpack)
           (rotatef buf1 buf2)
           (setf div (* div divisor)))
      (when (not (eq buf1 array))
        (loop
           for i below len
           do (setf (aref array i)
                    (aref buf1 i))))
      array)))
