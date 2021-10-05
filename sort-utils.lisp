(in-package :sort-utils)

(declaim (optimize (speed 3)
                   (space 2)))

(defun bubble-sort! (array
                     &optional
                       (predicate #'<=))
  "Bubble sort implementation.  Note that #'< and #'> do not work with
this sort algorithm whenever there are repeated values."
  (let* ((differ t)
         (len (length array)))
  (loop
     while differ
     do
       (setf differ nil)
       (loop
          for i below (1- len)
          do (when (not (funcall predicate
                                 (aref array i)
                                 (aref array (1+ i))))
               (rotatef (aref array i)
                        (aref array (1+ i)))
               (setf differ t))))
  array))

(defun merge-sort! (array
                    &optional
                      (predicate #'<))
  "Implements top-down merge sort for arrays."
  (let* ((n (length array))
         (b (make-array n)))
    (labels ((copy (a begin end b)
               (loop
                  for k from begin below end
                  do (setf (aref b k)
                           (aref a k))))
             (merge! (a begin middle end b)
               (let* ((i begin)
                      (j middle))
                 (loop
                    for k from begin below end
                    do
                      (if (and (< i middle)
                               (or (>= j end)
                                   (funcall predicate
                                            (aref a i)
                                            (aref a j))))
                          (progn
                            (setf (aref b k)
                                  (aref a i))
                            (incf i))
                          (progn
                            (setf (aref b k)
                                  (aref a j))
                            (incf j))))))
             (splitmerge! (b begin end a)
               (when (> (- end begin) 1)
                 (let* ((middle (truncate (+ end begin)
                                          2)))
                   (splitmerge! a begin middle b)
                   (splitmerge! a middle end b)
                   (merge! b begin middle end a))))
             (mergesort! (a b)
               (copy a 0 n b)
               (splitmerge! b 0 n a)))
      (mergesort! array b)
      array)))

(defun count-sort! (array)
  "Count sort for non-negative integers."
  (let* ((max (reduce #'max array))
         (counts (make-array (1+ max)
                             :element-type 'integer
                             :initial-element 0))
         (index 0))
    (map nil (lambda (x)
               (incf (aref counts x)))
         array)
    (loop
       for i upto max
       for c = (aref counts i)
       do (loop
             for j below c
             do (setf (aref array (1- (incf index)))
                      i)))
    array))

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
