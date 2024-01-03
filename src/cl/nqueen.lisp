(defpackage :nqueen
  (:use :cl)
  (:export #:main))

(in-package :nqueen)

(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)
                   (compilation-speed 0)))

(defun solve (n)
  (declare (type (Integer 0 63) n))
  (let ((a (make-array n
                       :element-type 'Fixnum
                       :initial-element -1))
        (l (make-array n
                       :element-type 'Fixnum
                       :initial-element 0))
        (c (make-array n
                       :element-type 'Fixnum
                       :initial-element 0))
        (r (make-array n
                       :element-type 'Fixnum
                       :initial-element 0))
        (i 0)
        (k 0)
        (m 0)
        (y 0)
        (y0 (1- (the Fixnum (ash (the Fixnum 1) n))))
        (z 0))
    (declare (type (Integer 0 63) i)
             (type Fixnum k m y y0 z))
    (tagbody
     beginning
       (if (< k 0)
           (go end)
           (setf y
                 (logand (logior (aref l k)
                                 (aref c k)
                                 (aref r k))
                         y0)))
       (setf i (1+ (aref a k)))
       (when (= (ash (the Fixnum (logxor y y0))
                     (the Fixnum (- i)))
                0)
         (go backtrack))
     inc-i
       (setf z (ash 1 i))
       (when (and (< i n)
                  (/= (logand y z)
                      0))
         (incf i)
         (go inc-i))
       (if (< k (1- n))
           (progn
             (setf (aref a k) i)
             (incf k)
             (setf (aref l k)
                   (ash (logior (aref l (1- k))
                                z)
                        1))
             (setf (aref c k)
                   (logior (aref c (1- k))
                           z))
             (setf (aref r k)
                   (ash (logior (aref r (1- k))
                                z)
                        -1)))
           (progn
             (incf m)
             (decf k)))
       (go beginning)
     backtrack
       (setf (aref a k)
             -1)
       (decf k)
       (go beginning)
     end)
    m))

(defun main nil
  (format t "~A~%"
          (solve 15)))
