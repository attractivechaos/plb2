(defpackage :matmul
  (:use :cl)
  (:export #:main))

(in-package :matmul)

(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)
                   (compilation-speed 0)))

(defun matgen (n)
  (declare (type (Unsigned-Byte 32) n))
  (let ((const (/ 1.0
                  (coerce n 'Double-Float)
                  (coerce n 'Double-Float)))
        (res (make-array (list n n)
                         :element-type 'Double-Float
                         :initial-element 0d0)))
    (declare (type Double-Float const))
    (dotimes (x n res)
      (dotimes (y n)
        (setf (aref res x y)
              (* const
                 (- x y)
                 (+ x y)))))))

(defun matmul (n a b)
  (declare (type (Unsigned-Byte 32) n)
           (type (Simple-Array Double-Float (* *)) a b))
  (let ((res (make-array (list n n)
                         :element-type 'Double-Float
                         :initial-element 0d0)))
    (dotimes (i n res)
      (dotimes (j n)
        (let ((aij (aref a i j)))
          (declare (type Double-Float aij))
          (dotimes (k n)
            (incf (aref res i k)
                  (* aij
                     (aref b j k)))))))))

(defun main nil
  (let* ((n 1500)
         (m (ash n -1)))
    (declare (type (Unsigned-Byte 32) n m))
    (format t "~F~%"
            (aref (matmul n
                          (matgen n)
                          (matgen n))
                  m m))))
