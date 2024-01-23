(defpackage :sudoku
  (:use :cl)
  (:export #:main))

(in-package :sudoku)

(declaim (optimize (speed 3) (space 0) (safety 0) (debug 0)
                   (compilation-speed 0)))

(deftype I8 nil
  '(Signed-Byte 8))

(deftype U8 nil
  '(Unsigned-Byte 8))

(deftype I16 nil
  '(Signed-Byte 16))

(deftype U16 nil
  '(Unsigned-Byte 16))

(deftype Sudoku-String nil
  '(Simple-String 81))

(declaim (type (Simple-Array U16 (729 4)) *choices*))
(defparameter *choices*
  (make-array '(729 4)
              :element-type 'U16
              :initial-contents
              (loop :for i :below 9
                    :append
                    (loop :for j :below 9
                          :append
                          (loop :for k :below 9
                                :collect
                                (list (+ (* i 9) j)
                                      (+ (* (+ (* (truncate i 3) 3)
                                               (truncate j 3))
                                            9)
                                         k 81)
                                      (+ (* i 9)
                                         k 162)
                                      (+ (* j 9)
                                         k 243)))))))

(declaim (type (Simple-Array U16 (324 9)) *constraints*))
(defparameter *constraints*
  (let ((constraints (make-array '(324 9)
                                 :element-type 'U16
                                 :initial-element 0))
        (nr (make-array 324 :element-type 'I8 :initial-element 0)))
    (dotimes (r 729 constraints)
      (dotimes (c 4)
        (let ((k (aref *choices* r c)))
          (setf (aref constraints k
                      (aref nr k))
                r)
          (incf (aref nr k)))))))

(defmacro setor (place value)
  `(setf ,place
         (logior ,place ,value)))

(defmacro setand (place value)
  `(setf ,place
         (logand ,place ,value)))

(defun solve (puzzle)
  (declare (type Sudoku-String puzzle))
  (let ((sr (make-array 729 :element-type 'I8 :initial-element 0))
        (sc (make-array 324 :element-type 'U8 :initial-element 9))
        (cr (make-array 81 :element-type 'I8 :initial-element -1))
        (cc (make-array 81 :element-type 'I16 :initial-element -1))
        (out (copy-seq puzzle))
        (dir t)
        (hints 81)
        (cand (ash 10 16))
        (solutions 0)
        (i 0))
    (declare (type Fixnum hints cand solutions i))
    (flet ((update-forward (choice)
             (declare (type Fixnum choice))
             (let ((min 10)
                   (min-c 0))
               (dotimes (c2 4)
                 (setor (aref sc
                              (aref *choices* choice c2))
                        (ash 1 7)))
               (dotimes (c2 4)
                 (let ((c (aref *choices* choice c2)))
                   (dotimes (r2 9)
                     (let* ((rr (aref *constraints* c r2))
                            (srrr (aref sr rr)))
                       (incf (aref sr rr))
                       (when (= srrr 0)
                         (dotimes (cc2 4)
                           (let ((cc (aref *choices* rr cc2)))
                             (when (< (decf (aref sc cc))
                                      min)
                               (setf min (aref sc cc))
                               (setf min-c cc)))))))))
               (logior (ash min 16)
                       min-c)))
           (update-backward (choice)
             (declare (type Fixnum choice))
             (dotimes (c2 4)
               (setand (aref sc
                             (aref *choices* choice c2))
                       #x7F))
             (dotimes (c2 4)
               (let ((c (aref *choices* choice c2)))
                 (dotimes (r2 9)
                   (let ((rr (aref *constraints* c r2)))
                     (when (= (decf (aref sr rr))
                              0)
                       (incf (aref sc (aref *choices* rr 0)))
                       (incf (aref sc (aref *choices* rr 1)))
                       (incf (aref sc (aref *choices* rr 2)))
                       (incf (aref sc (aref *choices* rr 3))))))))))
      (dotimes (i 81)
        (let ((a (digit-char-p (aref puzzle i))))
          (when a
            (update-forward (+ (* i 9) a -1))
            (decf hints))))
      (tagbody
       beginning
         (unless (< -1 i hints)
           (go after-run))
         (when dir
           (let ((min (ash cand -16)))
             (declare (type Fixnum min))
             (setf (aref cc i)
                   (logand cand #xFFFF))
             (when (> min 1)
               (dotimes (c 324)
                 (when (< (aref sc c)
                          min)
                   (setf min
                         (aref sc c))
                   (setf (aref cc i)
                         c)
                   (when (<= min 1)
                     (return)))))
             (when (or (= min 0)
                       (= min 10))
               (setf (aref cr i)
                     -1)
               (decf i)
               (setf dir nil))))
         (let ((c (aref cc i))
               (cri (aref cr i)))
           (when (and (not dir)
                      (>= cri 0))
             (update-backward (aref *constraints* c cri)))
           (let ((r2 (do ((r2 (1+ cri)
                              (1+ r2)))
                         ((or (= r2 9)
                              (= (aref sr
                                       (aref *constraints* c r2))
                                 0))
                          r2))))
             (declare (type Fixnum r2))
             (if (< r2 9)
                 (progn
                   (setf cand
                         (update-forward (aref *constraints* c r2)))
                   (setf (aref cr i)
                         r2)
                   (incf i)
                   (setf dir t))
                 (progn
                   (setf (aref cr i)
                         -1)
                   (decf i)
                   (setf dir nil)))))
         (go beginning)
       after-run
         (when (< i 0)
           (go solved))
         (dotimes (j i)
           (multiple-value-bind (index value)
               (truncate (aref *constraints*
                               (aref cc j)
                               (aref cr j))
                         9)
             (setf (aref out index)
                   (digit-char (1+ value)))))
         (princ out)
         (terpri)
         (incf solutions)
         (decf i)
         (setf dir nil)                 ; backtrack.
         (go beginning)
       solved)
      solutions)))

(defun main nil
  (let ((puzzles
          '#("..............3.85..1.2.......5.7.....4...1...9.......5......73..2.1........4...9"
             ".......12........3..23..4....18....5.6..7.8.......9.....85.....9...4.5..47...6..."
             ".2..5.7..4..1....68....3...2....8..3.4..2.5.....6...1...2.9.....9......57.4...9.."
             "........3..1..56...9..4..7......9.5.7.......8.5.4.2....8..2..9...35..1..6........"
             "12.3....435....1....4........54..2..6...7.........8.9...31..5.......9.7.....6...8"
             "1.......2.9.4...5...6...7...5.9.3.......7.......85..4.7.....6...3...9.8...2.....1"
             ".......39.....1..5..3.5.8....8.9...6.7...2...1..4.......9.8..5..2....6..4..7....."
             "12.3.....4.....3....3.5......42..5......8...9.6...5.7...15..2......9..6......7..8"
             "..3..6.8....1..2......7...4..9..8.6..3..4...1.7.2.....3....5.....5...6..98.....5."
             "1.......9..67...2..8....4......75.3...5..2....6.3......9....8..6...4...1..25...6."
             "..9...4...7.3...2.8...6...71..8....6....1..7.....56...3....5..1.4.....9...2...7.."
             "....9..5..1.....3...23..7....45...7.8.....2.......64...9..1.....8..6......54....7"
             "4...3.......6..8..........1....5..9..8....6...7.2........1.27..5.3....4.9........"
             "7.8...3.....2.1...5.........4.....263...8.......1...9..9.6....4....7.5..........."
             "3.7.4...........918........4.....7.....16.......25..........38..9....5...2.6....."
             "........8..3...4...9..2..6.....79.......612...6.5.2.7...8...5...1.....2.4.5.....3"
             ".......1.4.........2...........5.4.7..8...3....1.9....3..4..2...5.1........8.6..."
             ".......12....35......6...7.7.....3.....4..8..1...........12.....8.....4..5....6.."
             "1.......2.9.4...5...6...7...5.3.4.......6........58.4...2...6...3...9.8.7.......1"
             ".....1.2.3...4.5.....6....7..2.....1.8..9..3.4.....8..5....2....9..3.4....67....."))
        (n 200))
    (declare (type Fixnum n)
             (type (Simple-Array Sudoku-String (20)) puzzles))
    (dotimes (i n)
      (loop :for puzzle :of-type Sudoku-String :across puzzles
            :do
               (format t "~A~%"
                       (solve puzzle))))))
