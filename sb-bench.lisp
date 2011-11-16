(in-package :microbench)

;;;; Sample Benchmarks

(defun generic+ (x y)
  (+ x y))

(defbenchmark generic+[fff] (:group generic+)
  (generic+ 13212 9283))

(defbenchmark generic+[ffb] (:group generic+)
  (generic+ 123 most-positive-fixnum))

(defbenchmark generic+[fbf] (:group generic+)
  (generic+ most-negative-fixnum #.(+ 123 most-positive-fixnum)))

(defbenchmark generic+[fbb] (:group generic+)
  (generic+ 123 #.(+ 1203981 most-positive-fixnum)))

(defbenchmark generic+[bbb] (:group generic+)
  (generic+ #.(+ 123 most-positive-fixnum)
            #.(* 2 most-positive-fixnum)))

(defbenchmark generic+[bbf] (:group generic+)
  (generic+ #.(+ most-positive-fixnum 1000)
            #.(- most-negative-fixnum 1000)))

(defbenchmark generic+[sss] (:group generic+)
  (generic+ 123.0f0 129387.0f0))

(defbenchmark generic+[fss] (:group generic+)
  (generic+ 123 129387.1f0))

(defbenchmark generic+[ddd] (:group generic+)
  (generic+ 123.9d0 129387.1d0))

(defbenchmark generic+[fdd] (:group generic+)
  (generic+ 123 129387.12d0))

(defbenchmark generic+[sdd] (:group generic+)
  (generic+ 123.1f0 129387.13d0))

(defbenchmark generic+[cfcfcf] (:group generic+)
  (generic+ #c(12 123) #c(987 231)))

(defbenchmark generic+[cscscs] (:group generic+)
  (generic+ #c(12.123f0 123.123f0) #c(987.987f0 231.231f0)))

(defbenchmark generic+[cdcdcd] (:group generic+)
  (generic+ #c(12.123d0 123.123d0) #c(987.987d0 231.231d0)))

(defun fixnum+ (x y)
  (declare (fixnum x y))
  (+ x y))

(defbenchmark fixnum+ (:group specialized+)
  (fixnum+ 1239 1098))

(defun modular-fixnum+ (x y)
  (declare (fixnum x y))
  (logand most-positive-fixnum (+ x y)))

(defbenchmark modular-fixnum+ (:group specialized+)
  (modular-fixnum+ 1239 1098))

(defun complex-single+ (x y)
  (declare (type (complex single-float) x y))
  (+ x y))

(defbenchmark complex-single+ (:group specialized+)
  (complex-single+ #c(12.123f0 123.123f0) #c(987.987f0 231.231f0)))

(defun complex-double+ (x y)
  (declare (type (complex double-float) x y))
  (+ x y))

(defbenchmark complex-double+ (:group specialized+)
  (complex-double+ #c(12.123d0 123.123d0) #c(987.987d0 231.231d0)))

(defun single+ (x y)
  (declare (single-float x y))
  (+ x y))

(defbenchmark single+ (:group specialized+)
  (single+ 123.123f0 534.534f0))

(defun double+ (x y)
  (declare (double-float x y))
  (+ x y))

(defbenchmark double+ (:group specialized+)
  (double+ 411.1231d0 897.1231d0))

(defun double-sans-result+ (x y)
  (declare (double-float x y))
  (= 0.0d0 (+ x y)))

(defbenchmark double-sans-result+ (:group specialized+)
  (double-sans-result+ 123.123d0 9870.213d0))

(defun double-unsafe-sans-result+ (x y)
  (declare (double-float x y)
           (optimize (safety 0)))
  (= 0.0d0 (+ x y)))
(defbenchmark double-unsafe-sans-result+ (:group specialized+)
  (double-unsafe-sans-result+ 123.123d0 9870.213d0))

;;; This is a faintly ridiculous amount to get a "fair" comparison to
;;; benchmarks doing out-of-line calls, but seems about right.
;;;
;;; ...and there's no guarantee that it isn't pessimized for another
;;; implementation.
;;;
;;; All this really does is goes to show that we need a decent non-micro
;;; benchmark for FP.
(declaim (notinline a-single-float))
(defun a-single-float ()
  (load-time-value 123.123f0))
(declaim (inline single-inline+))
(defun single-inline+ (x y)
  (declare (single-float x y))
  (+ x y))
(defbenchmark single-inline+ (:group specialized+
                              :let ((x (a-single-float))
                                    (y (a-single-float)))
                              :declare ((single-float x y)))
  (when (= 0.0f0 (setf x (single-inline+ x y)))
    (throw 'oops t)))

(declaim (notinline a-complex-single-float))
(defun a-complex-single-float ()
  (load-time-value (complex 534.123f0 123.123f0)))
(declaim (inline complex-single-inline+))
(defun complex-single-inline+ (x y)
  (declare (type (complex single-float) x y))
  (+ x y))
(defbenchmark complex-single-inline+ (:group specialized+
                              :let ((x (a-complex-single-float))
                                    (y (a-complex-single-float)))
                              :declare ((type (complex single-float) x y)))
  (when (= #c(0.0f0 0.0f0) (setf x (complex-single-inline+ x y)))
    (throw 'oops t)))

(declaim (notinline a-double-float))
(defun a-double-float ()
  (load-time-value 123.123d0))
(declaim (inline double-inline+))
(defun double-inline+ (x y)
  (declare (double-float x y))
  (+ x y))
(defbenchmark double-inline+ (:group specialized+
                              :let ((x (a-double-float))
                                    (y (a-double-float)))
                              :declare ((double-float x y)))
  (when (= 0.0d0 (setf x (double-inline+ x y)))
    (throw 'oops t)))

(declaim (notinline a-complex-double-float))
(defun a-complex-double-float ()
  (load-time-value (complex 534.123d0 123.123d0)))
(declaim (inline complex-double-inline+))
(defun complex-double-inline+ (x y)
  (declare (type (complex double-float) x y))
  (+ x y))
(defbenchmark complex-double-inline+ (:group specialized+
                              :let ((x (a-complex-double-float))
                                    (y (a-complex-double-float)))
                              :declare ((type (complex double-float) x y)))
  (when (= #c(0.0d0 0.0d0) (setf x (complex-double-inline+ x y)))
    (throw 'oops t)))

(declaim (notinline a-fixnum))
(defun a-fixnum ()
  42)
(declaim (inline fixnum-inline+))
(defun fixnum-inline+ (x y)
  (declare (fixnum x y))
  (+ x y))
(defbenchmark fixnum-inline+ (:group specialized+
                              :let ((x (a-fixnum))
                                    (y (a-fixnum)))
                              :declare ((fixnum x y)))
  (when (eql 0 (setf x (fixnum-inline+ x y)))
    (throw 'oops t)))
(defbenchmark modular-fixnum-inline+ (:group specialized+
                              :let ((x (a-fixnum))
                                    (y (a-fixnum)))
                              :declare ((fixnum x y)))
  (when (eql 0 (setf x (logand most-positive-fixnum (fixnum-inline+ x y))))
    (throw 'oops t)))
