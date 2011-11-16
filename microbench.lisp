;;;; My last benchmark harness didn't end up getting finished.
;;;;
;;;; Possibly because I started from bottom up, not thinking about
;;;; the interface I would like to use to look at the results.
;;;;
;;;; So this is another try, from top down.
;;;;
;;;; Or both ends, or something.

(defpackage :microbench
  (:use :cl :alexandria :cl-who)
  (:export
   #:html-report
   #:run-benchmarks
   #:select-benchmarks))

(in-package :microbench)

;;;; Couple of utilities that really should go into Alexandria

(defun read-file (pathname &key (external-format :default) (if-does-not-exist :error)
                                junk-allowed)
  "Returns the contents of the file denoted by PATHNAME as read by READ as
primary value.

Secondary value is NIL if file contains nothing more to for READ to process.
If JUNK-ALLOWED is true and the file contains more than one form, returns the
FILE-POSITION at the end of the read as secondary value.

If JUNK-ALLOWED is NIL (the default), and file contains more than one
form, an error is signalled."
  (with-open-file (stream pathname :direction :input
                                   :external-format external-format
                                   :if-does-not-exist if-does-not-exist)
    (if stream
        (let* ((obj (read stream))
               (p (file-position stream))
               (peek (peek-char t stream nil))
               (eof (or (not peek)
                        (eq stream (read stream nil stream)))))
          (cond (eof
                 (values obj nil))
                (junk-allowed
                 (values obj p))
                (t
                 (error "Junk at the end of file ~A, position ~A."
                        pathname p))))
        (values nil nil))))

(defun read-file-into-list (pathname &key (external-format :default) (if-does-not-exist nil))
  "Returns the contents of the file denoted by PATHNAME as a list read
successive calls to READ. Secondary return value is T if the file existed.

IF-DOES-NOT-EXIST defaults to NIL."
  (with-open-file (stream pathname :direction :input
                                   :external-format external-format
                                   :if-does-not-exist if-does-not-exist)
    (if stream
        (values (loop for form = (read stream nil stream)
                      until (eq stream form)
                      collect form)
                t)
        (values nil nil))))

;;;; Configuration

(defvar *hostname-prefixes* nil)
(defvar *benchmark-datadir* nil)

(defun user-conf-pathname ()
  (merge-pathnames ".microbench.conf" (user-homedir-pathname)))

(defun benchmark-datadir ()
  (or *benchmark-datadir*
      (merge-pathnames "/var/tmp/microbench/")))

(defun load-user-conf ()
  (setf *hostname-prefixes* nil
        *benchmark-datadir* nil)
  (dolist (conf (read-file-into-list (user-conf-pathname)))
    (destructuring-ecase conf
      ((:hostname-prefixes &rest prefixes)
       (setf *hostname-prefixes* prefixes))
      ((:datadir dir)
       (setf *benchmark-datadir* (merge-pathnames dir))))))

(load-user-conf)

;;;; Defining Benchmarks and accessing them

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *benchmark-info* (make-hash-table))
  (defparameter *benchmark-names* nil))

(defun get-benchmark (name)
  (let ((info (or (gethash name *benchmark-info*)
                  (error "Unknown benchmark: ~S" name))))
    (values (fdefinition (first info)) (rest info))))

(defun list-all-benchmarks ()
  (hash-table-keys *benchmark-info*))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun quote-plist-values (plist)
    (let (copy)
      (doplist (key val plist)
        (push key copy)
        (push (list 'quote val) copy))
      (reverse copy))))

(defmacro defbenchmark (name (&rest options &key let declare &allow-other-keys)
                        &body body)
  (let ((fname (symbolicate '#:benchmark- name)))
    (remove-from-plistf options :let :declare)
    (with-gensyms (iterations)
      `(progn
         (defun ,fname (,iterations)
           (declare (fixnum ,iterations))
           (let ,let
             (declare ,@declare)
             (loop repeat ,iterations
                   do (locally ,@body))))
         (setf (gethash ',name *benchmark-info*)
               (list ',fname ,@(quote-plist-values options)))))))

(defun round-up (n)
  (loop for i = 1 then (* i 10)
        and j = i
        when (> i n)
        do (loop for k = j then (+ k j)
                 when (>= k n)
                 do (return-from round-up k))))

(defun estimate-iterations (type seconds fun arguments)
  (declare (function fun))
  (let ((iterations 1))
    (declare (fixnum iterations))
    (let ((time-fun (ecase type
                      (:run-time
                       #'get-internal-run-time)
                      (:real-time
                       #'get-internal-real-time))))
      (loop
        (let ((trial-arguments (cons iterations arguments))
              (start (funcall time-fun)))
          (declare (dynamic-extent trial-arguments))
          (apply fun trial-arguments)
          (let ((time (- (funcall time-fun) start)))
            (cond ((>= (truncate internal-time-units-per-second 10) time)
                   (setf iterations (* iterations 10)))
                  ((>= (truncate internal-time-units-per-second 2) time)
                   (setf iterations (* iterations 2)))
                  (t
                   (let* ((elapsed (/ time internal-time-units-per-second))
                          (n (max 1 (round (* iterations (/ seconds elapsed))))))
                     (return (values (round-up n))))))))))))

(defun before-benchmark ()
  #+sbcl
  (sb-ext:gc :full t)
  #-sbcl
  nil)

(defun run-benchmark (name &key (arguments t) run-time real-time runs iterations
                                (verbose t))
  (multiple-value-bind (fun info) (get-benchmark name)
    (declare (function fun))
    (let* ((arguments (if (eq t arguments) (getf info :arguments) arguments))
           (run-time (or run-time (getf info :run-time)))
           (real-time (or real-time (getf info :real-time)))
           (runs (or runs 1))
           (iterations
             (or iterations
                 (cond ((and run-time real-time)
                        (max (estimate-iterations :run-time run-time fun arguments)
                             (estimate-iterations :real-time real-time fun arguments)))
                       (run-time
                        (estimate-iterations :run-time run-time fun arguments))
                       (t
                        (estimate-iterations :real-time (or real-time 1.0) fun arguments)))))
           (run-arguments (cons iterations arguments))
           (run-time (make-array runs :element-type 'fixnum))
           (real-time (make-array runs :element-type 'fixnum)))
      (check-type iterations positive-fixnum)
      (check-type runs positive-fixnum)
      (when verbose
        (format *trace-output* "~A, ~A x ~A"
                name runs iterations)
        (finish-output *trace-output*))
      (let ((ut (get-universal-time)))
        (before-benchmark)
        (loop for i from 0 below runs
              do (let ((real-time-start (get-internal-real-time))
                       (run-time-start (get-internal-run-time)))
                   (apply fun run-arguments)
                   (setf (aref real-time i) (- (get-internal-real-time) real-time-start))
                   (setf (aref run-time i) (- (get-internal-run-time) run-time-start))))
        (when verbose
          (let ((real-total (/ (reduce #'+ real-time) internal-time-units-per-second))
                (run-total (/ (reduce #'+ run-time) internal-time-units-per-second)))
            (format *trace-output*
                    ", ~,2Fs run-time, ~,2F% CPU~%  => ~,2FM/s.~%"
                    run-total
                    (* 100.0 (/ run-total real-total))
                    (/ (/ (* runs iterations) 1f6) run-total)))
          (finish-output *trace-output*))
        (list name :iterations iterations
                   :runs runs
                   :run-time run-time
                   :real-time real-time
                   :timestamp ut
                   :hostname (hostname)
                   :arch (machine-type)
                   :os (software-type)
                   :lisp (lisp-implementation-type)
                   :version (lisp-implementation-version))))))

(defun datestamp (&optional time)
  (multiple-value-bind (sec min hour day mon year)
      (if time
          (decode-universal-time time)
          (get-decoded-time))
    (declare (ignore sec min hour))
    (format nil "~D-~2,'0D-~2,'0D" year mon day)))

(defun hostname ()
  (let* ((instance-name (machine-instance))
         (name-len (length instance-name)))
    (dolist (prefix *hostname-prefixes* instance-name)
      (let ((prefix-len (length prefix)))
        (when (and (<= prefix-len name-len)
                   (string= instance-name prefix :end1 prefix-len))
          (return prefix))))))

(defun make-benchmark-pathname (&key (benchmark :wild)
                                     (hostname :wild)
                                     (arch :wild)
                                     (os :wild)
                                     (lisp :wild)
                                     (version :wild)
                                     (date :wild))
  (merge-pathnames
   (make-pathname
    :directory (list :relative hostname os arch lisp version)
    :name (if (eq benchmark :wild)
              benchmark
              (string benchmark))
    :type date)
   (benchmark-datadir)))

(defun benchmark-pathname (data)
  (apply #'make-benchmark-pathname :allow-other-keys t
                                   :date (datestamp (getf (cdr data) :timestamp))
                                   :benchmark data))

(defun save-benchmark (data &key (if-exists :error))
  (let ((pathname (benchmark-pathname data)))
    (ensure-directories-exist pathname)
    (when (and (eq if-exists :error) (probe-file pathname))
      (cerror "Overwrite" "Benchmark data for this benchmark, host, and date already exists.")
      (setf if-exists :supersede))
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists if-exists
                            :external-format :utf-8)
      (let ((*package* (find-package :microbench)))
        (when stream
          (write data
                 :stream stream
                 :escape t))))
    data))

(defun run-benchmarks (&key run-time real-time iterations runs save (if-exists :error)
                            (verbose t)
                            (select nil))
  (let (results)
    (dolist (name (if select
                      (ensure-list select)
                      (list-all-benchmarks)))
      (let ((res (run-benchmark name
                                :runs runs
                                :run-time run-time :real-time real-time
                                :iterations iterations
                                :verbose verbose)))
        (when save
          (save-benchmark res :if-exists if-exists))
        (push res results)))
    results))

(defun select-benchmarks (&rest args &key (lisp :wild)
                                          (version :wild)
                                          (benchmark :wild)
                                          (date :wild)
                                          (hostname :wild)
                                          (arch :wild)
                                          (os :wild))
  (declare (ignore lisp version benchmark date hostname arch os))
  (mapcar (lambda (pathname)
            (let ((*package* (find-package :microbench)))
              (read-file pathname :external-format :utf-8)))
          (directory
           (apply #'make-benchmark-pathname args))))

(defun js-k/s (benchmarks)
  `(array
    ,@(mapcar (lambda (benchmark-set)
                (let ((x (cdar benchmark-set)))
                  `(create
                    :title
                    ,(format nil "~A ~A ~A/~A (~A)"
                             (getf x :lisp)
                             (getf x :version)
                             (getf x :os)
                             (string-downcase (getf x :arch))
                             (getf x :hostname))
                    :data
                    (array
                     ,@(mapcar (lambda (benchmark)
                                 (let* ((name (pop benchmark))
                                        (total-time (/ (reduce #'+ (getf benchmark :run-time))
                                                       internal-time-units-per-second))
                                        (total-iterations (* (getf benchmark :iterations)
                                                             (getf benchmark :runs))))
                                   `(array ,(string-downcase name)
                                           ,(/ (/ total-iterations 1f6) total-time))))
                               benchmark-set)))))
              benchmarks)))

(defun benchmark-ips (benchmark time)
  (let* ((plist (cdr benchmark))
         (total-time (/ (reduce #'+ (getf plist time))
                        internal-time-units-per-second))
         (total-iterations (* (getf plist :iterations)
                              (getf plist :runs))))
    (coerce (/ total-iterations total-time) 'single-float)))

(defun sort-benchmarks (dataset sort)
  (let ((table (make-hash-table :test #'equal)))
    (dolist (data dataset)
      (let* ((plist (cdr data))
             (key (list (getf plist :hostname) (getf plist :os) (getf plist :arch)
                        (getf plist :lisp) (getf plist :version))))
        (push data (gethash key table))))
    (mapcar (ecase sort
              (:name
               (lambda (set)
                 (sort set #'string< :key #'car)))
              ((:run-time :real-time)
               (lambda (set)
                 (sort set #'> :key (lambda (set)
                                      (benchmark-ips set sort))))))
            (hash-table-values table))))

(defun html-report (dataset pathname &key (type :barchart) (log-scale nil)
                                          (sort :run-time))
  (ecase type
    (:barchart
     (html-barchart dataset pathname log-scale sort))))

(defun html-barchart (dataset pathname log-scale sort)
  (let* ((sets (sort-benchmarks dataset sort))
         (divs (loop for i from 0 below (length sets)
                     collect (format nil "chart_~A" i))))
    (with-open-file (stream pathname
                            :direction :output
                            :if-exists :supersede)
      (with-html-output (stream nil :indent t)
        (:html
         (:head
          (:title "Microbench Report")
          (:script :type "text/javascript"
                   :src "https://www.google.com/jsapi")
          (:script :type "text/javascript"
                   (str
                    (js:js*
                     `(defvar benchmarks ,(js-k/s sets))
                     `(defvar log-scale ,log-scale)))
                   (str
                    (js:js
                     (google.load "visualization" "1" (create :packages (array "corechart")))
                     (google.set-on-load-callback draw-charts)
                     (defun draw-charts ()
                       (let ((i 0))
                         (dolist (set benchmarks)
                           (let ((data (google.visualization.array-to-data-table set.data t))
                                 (chart (new (google.visualization.*column-chart
                                              (document.get-element-by-id
                                               (+ "chart_" (++ i)))))))
                             (chart.draw data (create :title set.title
                                                      :legend "none"
                                                      :font-size 10
                                                      :height 400
                                                      :h-axis (create :max-alternation 1
                                                                      :show-text-every 1
                                                                      :slanted-text true
                                                                      :slanted-text-angle 30)
                                                      :v-axis (create :log-scale log-scale
                                                                      :title "Mi/s")))))))))))
         (:body
          (dolist (div divs)
            (htm
             (:div :id div))))))
      pathname)))

