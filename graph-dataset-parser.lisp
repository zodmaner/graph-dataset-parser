;;;; graph-dataset-parser.lisp

(in-package #:graph-dataset-parser)

;;; "graph-dataset-parser" goes here. Hacks and glory await!

(declaim (optimize (speed 3) (safety 0) (debug 0))
         (inline split-sequence:split-sequence))

(declaim (ftype (function (pathname pathname) null) parse-graph-dataset))
(defun parse-graph-dataset (src dst)
  "Parse a graph dataset file into a format that's suitable for
consumption by Pregel+/Palgol applications and write the result out to
a specified output file."
  (flet ((comment-p (line)
           (declare (simple-string line))
           (let ((first-char (aref line 0)))
             (or (char= #\# first-char) (char= #\% first-char))))
         (parse-line (line)
           (declare (simple-string line))
           (-<>> line
                 (split-sequence-if (lambda (c) (or (char= c #\Tab) (char= c #\space))))
                 (remove "" <> :test #'string=)
                 (mapcar (lambda (s) (parse-integer s)))))
         (write-vertex (v adjvs)
           (declare (fixnum v)
                    ((vector fixnum) adjvs))
           (let ((adjvs-len (length adjvs)))
             (declare (fixnum adjvs-len))
             (format *standard-output* "~A~A~A ~A" v #\Tab v adjvs-len)
             (if (= adjvs-len 0)
                 (format *standard-output* "~%")
                 (progn
                   (loop :for adjv fixnum :across adjvs
                      :do (format *standard-output* " ~A ~A" adjv v))
                   (format *standard-output* "~%")))))
         (add-vertex (v adjv ht)
           (declare (fixnum v)
                    ((or fixnum null) adjv))
           (let ((adjvs (gethash v ht
                                 (make-array 1 :element-type 'fixnum :fill-pointer 0))))
             (declare ((vector fixnum) adjvs))
             (when (and (not (null adjv))
                        (not (find adjv adjvs)))
               (vector-push-extend adjv adjvs))
             (setf (gethash v ht) adjvs))))
    (declare (inline comment-p
                     parse-line
                     write-vertex
                     add-vertex))
    (let ((v-adjvs (make-hash-table :size 10000000)))
      (with-open-file (in-stream src)
        (loop :with lr fixnum := 0
           :for line :of-type (or simple-string null) := (read-line in-stream nil nil)
           :while line :do
           (when (not (comment-p line))             
             (multiple-value-bind (v adjv) (values-list (parse-line line))
               (declare (fixnum v adjv))
               (add-vertex v adjv v-adjvs)
               (add-vertex adjv nil v-adjvs)
               (format *error-output* "~A# of lines read: ~A... " #\Return lr)
               (incf lr)))
           :finally
           (format *error-output* "Done.~%~A lines read; start writing... " lr)
           (force-output *error-output*)))
      (with-open-file (out-stream dst
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
        (let ((*standard-output* out-stream))
          (maphash (lambda (v adjv)
                     (write-vertex v adjv))
                   v-adjvs)))))
  (format *error-output* "Done.~%"))

(declaim (ftype (function (list) null) main))
(defun main (args)
  "The executable image's toplevel function."
  (if (/= 3 (length args))
      (format t "Usage: ~A SOURCE DEST~%" (first args))
      (parse-graph-dataset (pathname (second args)) (pathname (third args)))))
