;;;; graph-dataset-parser.lisp

(in-package #:graph-dataset-parser)

;;; "graph-dataset-parser" goes here. Hacks and glory await!

(declaim (optimize (speed 3) (safety 0))
         (inline parse-vertex-and-adjacent-vertex
                 write-vertex-and-adjacent-vertices
                 split-sequence:split-sequence))

(defconstant +empty-list+ '())

(defparameter *default-vector-size* (expt 10 6))

(declaim (ftype (function (simple-string) list) parse-vertex-and-adjacent-vertex))
(defun parse-vertex-and-adjacent-vertex (line)
  "Given a line from a graph dataset, parse and return a list
containing two elements: a vertex label and its adjacent vertex
label."
  (mapcar (lambda (s) (parse-integer s :junk-allowed t))
          (split-sequence-if (lambda (c) (or (char= c #\Tab) (char= c #\space))) line)))

(declaim (ftype (function (fixnum (vector fixnum)) null) write-vertex-and-adjacent-vertices))
(defun write-vertex-and-adjacent-vertices (vertex adjacent-vertices)
  "Given a vertex and a vector containing adjacent vertices, write
them out to the *STANDARD-OUTPUT* in a format that's suitable for
consumption by Pregel+/Palgol applications."
  (format *standard-output* "~A~A~A ~A" vertex #\Tab vertex (length adjacent-vertices))
  (loop :for adjv fixnum :across adjacent-vertices :do
     (format *standard-output* " ~A ~A" adjv vertex))
  (format *standard-output* "~%"))

(declaim (ftype (function (pathname pathname) null) parse-graph-dataset))
(defun parse-graph-dataset (src dst)
  "Parse a graph dataset file into a format that's suitable for
consumption by Pregel+/Palgol applications and write the result out to
a specified output file."
  (with-open-file (in-stream src)
    (with-open-file (out-stream dst :direction :output
                                :if-exists :supersede :if-does-not-exist :create)
      (let ((*standard-output* out-stream))
        (loop
           :with vertex fixnum := -1
           :and adjacent-vertices :of-type (vector fixnum) := (make-array *default-vector-size*
                                                                          :element-type 'fixnum
                                                                          :fill-pointer 0)
           :for line :of-type (or simple-string null) := (read-line in-stream nil nil)
           :while line :do
           (when (and (char/= #\# (aref line 0)) (char/= #\% (aref line 0)))
              (multiple-value-bind (v adjv) (values-list (parse-vertex-and-adjacent-vertex line))
               (declare (fixnum v adjv))
               (when (/= vertex v)
                 (when (/= (length adjacent-vertices) 0)
                   (write-vertex-and-adjacent-vertices vertex adjacent-vertices))
                 (setf vertex v
                       (fill-pointer adjacent-vertices) 0))
               (vector-push adjv adjacent-vertices)))
           :finally (write-vertex-and-adjacent-vertices vertex adjacent-vertices))))))

(declaim (ftype (function (list) null) main))
(defun main (args)
  "The executable image's toplevel function."
  (if (/= 3 (length args))
      (format t "Usage: ~A SOURCE DEST~%" (first args))
      (parse-graph-dataset (pathname (second args)) (pathname (third args)))))
