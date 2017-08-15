;;;; graph-dataset-parser.lisp

(in-package #:graph-dataset-parser)

;;; "graph-dataset-parser" goes here. Hacks and glory await!

(declaim (optimize (speed 3) (safety 0))
         (inline split-sequence:split-sequence))

(declaim (ftype (function (pathname pathname) null) parse-graph-dataset))
(defun parse-graph-dataset (src dst)
  "Parse a graph dataset file into a format that's suitable for
consumption by Pregel+/Palgol applications and write the result out to
a specified output file."
  (flet ((parse-line (line)
           (-<>> line
                 (split-sequence-if (lambda (c) (or (char= c #\Tab) (char= c #\space))))
                 (remove "" <> :test #'equal)
                 (mapcar (lambda (s) (parse-integer s)))))
         (write-vertex (vertex adjacent-vertices)
           (let ((adjvs-len (length adjacent-vertices)))
             (format *standard-output* "~A~A~A ~A" vertex #\Tab vertex adjvs-len)
             (if (= adjvs-len 0)
                 (format *standard-output* "~%")
                 (progn
                   (loop :for adjacent-vertex :across adjacent-vertices
                      :do (format *standard-output* " ~A ~A" adjacent-vertex vertex))
                   (format *standard-output* "~%")))))
         (add-unwritten-vertex-to-set (vid set)
           (when (null (gethash vid set))
             (setf (gethash vid set) :unwritten)))
         (add-written-vertex-to-set (vid set)
           (let ((value (gethash vid set)))
             (when (or (null value)
                       (eql value :unwritten))
               (setf (gethash vid set) :written)))))
    (declare (inline parse-line
                     write-vertex
                     add-unwritten-vertex-to-set
                     add-written-vertex-to-set))
    (with-open-file (in-stream src)
      (with-open-file (out-stream dst
                                  :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
        (let ((*standard-output* out-stream)
              (adjvs (make-array 1000 :element-type 'fixnum :fill-pointer 0))
              (chvs (make-hash-table)))
          (loop
             :with vertex fixnum := -1
             :for line :of-type (or simple-string null) := (read-line in-stream nil nil)
             :while line :do
             (when (and (char/= #\# (aref line 0)) (char/= #\% (aref line 0)))
               (multiple-value-bind (v adjv) (values-list (parse-line line))
                 (declare (fixnum v adjv))
                 (when (/= vertex v)
                   (when (/= (length adjvs) 0)
                     (write-vertex vertex adjvs)
                     (add-written-vertex-to-set vertex chvs))
                   (setf vertex v
                         (fill-pointer adjvs) 0))
                 (vector-push-extend adjv adjvs)
                 (add-unwritten-vertex-to-set adjv chvs)))
             :finally
             (write-vertex vertex adjvs)
             (add-written-vertex-to-set vertex chvs))
          ;; Next, we write the remaining unwritten leaf vertices in
          ;; the chvs set to the output file
          (when (< 0 (hash-table-count chvs))
            (maphash (lambda (v s)
                       (when (eql s :unwritten)
                         (write-vertex v #())))
                     chvs)))))))

(declaim (ftype (function (list) null) main))
(defun main (args)
  "The executable image's toplevel function."
  (if (/= 3 (length args))
      (format t "Usage: ~A SOURCE DEST~%" (first args))
      (parse-graph-dataset (pathname (second args)) (pathname (third args)))))
