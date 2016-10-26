;;;; graph-dataset-parser.lisp

(in-package #:graph-dataset-parser)

;;; "dataset-parser" goes here. Hacks and glory await!

(declaim (optimize (speed 3) (safety 0))
         (inline parse-user-and-follower
                 write-user-and-follwers
                 split-sequence:split-sequence))

(defun parse-user-and-follower (line)
  (declare (type (simple-array character) line))
  "Given a line from a graph, parse and return a list containing two
elements: a user ID and a follower ID."
  (mapcar #'parse-integer (split-sequence:split-sequence #\Tab line)))

(defun write-user-and-follwers (user-id follower-ids destination)
  "Given a user ID and a list of follower IDs, write to the
destination stream in a format that's suitable for consumption by
Pregel+ applications."
  (declare (type fixnum user-id)
           (type list follower-ids)
           (type stream destination))
  (format destination "~A~A~A ~{~A~^ ~}~%" user-id #\Tab (length follower-ids) follower-ids))

(defun parse-graph-dataset (source-file output-file)
  "Convert a source file into a format that's suitable for consumption
by Pregel+ applications and write the result out to a specified output
file."
  (declare (type string source-file output-file))
  (with-open-file (input-s source-file)
    (with-open-file (output-s output-file
                              :direction :output
                              :if-exists :overwrite :if-does-not-exist :create)
      (loop
         :with user-id fixnum := 0
         :and follower-ids list := nil
         :for line := (read-line input-s nil nil) :while line :do
         (multiple-value-bind (uid fid) (values-list (parse-user-and-follower line))
           (declare (type fixnum uid fid))
           (when (/= user-id uid)
             (when follower-ids (write-user-and-follwers user-id follower-ids output-s))
             (setf user-id uid
                   follower-ids nil))
           (push fid follower-ids))
         :finally (write-user-and-follwers user-id follower-ids output-s)))))

(defun main (args)
  "The executable image's toplevel function."
  (declare (type list args))
  (if (/= 3 (length args))
      (format t "Usage: ~A SOURCE-FILE OUTPUT-FILE~%" (first args))
      (parse-graph-dataset (second args) (third args))))
