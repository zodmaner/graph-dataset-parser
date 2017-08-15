;;;; graph-dataset-parser.asd

(asdf:defsystem #:graph-dataset-parser
  :description "A simple graph dataset parser"
  :author "Smith Dhumbumroong <zodmaner@gmail.com>"
  :license "Public Domain"
  :depends-on (#:split-sequence
               #:cl-arrows)
  :serial t
  :components ((:file "package")
               (:file "graph-dataset-parser")))

