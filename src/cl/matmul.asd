(asdf:defsystem "matmul"
  :components ((:file "matmul"))
  :build-operation "program-op"
  :build-pathname "matmul"
  :entry-point "MATMUL:MAIN")
