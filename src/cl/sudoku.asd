(asdf:defsystem "sudoku"
  :components ((:file "sudoku"))
  :build-operation "program-op"
  :build-pathname "sudoku"
  :entry-point "SUDOKU:MAIN")
