files = \
  Main.agda-lib \
  TwoThreeTree.agda \
  LLRBTreeBalInsert.agda \
  Everything.agda \
  TwoThree.hs

start: live-start.zip

%.zip : $(files) Makefile html
	zip $@  $(files) Makefile html/ html/*.html

html : $(files)
	agda --html Everything.agda

# EOF
