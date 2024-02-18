files = \
  InsertionSortBool.agda \
  LengthIsortRewrite.agda \
  LengthIsortChain.agda \
  Logic.agda \
  Le.agda \
  InsertionSortLe.agda \
  Everything.agda

std-lib = $(HOME)/agda-master/std-lib/src

html : $(files)
	agda --html -i. -i$(std-lib) Everything.agda

pack :
	zip AFP-Agda.zip $(files) Makefile html/ html/*.html

# EOF
