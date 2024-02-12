files = \
  Prelude.agda \
  TreeSortOrd.agda \
  Everything.agda \
  InsertionSort.hs

html : $(files)
	agda --html --allow-unsolved-metas Everything.agda

start: live-start.zip

%.zip : $(files) Makefile html
	zip $@  $(files) Makefile html/ html/*.html

# EOF
