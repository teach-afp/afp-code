destdir = $(HOME)/public_html/AFP-2024

files = \
  Prelude.agda \
  TreeSortOrd.agda \
  Everything.agda \
  InsertionSort.hs

html : $(files)
	agda --html --allow-unsolved-metas Everything.agda

pack : AFP-2024-Agda.zip

ship : html AFP-2024-Agda.zip
	mkdir -p $(destdir)
	cp -pr $^ $(destdir)

AFP-2024-Agda.zip : $(files) Makefile html
	zip $@  $(files) Makefile html/ html/*.html

# EOF
