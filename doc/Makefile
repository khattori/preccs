#
#  Makefile for TeX documents compiling
#      Kenta Hattori, Oct. 2 2006
#

# .tex.dvi:
# 	platex $<
# .dvi.pdf:
#	dvipdfm $<

TEX	= platex.sh
DVIPDFM	= dvipdfmx

TEXINPUTS = .;../texsty//;

tut/tut.pdf : tut/tut.dvi
	cd tut && ${DVIPDFM} tut

tut/tut.dvi : tut/tut.tex
	cd tut && ../${TEX} tut && ../${TEX} tut

clean	:
	rm tut/tut.dvi tut/tut.pdf