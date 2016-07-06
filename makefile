# Makefile for the two latex-based figures

all: figures/figure2.jpeg figures/figure4.jpeg

figures/figure2.jpeg: tex/figure2.tex
	pdflatex tex/figure2.tex
	convert -density 400 figure2.pdf -quality 75 figures/figure2.jpeg
	rm figure2.*
	
figures/figure4.jpeg: tex/figure4.tex
	pdflatex tex/figure4.tex
	convert -density 400 figure4.pdf -quality 75 figures/figure4.jpeg
	rm figure4.*