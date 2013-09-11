HOCKING-svm-compare.pdf: HOCKING-svm-compare.tex refs.bib figure-norm-data.tex
	rm -f *.aux *.bbl
	pdflatex HOCKING-svm-compare
	bibtex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
figure-norm-data.tex: figure-norm-data.R tikz.R
	R --no-save < $<