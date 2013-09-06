HOCKING-svm-compare.pdf: HOCKING-svm-compare.tex refs.bib
	pdflatex HOCKING-svm-compare
	bibtex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
