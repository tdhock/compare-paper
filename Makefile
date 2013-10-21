HOCKING-svm-compare.pdf: HOCKING-svm-compare.tex refs.bib figure-norm-data.tex figure-hard-margin.tex aistats2014.sty figure-simulation-samples.tex figure-norm-level-curves.tex figure-simulation-proportion.tex
	rm -f *.aux *.bbl
	pdflatex HOCKING-svm-compare
	bibtex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
figure-norm-data.tex: figure-norm-data.R tikz.R colors.R
	R --no-save < $<
figure-hard-margin.tex: figure-hard-margin.R tikz.R colors.R
	R --no-save < $<
simulation.samples.RData: simulation.samples.R svmlight.R
	R --no-save < $<
figure-simulation-samples.tex: figure-simulation-samples.R simulation.samples.RData tikz.R Nsamp.R colors.R
	R --no-save < $<
figure-norm-level-curves.tex: figure-norm-level-curves.R tikz.R simulation.samples.RData Nsamp.R colors.R
	R --no-save < $<
simulation.RData: simulation.R svmlight.R
	R --no-save < $<
simulation.proportion.RData: simulation.proportion.R
	R --no-save < $<
figure-simulation-proportion.tex: figure-simulation-proportion.R colors.R
	R --no-save < $<

# MSLR-WEB10K should contain Fold1, Fold2, ...
~/MSLR-WEB10K/folds.RData: folds.R
	R --no-save < $<
mslr.queries.RData: mslr.queries.R ~/MSLR-WEB10K/folds.RData
	R --no-save < $<
small.folds.RData: small.folds.R mslr.queries.RData
	R --no-save < $<
mslr.proportion.RData: mslr.proportion.R svmlight.R small.folds.RData
	R --no-save < $<

