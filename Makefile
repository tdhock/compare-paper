HOCKING-svm-compare.pdf: HOCKING-svm-compare.tex refs.bib figure-norm-data.tex figure-hard-margin.tex aistats2014.sty figure-simulation-sushi-samples.tex figure-norm-level-curves.tex figure-auc.tex
	rm -f *.aux *.bbl
	pdflatex HOCKING-svm-compare
	bibtex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
figure-norm-data.tex: figure-norm-data.R tikz.R colors.R
	R --vanilla < $<
figure-hard-margin.tex: figure-hard-margin.R tikz.R colors.R
	R --vanilla < $<
simulation.sushi.samples.RData: simulation.sushi.samples.R sushi.pairs.RData
	R --vanilla < $<
figure-simulation-samples.tex: figure-simulation-samples.R simulation.samples.RData tikz.R Nsamp.R colors.R sushi.samples.RData
	R --vanilla < $<
figure-simulation-sushi-samples.tex: figure-simulation-sushi-samples.R simulation.sushi.samples.RData
	R --vanilla < $<
figure-norm-level-curves.tex: figure-norm-level-curves.R tikz.R simulation.samples.RData Nsamp.R colors.R
	R --vanilla < $<
simulation.RData: simulation.R svmlight.R
	R --vanilla < $<
simulation.proportion.RData: simulation.proportion.R calc.roc.R svmlight.R
	R --vanilla < $<
figure-simulation-proportion.tex: figure-simulation-proportion.R colors.R simulation.proportion.RData
	R --vanilla < $<
simulation.roc.RData: simulation.roc.R simulation.proportion.RData
	R --vanilla < $<
figure-auc.tex: figure-auc.R simulation.proportion.RData tikz.R colors.R sushi.proportion.RData
	R --vanilla < $<
mslr.roc.RData: mslr.roc.R mslr.proportion.RData
	R --vanilla < $<

# MSLR-WEB10K should contain Fold1, Fold2, ...
~/MSLR-WEB10K/folds.RData: folds.R
	R --vanilla < $<
mslr.queries.RData: mslr.queries.R ~/MSLR-WEB10K/folds.RData
	R --vanilla < $<
small.folds.RData: small.folds.R mslr.queries.RData
	R --vanilla < $<
mslr.proportion.RData: mslr.proportion.R svmlight.R small.folds.RData
	R --vanilla < $<
mslr.samples.RData: mslr.samples.R svmlight.R small.folds.RData
	R --vanilla < $<
# sushi data
sushi.RData: sushi.R
	R --vanilla < $<
prefectureList.RData: prefectureList.R sushi.RData
	R --vanilla < $<
sushi.features.RData: sushi.features.R prefectureList.RData sushi.RData
	R --vanilla < $<
sushi.pairs.RData: sushi.pairs.R sushi.features.RData sushi.RData
	R --vanilla < $<
sushi.proportion.RData: sushi.proportion.R sushi.pairs.RData svmlight.R
	R --vanilla < $<
sushi.roc.RData: sushi.roc.R sushi.proportion.RData
	R --vanilla < $<
sushi.samples.RData: sushi.samples.R svmlight.R sushi.pairs.RData
	R --vanilla < $<
