HOCKING-svm-compare.pdf: HOCKING-svm-compare.tex refs.bib figure-norm-data.tex figure-hard-margin.tex aistats2014.sty figure-simulation-sushi-samples.tex figure-norm-level-curves.tex figure-auc.tex
	rm -f *.aux *.bbl
	pdflatex HOCKING-svm-compare
	bibtex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
	pdflatex HOCKING-svm-compare
figure-norm-data.tex: figure-norm-data.R tikz.R colors.R
	R --no-save < $<
figure-hard-margin.tex: figure-hard-margin.R tikz.R colors.R
	R --no-save < $<
simulation.sushi.samples.RData: simulation.sushi.samples.R sushi.pairs.RData
	R --no-save < $<
figure-simulation-samples.tex: figure-simulation-samples.R simulation.samples.RData tikz.R Nsamp.R colors.R sushi.samples.RData
	R --no-save < $<
figure-simulation-sushi-samples.tex: figure-simulation-sushi-samples.R simulation.sushi.samples.RData
	R --no-save < $<
figure-norm-level-curves.tex: figure-norm-level-curves.R tikz.R simulation.samples.RData Nsamp.R colors.R
	R --no-save < $<
simulation.RData: simulation.R svmlight.R
	R --no-save < $<
simulation.proportion.RData: simulation.proportion.R calc.roc.R svmlight.R
	R --no-save < $<
figure-simulation-proportion.tex: figure-simulation-proportion.R colors.R simulation.proportion.RData
	R --no-save < $<
simulation.roc.RData: simulation.roc.R simulation.proportion.RData
	R --no-save < $<
figure-auc.tex: figure-auc.R simulation.proportion.RData tikz.R colors.R sushi.proportion.RData
	R --no-save < $<
mslr.roc.RData: mslr.roc.R mslr.proportion.RData
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
mslr.samples.RData: mslr.samples.R svmlight.R small.folds.RData
	R --no-save < $<
# sushi data
sushi.RData: sushi.R
	R --no-save < $<
prefectureList.RData: prefectureList.R sushi.RData
	R --no-save < $<
sushi.features.RData: sushi.features.R prefectureList.RData sushi.RData
	R --no-save < $<
sushi.pairs.RData: sushi.pairs.R sushi.features.RData sushi.RData
	R --no-save < $<
sushi.proportion.RData: sushi.proportion.R sushi.pairs.RData svmlight.R
	R --no-save < $<
sushi.roc.RData: sushi.roc.R sushi.proportion.RData
	R --no-save < $<
sushi.samples.RData: sushi.samples.R svmlight.R sushi.pairs.RData
	R --no-save < $<
