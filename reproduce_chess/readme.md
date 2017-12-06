# For Reproduction of Chess Data Results

## 1. Loading Chess datasets from Chessmetrics

-Modify load.R, with correct date ranges

Example:
```R
train <- chess[chess$Month > 48.5 & chess$Month < 51.5,]

trainM <- train$Month
test <- chess[chess$Month > 51.5 & chess$Month < 60.5,]
```

This would represent all matches player in year 5 with the train set containing 3 months and the test set containing 9 months

You also must modify the last line.
```R
save.image("5_data.RData")
```
This will allow you to label your RData files containing the 12 month periods correctly.

## 2.  Run hyperparameter selection and test AUC calculation

3 Files exist for each kernal: CV_polydot.R, CV_vanilla.R and CV_rbf.R

You must pass an argument containing the name of the RData file generated from load.R

Example

R CMD BATCH -1_data.RData CV_polydot.R

This will generate a object in the form below, where the AUC value in this object contains the test AUC and FINAL_AUC$ROC contains the ROC curve. kern.params contains the optimal hyperparameters.

1_data.RData_polydot_AUC_aftrs

## 3. Run hyperparameter selection and test AUC calculation while only considering ELO and Glicko score features

3 Files exist for each kernal: CV_polydot_2.R, CV_vanilla_2.R and CV_rbf_2.R

You must pass an argument containing the name of the RData file generated from load.R

Example

R CMD BATCH -1_data.RData CV_polydot_2.R

This will generate a object in the form below, where the AUC value in	this object contains the test AUC and FINAL_AUC$ROC contains the ROC curve. kern.params contains the optimal hyperparameters.

1_data.RData_polydot_AUC_2ftrs

## 4. Generate ELO scores

In calc_ELO_AUC.R

Change this to select the correct data file
```R
load("2_data.RData")
```
Run calc_ELO_AUC.R

This will produce a RData object in the format

1_ELO.RData

Where the AUC object in this object contains the test AUC

## 5. Generate Glicko scores

In calc_Glicko_AUC.R

Change this to select the correct data file
```R
load("2_data.RData")
```
Run calc_Glicko_AUC.R

This will produce a RData object in the format

1_GLIKO.RData

Where the AUC object in this object contains the test AUC
