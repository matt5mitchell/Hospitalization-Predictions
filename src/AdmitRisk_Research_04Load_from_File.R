##############
## RESEARCH ##
##############
## Evaluate effect of subgroups on risk prediction
## Adaptation of Admit Risk Refresh from fall 2017

if(!require("keras")){
  install.packages("keras")
}

library(keras)

#Load data
load(file="train_data.rda")
load(file="test_data.rda")
load(file="tune_data.rda")
load(file="val_data.rda")

#Load subgroup neural net
subgroup_model <- load_model_hdf5("subgroup_model.h5")

#Freeze weights for training hospitalization models
freeze_weights(subgroup_model)
