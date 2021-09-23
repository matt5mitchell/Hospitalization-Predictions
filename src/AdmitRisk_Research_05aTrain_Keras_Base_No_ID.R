##############
## RESEARCH ##
##############
## Evaluate effect of subgroups on risk prediction
## Adaptation of Admit Risk Refresh from fall 2017

## Base model WITHOUT subject-specific treatment (patient ID as input)

###########
## Setup ##
###########

#Libraries
if(!require("tidyverse")){
  install.packages("tidyverse")
}
if(!require("onehot")){
  install.packages("onehot")
}
if(!require("keras")){
  install.packages("keras")
}
if(!require("pROC")){
  install.packages("pROC")
}
if(!require("purrr")){
  install.packages("purrr")
}
if(!require("readr")){
  install.packages("readr")
}

library(tidyverse)
library(keras) #neural net
library(pROC) #auc
library(purrr) #map functions
library(readr)

#Tune model hyperparameters
train_list <- tune_data
test_list <- val_data

# #Train final model
# train_list <- train_data
# test_list <- test_data


#################################
## Neural Net function (keras) ##
#################################

nn_fit <- function(units_gru1, units_gru2, units_d1, units_d2, dropout, l2, lr, epoch, train_list, test_list, ...) {
  
  gru_input_shape <- as.numeric(dim(test_list[[1]])[2:3]) #3D input
  dense_input_shape <- as.numeric(dim(test_list[[2]])[2]) #2D input
  output_shape <- if (is.atomic(test_list[[4]])) { 1 } else {as.numeric(dim(test_list[[4]])[2])} #Test 1D vs 2D
  output_aux_shape <- as.numeric(dim(test_list[[5]])[2]) #2D input
  
  # Clearing the session after the computations have finished
  # clears memory used by the last trial in preparation for 
  # the next iteration. 
  on.exit(keras::backend()$clear_session())
  
  # Set the seed to get reproducible starting values and dropouts
  set.seed(2971)
  
  # Define model
  gru_input <- layer_input(shape = gru_input_shape,
                           name = 'gru_input')
  dense_input <- layer_input(shape = dense_input_shape,
                             name = 'dense_input')
  
  gru_out <- gru_input %>%
    layer_gru(
      units = units_gru1,
      activation = 'tanh',
      recurrent_activation = 'hard_sigmoid',
      return_sequences = TRUE,
      recurrent_dropout = dropout #https://arxiv.org/abs/1603.05118
    ) %>%
    layer_gru(
      units = units_gru2,
      activation = 'tanh',
      recurrent_activation = 'hard_sigmoid',
      recurrent_dropout = dropout
    )
  
  aux_output <- gru_out %>%
    layer_dense(
      units = output_aux_shape,
      name = 'aux_output'
    )
  
  dense_1_out <- layer_concatenate(c(gru_out, dense_input)) %>%
    layer_dropout(rate = dropout) %>%
    layer_dense(
      units = units_d1, 
      activation = 'relu', 
      kernel_regularizer = regularizer_l2(l = l2)
    )
  
  main_output <- dense_1_out %>%
    layer_dropout(rate = dropout) %>%
    layer_dense(
      units = units_d2, 
      activation = 'relu', 
      kernel_regularizer = regularizer_l2(l = l2)
    ) %>%
    layer_dropout(rate = dropout) %>% 
    layer_dense(units = output_shape, 
                activation = 'sigmoid',
                name = 'main_output')
  
  #Define multi-input multi-output model
  model <- keras_model(
    inputs=list(gru_input, dense_input),
    outputs=list(main_output, aux_output))
  
  #Complie model
  model %>% compile(
    loss = list(main_output = 'binary_crossentropy', aux_output = 'mean_squared_error'),
    loss_weights = list(main_output = 1.0, aux_output = 0.2),
    optimizer = optimizer_adam(lr=lr),
    metrics = list(main_output = 'binary_accuracy', aux_output = 'mean_absolute_error')
  )
  
  #Fit the model
  model %>% fit(
    x = list(gru_input = train_list[[1]], dense_input = train_list[[2]]), 
    y = list(main_output = train_list[[4]], aux_output = train_list[[5]]),
    validation_data = list(list(gru_input = test_list[[1]], dense_input = test_list[[2]]),
                           list(main_output = test_list[[4]], aux_output = test_list[[5]])),
    epochs = epoch,
    batch_size = 128, 
    verbose = 0
  )
  
  #Loss and accuracy with held out test data
  eval <- model %>% evaluate(list(gru_input = test_list[[1]], dense_input = test_list[[2]]),
                             list(main_output = test_list[[4]], aux_output = test_list[[5]]))
  
  #Predictions with held out test data
  pred <- predict(model, list(gru_input = test_list[[1]], dense_input = test_list[[2]]))[[2]]
  
  data.frame(tot_loss=eval$loss, #loss optimized by the model
             main_loss=eval$main_output_loss, #loss for main output
             aux_loss=eval$aux_output_loss, #loss for aux output
             main_acc=eval$main_output_binary_accuracy, #accuracy for main output
             aux_mae=eval$aux_output_mean_absolute_error, #accuracy for aux output
             auc=roc(test_list[[4]], pred[,1])$auc, #AUC for evaluating performance
             mean=mean(pred[,1]), #reality check on predictions
             median=median(pred[,1]), #reality check on predictions
             max=max(pred[,1])) #reality check on predictions
}


####################################
## Train split across tuning grid ##
####################################

across_grid <- function(train_list, test_list) {
  #Create list of tuning values for pmap_dbl()
  
  # tg <- expand.grid(units_gru1=c(4, 8, 16),
  #                   units_gru2=c(4, 8),
  #                   units_d1=c(8, 16, 32),
  #                   units_d2=c(4, 8, 16, 32),
  #                   dropout=.5,
  #                   l2=c(.0001, .001),
  #                   lr=c(.0001, .001),
  #                   epoch=100) %>%
  #   filter(units_gru1 >= units_gru2,
  #          units_d1 >= units_d2) #second layer no larger than first
  
  tg <- expand.grid(units_gru1=4,
                    units_gru2=4,
                    units_d1=8,
                    units_d2=4,
                    dropout=.5,
                    l2=.001,
                    lr=.001,
                    epoch=20)
  
  tg_values <- as.list(tg)
  
  # Execute grid for this resample
  train_results <- pmap_df(
    tg_values, 
    nn_fit,
    train_list=train_list,
    test_list=test_list
  )
  # Now attach the resample indicators using `labels`
  cbind(tg_values, train_results)
}


################
## Fit models ##
################

t0 <- Sys.time()


## Train the model across tuning grid ##
tune_results <- across_grid(train_list, test_list)


t1 <- Sys.time()

difftime(t1, t0, units="mins")

#Show results
View(tune_results)

#Save results
write_csv(tune_results, path=paste0("tune_results_", Sys.Date(), ".csv"))

