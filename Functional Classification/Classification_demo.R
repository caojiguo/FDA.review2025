##################################################################################
# Demo of Functional Data Classification: Applying the Functional Neural Network 
# Classifier Developed by Thind et al. (2020a) to the Phoneme Dataset
##################################################################################

# Libraries
library(fda)
library(fda.usc)
library(keras)
library(ggplot2)
library(refund)
library(modEvA)
library(future.apply)
library(caret)
library(randomForest)
library(e1071)
library(gbm)
library(stringr)
library(future)
source("FNN_FunctionsFile.R")

# Clearing backend
K <- backend()
K$clear_session()
options(warn=-1)

# Setting seeds
set.seed(1919)
set_random_seed(1919)

# Loading data
data(phoneme)

# Combining data
full_resp = as.numeric(as.vector(c(phoneme$classlearn, phoneme$classtest))) - 1
full_df = rbind(phoneme$learn$data, phoneme$test$data)

# Making classification bins
resp = full_resp

# define the time points on which the functional predictor is observed. 
timepts = seq(1, 150, 1)

# define the fourier basis 
nbasis = 65
spline_basis = create.fourier.basis(c(min(timepts), max(timepts)), nbasis)

# convert the functional predictor into a fda object
fd =  Data2fd(timepts, t(full_df), spline_basis)
deriv1 = deriv.fd(fd)
deriv2 = deriv.fd(deriv1)

# Setting up arrays
func_cov_1 = fd$coefs
func_cov_2 = deriv1$coefs
func_cov_3 = deriv2$coefs
final_data = array(dim = c(nbasis, nrow(full_df), 3))
final_data[,,1] = func_cov_1
final_data[,,2] = func_cov_2
final_data[,,3] = func_cov_3

# fData Object
fdata_obj = fdata(full_df, argvals = timepts, rangeval = c(min(timepts), max(timepts)))

# Choosing fold number
num_folds = 10

# Creating folds
fold_ind = createFolds(resp, k = num_folds)

# number of models
num_models = 7

# number of measures
num_measures = 5

# Initializing matrices for results
error_mat_flm = matrix(nrow = num_folds, ncol = num_measures)
error_mat_pc = matrix(nrow = num_folds, ncol = num_measures)
error_mat_pls = matrix(nrow = num_folds, ncol = num_measures)
error_mat_np = matrix(nrow = num_folds, ncol = num_measures)
error_mat_fnn = matrix(nrow = num_folds, ncol = num_measures)
error_mat_svm = matrix(nrow = num_folds, ncol = num_measures)
error_mat_nn = matrix(nrow = num_folds, ncol = num_measures)

# Doing pre-processing of neural networks
if(dim(final_data)[3] > 1){
  # Now, let's pre-process
  pre_dat = FNN_Preprocess(func_cov = final_data,
                           basis_choice = c("fourier", "fourier", "fourier"),
                           num_basis = c(5, 7, 9),
                           domain_range = list(c(min(timepts), max(timepts)), 
                                               c(min(timepts), max(timepts)), 
                                               c(min(timepts), max(timepts))),
                           covariate_scaling = T,
                           raw_data = F)
  
} else {
  
  # Now, let's pre-process
  pre_dat = FNN_Preprocess(func_cov = final_data,
                           basis_choice = c("fourier"),
                           num_basis = c(21),
                           domain_range = list(c(min(timepts), max(timepts))),
                           covariate_scaling = T,
                           raw_data = F)
}

# Functional weights
func_weights1 = matrix(nrow = num_folds, ncol = 5)
func_weights2 = matrix(nrow = num_folds, ncol = 7)
func_weights3 = matrix(nrow = num_folds, ncol = 9)

# Looping to get results
for (i in 1:num_folds) {
  
  ################## 
  # Splitting data #
  ##################
  
  # Test and train
  train_x = fdata_obj[-fold_ind[[i]],]
  test_x = fdata_obj[fold_ind[[i]],]
  train_y = resp[-fold_ind[[i]]]
  test_y = resp[fold_ind[[i]]]
  
  # Setting up for FNN
  pre_train = pre_dat$data[-fold_ind[[i]], ]
  pre_test = pre_dat$data[fold_ind[[i]], ]

  ###########################################
  # Running Functional Linear Model (Basis) #
  ###########################################
  
  l=2^(-2:8)
  func_basis = fregre.basis.cv(train_x, train_y, type.basis = "fourier",
                               lambda=l, type.CV = GCV.S, par.CV = list(trim=0.15))
  pred_basis = round(predict(func_basis[[1]], test_x))
  final_pred_basis = ifelse(pred_basis < min(test_y), min(test_y), ifelse(pred_basis > max(test_y), max(test_y), pred_basis))
  confusion_flm = confusionMatrix(as.factor(final_pred_basis), as.factor(test_y))
  
  #################################################################
  # Runnig Functional Principal Component Regression (No Penalty) #
  #################################################################
  
  func_pc = fregre.pc.cv(train_x, train_y, 8)
  pred_pc = round(predict(func_pc$fregre.pc, test_x))
  final_pred_pc = ifelse(pred_pc < min(test_y), min(test_y), ifelse(pred_pc > max(test_y), max(test_y), pred_pc))
  confusion_fpc = confusionMatrix(as.factor(final_pred_pc), as.factor(test_y))
  
  ####################################################################
  # Running Functional Partial Least Squares Regression (No Penalty) #
  ####################################################################
  
  func_pls = fregre.pls(train_x, train_y, 1:4)
  pred_pls = round(predict(func_pls, test_x))
  final_pred_pls = ifelse(pred_pls < min(test_y), min(test_y), ifelse(pred_pls > max(test_y), max(test_y), pred_pls))
  confusion_pls = confusionMatrix(as.factor(final_pred_pls), as.factor(test_y))
  
  ################################################
  # Running Functional Non-Parametric Regression #
  ################################################
  
  func_np = fregre.np(train_x, train_y, Ker = AKer.tri, metric = semimetric.deriv)
  pred_np = round(predict(func_np, test_x))
  final_pred_np = ifelse(pred_np < min(test_y), min(test_y), ifelse(pred_np > max(test_y), max(test_y), pred_np))
  confusion_np = confusionMatrix(as.factor(final_pred_np), as.factor(test_y))
  
  ###############
  # Running svm #
  ###############
  
  # Setting up MV data
  MV_train = as.data.frame(full_df[-fold_ind[[i]],])
  MV_test = as.data.frame(full_df[fold_ind[[i]],])
  colnames(MV_train) = paste0("v", gsub(" ", "_", colnames(MV_train)))
  colnames(MV_test) = paste0("v", gsub(" ", "_", colnames(MV_test)))
  train_y = resp[-fold_ind[[i]]]
  test_y = resp[fold_ind[[i]]]
  
  fit_svm = svm.model <- svm(as.factor(train_y) ~ ., data = MV_train)
  svm_pred = predict(fit_svm, newdata = MV_test, type = "response")
  confusion_svm = confusionMatrix(svm_pred, as.factor(test_y))
  
  ##############
  # Running NN #
  ##############
  
  # Setting seeds
  set.seed(i)
  set_random_seed(i)

  # Setting up FNN model
  model_nn <- keras_model_sequential()
  model_nn %>%
    layer_dense(units = 64, activation = 'relu') %>%
    layer_dense(units = 32, activation = 'relu') %>%
    layer_dense(units = 16, activation = 'relu') %>%
    layer_dense(units = length(unique(resp)), activation = 'softmax')

  # Setting parameters for FNN model
  model_nn %>% compile(
    optimizer = optimizer_adam(lr = 0.001),
    loss = 'sparse_categorical_crossentropy',
    metrics = c('accuracy')
  )

  # Early stopping
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = 15)

  # Training FNN model
  model_nn %>% fit(as.matrix(MV_train),
                   train_y,
                   epochs = 150,
                   validation_split = 0.2,
                   callbacks = list(early_stop),
                   verbose = 0)

  # Predictions
  test_predictions <- model_nn %>% predict(as.matrix(MV_test))
  preds = apply(test_predictions, 1, function(x){return(which.max(x))}) - 1

  # Plotting
  confusion_nn = confusionMatrix(as.factor(preds), as.factor(test_y))

  
  #####################################
  # Running Functional Neural Network #
  #####################################
  
  # Setting seeds
  set.seed(i)
  set_random_seed(i)

  # Setting up FNN model
  model_fnn <- keras_model_sequential()
  model_fnn %>% 
    layer_dense(units = 128,
                activation = "relu") %>%
    layer_dense(units = 64,
                activation = "relu") %>%
    layer_dropout(0.4) %>%
    layer_dense(units = 128,
                activation = "sigmoid") %>%
    layer_dense(units = length(unique(resp)), activation = 'softmax')
  
  
  # Setting parameters for FNN model
  model_fnn %>% compile(
    optimizer = optimizer_adam(lr = 5e-03), 
    loss = 'sparse_categorical_crossentropy',
    metrics = c('accuracy')
  )
  
  # Early stopping
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = 15)
  
  # Training FNN model
  model_fnn %>% fit(pre_train, 
                    train_y, 
                    epochs = 300,  
                    validation_split = 0.2,
                    callbacks = list(early_stop),
                    verbose = 0)
  
  # Predictions
  test_predictions <- model_fnn %>% predict(pre_test)
  preds_fnn = apply(test_predictions, 1, function(x){return(which.max(x))}) - 1
  
  # Plotting
  confusion_fnn = confusionMatrix(as.factor(preds_fnn), as.factor(test_y))
  
  # Storing weights
  func_weights1[i,] = rowMeans(get_weights(model_fnn)[[1]])[1:5]
  func_weights2[i,] = rowMeans(get_weights(model_fnn)[[1]])[6:12]
  func_weights3[i,] = rowMeans(get_weights(model_fnn)[[1]])[13:21]

  ###################
  # Storing Results #
  ###################
  
  error_mat_flm[i, ] = c(confusion_flm$overall[1], 
                         mean(confusion_flm$byClass[,1], na.rm = T),
                         mean(confusion_flm$byClass[,2], na.rm = T), 
                         mean(confusion_flm$byClass[,3], na.rm = T),
                         mean(confusion_flm$byClass[,4], na.rm = T))
  error_mat_pc[i, ] = c(confusion_fpc$overall[1], 
                        mean(confusion_fpc$byClass[,1], na.rm = T),
                        mean(confusion_fpc$byClass[,2], na.rm = T), 
                        mean(confusion_fpc$byClass[,3], na.rm = T),
                        mean(confusion_fpc$byClass[,4], na.rm = T))
  error_mat_pls[i, ] = c(confusion_pls$overall[1], 
                         mean(confusion_pls$byClass[,1], na.rm = T),
                         mean(confusion_pls$byClass[,2], na.rm = T), 
                         mean(confusion_pls$byClass[,3], na.rm = T),
                         mean(confusion_pls$byClass[,4], na.rm = T))
  error_mat_np[i, ] = c(confusion_np$overall[1], 
                        mean(confusion_np$byClass[,1], na.rm = T),
                        mean(confusion_np$byClass[,2], na.rm = T), 
                        mean(confusion_np$byClass[,3], na.rm = T),
                        mean(confusion_np$byClass[,4], na.rm = T))
  error_mat_fnn[i, ] = c(confusion_fnn$overall[1], 
                         mean(confusion_fnn$byClass[,1], na.rm = T),
                         mean(confusion_fnn$byClass[,2], na.rm = T), 
                         mean(confusion_fnn$byClass[,3], na.rm = T),
                         mean(confusion_fnn$byClass[,4], na.rm = T))
  error_mat_svm[i, ] = c(confusion_svm$overall[1], 
                         mean(confusion_svm$byClass[,1], na.rm = T),
                         mean(confusion_svm$byClass[,2], na.rm = T), 
                         mean(confusion_svm$byClass[,3], na.rm = T),
                         mean(confusion_svm$byClass[,4], na.rm = T))
  error_mat_nn[i, ] = c(confusion_nn$overall[1], 
                        mean(confusion_nn$byClass[,1], na.rm = T),
                        mean(confusion_nn$byClass[,2], na.rm = T), 
                        mean(confusion_nn$byClass[,3], na.rm = T),
                        mean(confusion_nn$byClass[,4], na.rm = T))
  
  # Resetting things
  K <- backend()
  K$clear_session()
  options(warn=-1)
  
  # Printing iteration number
  print(paste0("Done Iteration: ", i))
  
}

# Initializing final table: average of errors
Accuracy_Table = matrix(nrow = num_folds, ncol = num_models)
Final_Table = matrix(nrow = num_models, ncol = num_measures + 1)

# Collecting errors
Accuracy_Table[,1] = error_mat_flm[,1]
Accuracy_Table[,2] = error_mat_np[,1]
Accuracy_Table[,3] = error_mat_pc[,1]
Accuracy_Table[,4] = error_mat_pls[,1]
Accuracy_Table[,5] = error_mat_svm[,1]
Accuracy_Table[,6] = error_mat_nn[,1]
Accuracy_Table[,7] = error_mat_fnn[,1]

Final_Table[1, ] = c(colMeans(error_mat_flm, na.rm = T), sd(error_mat_flm[,1]))
Final_Table[2, ] = c(colMeans(error_mat_np, na.rm = T), sd(error_mat_np[,1]))
Final_Table[3, ] = c(colMeans(error_mat_pc, na.rm = T), sd(error_mat_pc[,1]))
Final_Table[4, ] = c(colMeans(error_mat_pls, na.rm = T), sd(error_mat_pls[,1]))
Final_Table[5, ] = c(colMeans(error_mat_svm, na.rm = T), sd(error_mat_svm[,1]))
Final_Table[6, ] = c(colMeans(error_mat_nn, na.rm = T), sd(error_mat_nn[,1]))
Final_Table[7, ] = c(colMeans(error_mat_fnn, na.rm = T), sd(error_mat_fnn[,1]))


# Editing names
rownames(Final_Table) = colnames(Accuracy_Table) = 
  c("FLM", "FNP", "FPC", "FPLS","SVM", "NN", "FNN")
colnames(Final_Table) = c("Accuracy", "Sensitivity", "Specificity", "PPV", "NPV Rate", "SD_Error")

# Looking at results
Final_Table
Accuracy_Table

# Plotting
library(ggplot2)
library(reshape2)  # For melt function

accuracy_df <- melt(Accuracy_Table[, c("FLM", "FNP", "FPC",  "FPLS", "SVM", "NN", "FNN")])
colnames(accuracy_df) <- c("Iteration", "Method","Accuracy")
# Create boxplot with mean values
ggplot(accuracy_df, aes(x=Method, y=Accuracy, fill=Method)) +
  geom_boxplot() +
  stat_summary(
    fun = mean,  # Compute mean
    geom = "point",  # Display as a point
    shape = 18,  # Diamond shape (optional)
    size = 3,  # Size of the mean marker
    color = "red",  # Color of the mean marker
    show.legend = FALSE  # Hide from legend
  ) +
  geom_text(
    data = aggregate(Accuracy ~ Method, accuracy_df, mean),  # Compute mean per method
    aes(label = sprintf("%.3f", Accuracy), y = Accuracy + 0.1), 
    color = "black",
    size = 3,
    fontface = "bold"
  ) +
  # geom_jitter(width=0.2, alpha=0.3, size=1.5) +  # Add individual points
  # scale_y_continuous(limits=c(0,1), breaks=seq(0,1,0.1)) +  # Set y-axis for accuracy
  labs(#title="Classification Accuracy by Method",
       x="Method",
       y="Classification Accuracy") +
  theme_minimal() +
  theme(legend.position="none",  # Remove legend
        axis.text.x=element_text(angle=45, hjust=1),
        plot.title = element_text(hjust = 0.5))  # Rotate x labels

