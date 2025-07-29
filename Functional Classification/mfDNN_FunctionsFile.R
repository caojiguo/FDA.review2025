## Note: This is a modified version of the helper functions file, based on the original R script 'dnn_1d.R', 
## which is publicly available on GitHub at https://github.com/FDASTATAUBURN/mfdnn.

#################################
# mFDNN Classification Paper    #
#                               #
# Helper Functions File         # 
#################################


###one-dimensional FDNN classification
#################################################
##########Fourier basis function#################
#################################################
Fourier=function(s, M, j){
  k=j %/% 2
  
  if(j==1){
    return(rep(1,M))
  }else if(j %% 2 == 0){
    return(sqrt(2/M)*cos(2*pi*k*s))
  }else if(j %% 2 != 0){
    return(sqrt(2/M)*sin(2*pi*k*s))
  }
}

##input
#S: a vector of all grid points with length M
#J: number of truncated eigenvalues
#x.train: training data - a matrix of dimensions n_train × M, where n_train is the number of training samples
#D.test: testing data - a matrix of dimensions n_test × M, where n_test is the number of testing samples
#L: length of the DNN
#p: width of the DNN
#s: dropout rate
#epoch: epoch number
#batch: batch size
##return
#error: misclassification rate of the testing set

mfdnn.1d=function(x.train, x.test, y.train, y.test, J, S, L, p, s, epoch, batch){
  K = length(unique(c(y.train, y.test)))
  M=length(S)
  
  phi=c()
  for(j in 1:J){
    phi=cbind(phi, Fourier(S,M,j))
  }
  
  x_train=(x.train/M) %*% phi
  x_test=(x.test/M) %*% phi
  
  y_train=keras::to_categorical(matrix(y.train))
  y_test=keras::to_categorical(matrix(y.test))
  
  
  model=keras::keras_model_sequential()
  model %>% keras::layer_dense(units=p, activation = "relu", input_shape = c(J), kernel_initializer = "normal", constraint_maxnorm(max_value = 1, axis = 0))%>% 
    layer_dropout(rate = s)
  for(xx in 1:L){
    model %>% keras::layer_dense(units=p, activation = "relu", kernel_initializer = "normal", constraint_maxnorm(max_value = 1, axis = 0))%>% 
      layer_dropout(rate = s)
  }
  model %>% keras::layer_dense(units =K, activation = "softmax")  
  
  model %>% keras::compile(
    loss="categorical_crossentropy",
    optimizer=optimizer_adam(),
    metrics=c('accuracy')
  )
  
  
  history = model %>% keras::fit(
    x_train, y_train,
    epochs=epoch, batch_size=batch,
    verbose = 0
  )
  
  y.prob = model %>% predict(x_test)
  y.pred=model %>% predict(x_test) %>% k_argmax()
  
  scores <- model %>% evaluate(x_test, y_test)
  
  E=1-scores[2]
  
  attributes(E)=NULL
  
  
  list(error=E, y.pred = y.pred, y.prob = y.prob)
}

# 
# ################
# # Running mFDNN
# ###############
# 
# # Setting up for FDNN
# x.train = full_df[-fold_ind[[i]],]
# x.test = full_df[fold_ind[[i]],]
# train_y = resp[-fold_ind[[i]]]
# test_y = resp[fold_ind[[i]]]
# 
# J=10; L=3; p=300; s=0.1
# #fit mfdnn model
# r1=mfdnn.1d(x.train.mfdnn, x.test.mfdnn, train_y, test_y, J, S= timepts, L, p, s, epoch=200, batch=20)
# preds_mfdnn = apply(r1$y.prob, 1, function(x){return(which.max(x))}) - 1
# 
# # Plotting
# confusion_mfdnn = confusionMatrix(as.factor(preds_mfdnn), as.factor(test_y))
# confusion_mfdnn$overall
