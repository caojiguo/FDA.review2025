# Call functions and load packages
source('FoS_Functions.R')

# packages = c("fda", "fdapace", "MASS", "refund", "tensorflow", "keras", "caret", "dplyr", "pbapply")
# install.packages(packages)
library(fda)
library(fdapace)
library(MASS)
library(refund)
library(tensorflow)
library(keras)
library(caret)
library(dplyr)
library(pbapply)

# Import Data Set
load("asfr.RData")
data.x = asfr_list$X[,-1]
data.y = asfr_list$Y

# Set up seeds for iterations/replicates
set.seed(kind = "Mersenne-Twister", seed = 78, normal.kind = "Inversion")
set_random_seed(91)
niter = 20
seeds = sample(1:1000, niter, replace = F)

# --- Function-on-Scalar Regression ---
# Hyperparamter Tunning
fos.tune.list = list(basis.type.choice = c('B-spline', 'Fourier'),
                     nbasis.choice = list(c(5, 6, 7),
                                          c(3, 4, 5)))

fos.best.param = fos.param.tune(tune.list = fos.tune.list, nfolds = 5, 
                                data.x, data.y, tpts = NULL, norder = 4)

fos.nbasis = fos.best.param$best.set$nbasis # 6 was given in our run

# Interation Set-ups
fos.train.no = vector("list", length = niter)
fos.beta.matrix = vector("list", length = niter)
fos.mise = vector("logical", length = niter)
# Run interations
fos.start = Sys.time()
for (k in 1:niter){
  output = fos(data.x = data.x, data.y = data.y, basis.type = "B-spline", nbasis = fos.nbasis, norder = 4, 
               split.rate = 0.8, seed = seeds, iter = k, plot = F)
  fos.train.no[[k]] = output$train.no
  fos.mise[k] = output$fos.mise
  fos.beta.matrix[[k]] = output$beta
  print(paste("Case", k, "is done!"))
} 
fos.end = Sys.time()
# Check performance
fos.runtime = (fos.end - fos.start)/niter
round(mean(fos.mise), 4)
round(sd(fos.mise), 4)

# --- Functional Additive Mixed Model ---
# Interation Set-ups
fam.train.no = vector("list", length = niter)
fam.model = vector("list", length = niter)
fam.mise = vector("logical", length = niter)
# Run interations
fam.start = Sys.time()
for (k in 1:niter){
  output = fam(data.x = data.x, data.y = data.y, spline.basis="cr", split.rate = 0.8, 
                    seed = seeds, iter = k, plot = F)
  fam.train.no[[k]] = output$train.no
  fam.mise[k] = output$fam.mise
  fam.model[[k]] = output$model
  print(paste("Case", k, "is done!"))
} 
fam.end = Sys.time()
# Check performance
fam.runtime = (fam.end - fam.start)/niter
round(mean(fam.mise), 4)
round(sd(fam.mise), 4)

# --- Hyperparameters Tunning for NN-based Models ---
# The tuning process is time-consuming, you can opt to skip. The NN-based model's configuration is based on the tuning result.
tune.list = list(nbasis = fos.nbasis,
                 hidden.nodes = list(c(500, 300), c(50, 30), c(100)),
                 activations.choice = c('relu', 'sigmoid', 'linear'),
                 epochs.no = c(1000, 1500),
                 optimizer = c("adam", "nadam"),
                 batch.no = c(4, 8),
                 val.rate = c(0.1),
                 early.patience = c(30, 50),
                 explained.var = NULL)

best.param = NN.param.tune(tune.list, nfolds = 5, NN.model = "NNBR",
                           data.x, data.y, scale.type = 1, basis.type = "B-spline", norder = 4, nfpc = 10,
                           early.stopping = T, penalty = NULL,
                           penalty.rate = 0)

# --- Neunal Network-Based FoS Model ---
# Iternation Set-ups
NNBR.train.no = vector("list", length = niter)
NNBR.mise = vector("logical",length = niter)
NNBR.basiscoef = vector("list", length = niter)
NNBR.y_pred = vector("list", length = niter)
NNBR.train.history = vector("list", length = niter)
# Run iterations
NNBR.start = Sys.time()
for (k in 10:niter){
  output = NNBR(data.x, data.y, scale.type = 0, basis.type = "B-spline", nbasis = fos.nbasis, norder = 4, 
                split.rate = 0.8, val.rate = 0.1, seed = seeds, iter = k,
                hidden.nodes = c(50, 30), activations = c('sigmoid', 'relu', 'sigmoid'), 
                batch.no = 8, epochs.no = 1500, early.stopping = T , early.patience = 50,
                penalty = '2nd.deriv',
                penalty.rate = 0, plot = F)
  NNBR.train.no[[k]] = output$train.no
  NNBR.mise[k] = output$NNBR.mise
  NNBR.basiscoef[[k]] = output$basiscoef_pred
  NNBR.y_pred[[k]] = output$y_pred
  NNBR.train.history[[k]] = output$train.history
  print(paste("Case", k, "is done!"))
}
NNBR.end = Sys.time()
# Check performance
NNBR.runtime = (NNBR.end - NNBR.start)/niter
round(mean(NNBR.mise), 4)
round(sd(NNBR.mise), 4)

#######################################
## Plot of Basis Coef vs. Predictor ##
######################################

tpts.no = ncol(data.y)
tpts = seq(0, 1, length.out = tpts.no)
n = nrow(data.x)

data.cov = cbind(1, data.x)
q = ncol(data.cov)

# Create basis functions for functional response
basis.rep.result = basis.rep(basis.type = "B-spline", nbasis = fos.nbasis, norder = 4, tpts)
data.basis = basis.rep.result$data.basis
Lfdobj = basis.rep.result$Lfdobj

# Raw data-estimated 
if (anyNA(data.y)){
  data.fd.coef = matrix(NA, nrow = n, ncol = nbasis)
  data.fdPar = fdPar(data.basis, Lfdobj, lambda=0)
  for (i in 1:n){
    data.smooth = smooth.basis(tpts[!is.na(data.y[i,])], as.matrix(data.y[i,!is.na(data.y[i,])]), data.fdPar)
    data.fd = data.smooth$fd
    data.fd.coef[i,] = data.smooth$fd$coef
  }
}else{
  basis.lambda = basis.lambda.select(data.basis, Lfdobj, tpts, data.y)
  data.fdPar = fdPar(data.basis, Lfdobj, lambda=basis.lambda)
  data.smooth = smooth.basis(tpts, t(data.y), data.fdPar)
  data.fd = data.smooth$fd
  data.fd.coef = t(data.smooth$fd$coef)
}

raw.data.coef = data.fd.coef


k = 17 # A random number selected from 1 to 20

## FOS-estimated
fos.output = fos(data.x = data.x, data.y = data.y, basis.type = "B-spline", nbasis = fos.nbasis, norder = 4, 
                 split.rate = 0.8, seed = seeds, iter = k, plot = F)
eval.beta = matrix(data=NA, nrow = tpts.no, ncol = q)
for (i in 1:q){
  eval.beta[,i] = eval.fd(tpts, fos.output$model$betaestlist[[i]]$fd) 
}

fos.all.yhat = data.cov %*% t(eval.beta)

basis.rep.result = basis.rep(basis.type = "B-spline", nbasis = fos.nbasis, norder = 4, tpts)
data.basis = basis.rep.result$data.basis
Lfdobj = basis.rep.result$Lfdobj

fos.basis.lambda = basis.lambda.select(data.basis, Lfdobj, tpts, fos.all.yhat)
fos.data.fdPar = fdPar(data.basis, Lfdobj, lambda=fos.basis.lambda)
fos.data.smooth = smooth.basis(tpts, t(fos.all.yhat), fos.data.fdPar)
fos.data.fd = fos.data.smooth$fd
fos.data.fd.coef = t(fos.data.smooth$fd$coef)

## NN-based FoS-estimated
NNBR.output = NNBR(data.x, data.y, scale.type = 0, basis.type = "B-spline", nbasis = fos.nbasis, norder = 4, 
                   split.rate = 0.8, val.rate = 0.1, seed = seeds, iter = k,
                   hidden.nodes = c(50, 30), activations = c('sigmoid', 'relu', 'sigmoid'), 
                   batch.no = 8, epochs.no = 1500, early.stopping = T , early.patience = 50,
                   penalty = '2nd.deriv',
                   penalty.rate = 0, plot = F)

x_scaled = rescale(data.x, scale.type = 0)
x_train_scaled = x_scaled[NNBR.output$train.no,]
x_test_scaled = x_scaled[-NNBR.output$train.no,]
NNBR.data.coef = NNBR.output$model %>% predict(x_scaled)

# Get basis functions evaluated at all observed time stamps
basis_fct = t(eval.basis(tpts, data.basis))

NNBR.all.yhat = NNBR.data.coef %*% basis_fct 
NNBR.basis.lambda = basis.lambda.select(data.basis, Lfdobj, tpts, NNBR.all.yhat)
NNBR.data.fdPar = fdPar(data.basis, Lfdobj, lambda=NNBR.basis.lambda)
NNBR.data.smooth = smooth.basis(tpts, t(NNBR.all.yhat), NNBR.data.fdPar)
NNBR.data.fd = NNBR.data.smooth$fd
NNBR.data.fd.coef = t(NNBR.data.smooth$fd$coef)


## Create scatter plots
df.x = as.data.frame(data.x)
x_list = y_list <- vector("list", 6)
for (i in 1:6){
  if (i <=3){
    x_list[[i]] = df.x$u5_mort
  }else{
    x_list[[i]] = df.x$age_fm
  }
}
y_list[[1]] = data.fd.coef[,1]
y_list[[2]] = fos.data.fd.coef[,1]
y_list[[3]] = NNBR.data.coef[,1]

y_list[[4]] = data.fd.coef[,2]
y_list[[5]] = fos.data.fd.coef[,2]
y_list[[6]] = NNBR.data.coef[,2]

par(mfrow = c(2, 3),  # 2 rows, 3 columns
    oma = c(1.5,1.5,1, 1),  # Outer margins (bottom, left, top, right)
    mar = c(3,3, 3, 1),  # Inner margins per plot
    cex.main = 1.5,       # Title font size
    cex.lab = 2,        # Axis label font size
    cex.axis = 0.9)       # Tick label font size

# Plot each dataset
for (i in 1:6) {
  panel_titles <- c('"Observed"', "FoS Linear", "NN-Based FoS",
                    '"Observed"', "FoS Linear", "NN-Based FoS")
  # Base scatterplot
  plot(x_list[[i]], y_list[[i]], 
       pch = 19, col = "black", cex = 0.7,
       main = panel_titles[i],
       xlab = "", ylab = "",  # No default labels
       xaxt = "n", yaxt = "n")
  
  # if (i==)
  
  # Add LOWESS smooth (similar to geom_smooth in ggplot)
  lines(lowess(x_list[[i]], y_list[[i]], f = 0.6), 
        col = "red", lwd = 2, lty=2)
  
  # Custom axis labels (only for outer plots)
  if (i %in% c(1, 3,4, 6)) {  # Bottom row
    axis(1, cex.axis = 1.5)
  }
  if (i %in% c(5)) {  # Bottom row
    axis(1, cex.axis = 1.5)
    mtext("(Scaled) Female Age at First Marriage", side = 1, line = 3, cex = 1)
  }
  if (i %in% c(2)) {  # Bottom row
    axis(1, cex.axis = 1.5)
    mtext("(Scaled) Under-5 Mortality", side = 1, line = 3, cex = 1)
  }
  if (i ==1) {  # First column
    axis(2, cex.axis = 1.5)
    mtext(expression(paste("1st basis coefficient (", c[1], ")")), side = 2, line = 2.5, cex = 1)
  }
  if (i ==4) {  # First column
    axis(2, cex.axis = 1.5)
    mtext(expression(paste("2nd basis coefficient (", c[2], ")")), side = 2, line = 2.5, cex = 1)
  }
}

dev.off()
