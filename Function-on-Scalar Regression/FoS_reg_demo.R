# Call functions and load packages
source('Functions.R')

packages = c("fda", "fdapace", "MASS", "refund", "tensorflow", "keras", "caret", "dplyr", "pbapply")
install.packages(packages)
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

# --- Hyperparameters Tunning for NN-based Models (NNBB, NNBR, NNSS, NNSR) ---
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

# --- NNBB ---
# Interation Set-ups
NNBB.train.no = vector("list", length = niter)
NNBB.mise = vector("logical",length = niter)
NNBB.basiscoef = vector("list",length = niter)
NNBB.train.history = vector("list", length = niter)
# Run iterations
NNBB.start = Sys.time()
for (k in 1:niter){
  output = NNBB(data.x, data.y, scale.type = 1, basis.type = "B-spline", nbasis = fos.nbasis, norder = 4, 
                split.rate = 0.8, val.rate = 0.1, seed = seeds, iter = k,
                hidden.nodes = c(50,30), activations = c('sigmoid','relu'), 
                batch.no =8, epochs.no = 1500, early.stopping =T, early.patience = 50,
                plot = F)
  NNBB.train.no[[k]] = output$train.no
  NNBB.mise[k] = output$NNBB.mise
  NNBB.basiscoef[[k]] = output$basiscoef_pred
  NNBB.train.history[[k]] = output$train.history
  print(paste("Case", k, "is done!"))
}
NNBB.end = Sys.time()
# Check perfromance
NNBB.runtime = (NNBB.end - NNBB.start)/niter
round(mean(NNBB.mise), 4)
round(sd(NNBB.mise), 4)

# --- NNSS w/ fda (R package) ---
# Iternation Set-ups
NNSS.fda.train.no = vector("list", length = niter)
NNSS.fda.mise = vector("logical",length = niter)
NNSS.fda.train.history = vector("list", length = niter)
# Run iterations
NNSS.fda.start = Sys.time()
for (k in 1:niter){
  output = NNSS.fda(data.x, data.y, scale.type = 1, basis.type = "B-spline", nbasis = fos.nbasis, norder = 4,
                    nfpc = fos.nbasis, explained.var = 0.99,
                    split.rate = 0.8, val.rate = 0.1, seed = seeds, iter = k,
                    hidden.nodes = c(50,30), activations = c('relu','relu'), 
                    batch.no = 8, epochs.no = 1500, early.stopping = T, early.patience = 50,
                    plot = F)
  NNSS.fda.train.no[[k]] = output$train.no
  NNSS.fda.mise[k] = output$NNSS.fda.mise
  NNSS.fda.train.history[[k]] = output$train.history
  print(paste("Case", k, "is done!"))
}
NNSS.fda.end = Sys.time()
# Check perfromance
NNSS.fda.runtime = (NNSS.fda.end - NNSS.fda.start)/niter
round(mean(NNSS.fda.mise), 4)
round(sd(NNSS.fda.mise), 4)

# --- NNBR ---
# Iternation Set-ups
NNBR.train.no = vector("list", length = niter)
NNBR.mise = vector("logical",length = niter)
NNBR.basiscoef = vector("list", length = niter)
NNBR.y_pred = vector("list", length = niter)
NNBR.train.history = vector("list", length = niter)
# Run iterations
NNBR.start = Sys.time()
for (k in 1:niter){
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


# --- NNBR with penalty ------
# Leave-p-out CV to select lambda for roughness penalty
lambda.list = vector("logical", length = 8)
for (i in 1:length(lambda.list)){
  lambda.list[i] = 1*10^(-(i))
}

NNBR.CV = NNBR.pen.cv(data.x, data.y, scale.type = 1, nfolds = 5, basis.type = "B-spline",
                      nbasis = 10, norder = 4,
                      hidden.nodes = c(50, 30),activations = c('sigmoid','relu'), 
                      early.stopping = T, early.patience = 50,
                      batch.no = 8, epochs.no = 1500, 
                      penalty = "2nd.deriv", penalty.rate.list = lambda.list)

lambda = lambda.list[which.min(NNBR.CV[[1]])]

# Iternation Set-ups
NNBR.pen.train.no = vector("list", length = niter)
NNBR.pen.mise = vector("logical",length = niter)
NNBR.pen.basiscoef = vector("list", length = niter)
NNBR.pen.y_pred = vector("list", length = niter)
NNBR.pen.train.history = vector("list", length = niter)
# Run iterations
NNBR.pen.start = Sys.time()
for (k in 1:niter){
  output = NNBR(data.x, data.y, scale.type = 0, basis.type = "B-spline", nbasis = 10, norder = 4, 
                split.rate = 0.8, val.rate = 0.1, seed = seeds, iter = k,
                hidden.nodes = c(50, 30), activations = c('sigmoid', 'relu', 'sigmoid'), 
                batch.no = 8, epochs.no = 1500, early.stopping = T , early.patience = 50,
                penalty = '2nd.deriv',
                penalty.rate = lambda, plot = F)
  NNBR.pen.train.no[[k]] = output$train.no
  NNBR.pen.mise[k] = output$NNBR.mise
  NNBR.pen.basiscoef[[k]] = output$basiscoef_pred
  NNBR.pen.y_pred[[k]] = output$y_pred
  NNBR.pen.train.history[[k]] = output$train.history
  print(paste("Case", k, "is done!"))
}
NNBR.pen.end = Sys.time()
# Check performance
NNBR.pen.runtime = (NNBR.pen.end - NNBR.pen.start)/niter
round(mean(NNBR.pen.mise), 4)
round(sd(NNBR.pen.mise), 4)

# --- NNSR w/ fda (R package) ---
# Iternation Set-ups
NNSR.fda.train.no = vector("list", length = niter)
NNSR.fda.mise = vector("logical",length = niter)
NNSR.fda.y_pred = vector("list", length = niter)
NNSR.fda.train.history = vector("list", length = niter)
# Run iterations
NNSR.fda.start = Sys.time()
for (k in 1:niter){
  output = NNSR.fda(data.x, data.y, scale.type = 0, basis.type = "B-spline", nbasis = fos.nbasis, norder = 4,
                    nfpc = fos.nbasis, explained.var = 0.99,
                    split.rate = 0.8, val.rate = 0.1, seed = seeds, iter = k,
                    hidden.nodes = c(50, 30), activations = c('sigmoid','relu'), 
                    batch.no = 8, epochs.no = 1000, early.stopping = T, early.patience = 50,
                    plot = F)
  NNSR.fda.train.no[[k]] = output$train.no
  NNSR.fda.mise[k] = output$NNSR.fda.mise
  NNSR.fda.y_pred[[k]] = output$y_pred
  NNSR.fda.train.history[[k]] = output$train.history
  print(paste("Case", k, "is done!"))
}
NNSR.fda.end = Sys.time()
# Check performance
NNSR.fda.runtime = (NNSR.fda.end - NNSR.fda.start)/niter
round(mean(NNSR.fda.mise), 4)
round(sd(NNSR.fda.mise), 4)
