message("============ Experiment: SoF - Tecator ===============")

library(fda)
library(fdapace)
library(refund)
library(torch)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

print("Torch threads:")
print(torch_get_num_threads())

source("R/SoFNN.R")

save_dir <- file.path("results", "sof-tecator") 
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

USE_SAVED_MODELS <- FALSE
# FIXME: cannot save a torch model to RDS


# Load data --------------------------------------------------------------

data(Fatspectrum, package = "fds")
data(Fatvalues, package = "fds")

tvals = Fatspectrum$x
names(tvals) = paste0("t", seq_along(tvals))
X = t(Fatspectrum$y)
y = Fatvalues
n = length(y)
colnames(X) = paste0("t", seq_along(tvals))

tidy_data = cbind(subj = 1:nrow(X), X, Fat = y) |>
  as_tibble() |>
  pivot_longer(
    cols = paste0("t", seq_along(tvals)),
    names_to = "tid",
    values_to = "Spectrum"
  ) |>
  mutate(Wavelength = tvals[tid])

ggplot(tidy_data, aes(x = Wavelength, y = Spectrum)) +
  geom_line(aes(color = Fat, group = subj)) +
  labs(x = "Wavelength (nm)", color = "Fat (%)") +
  theme_bw() +
  theme(panel.grid = element_blank())
ggsave(file.path(save_dir, "Tecator.pdf"), width = 5, height=3, device = "pdf")


train_cv_err = c()
test_cv_err = c()


# Train-Test Split --------------------------------------------------------

seed = 1234
set.seed(seed)
nfold <- 5
folds <- sample(rep(seq_len(nfold), length.out = n))

# define data collection and a slicing helper
dataList = list(y = y, X = X, Z = NULL)
dataSlicer = function(dataList, idx) {
  list(
    y = dataList$y[idx],
    X = X[idx, , drop = F],
    Z = NULL
  )
}

# trainData and testData from the 1st fold
trainData = dataSlicer(dataList, folds != 1)
testData = dataSlicer(dataList, folds == 1)

# FNN  ----------------------------------------------

message("================ Start FNNs ================")

# define helper functions for cross-validation evaluation
modelFitAndEval = function(trainData, testData, model=NULL, ...) {
  nbasis <- smoothBsplGCV(trainData$X, tvals, nbasis.min = 10)$nbasis
  sofnn = if (is.null(model)) {
    fit.sofnn(
      trainData$y, trainData$X, trainData$Z,
      tgrid = tvals,
      nbasis = nbasis,
      lambda = 1e-8,
      ...
    )
  } else {model}
  ypred = predict.sofnn.fit(
    sofnn,
    testData$X, testData$Z,
    tgrid = tvals
  )
  list(
    model = sofnn,
    trainErr = mean((sofnn$ypred - trainData$y)^2),
    testErr = mean((ypred - testData$y)^2),
    yhatTrain = sofnn$ypred,
    yhatTest = ypred
  )
}

for (patience in c(200, 1000)) {
  message("Patience: ", patience)
  for (nhidden in c(1,2,3)) {
    message("Number of hidden layers: ", nhidden)
    modelname <- paste0("FNN", "p", patience, "l", nhidden)
    rdsfile = file.path(save_dir,
      paste0("cv", modelname, "-sd", seed, ".rds"))
    # evaluate mean CV MSE
    cvPerf = cvWrapper(
      dataList,
      dataSlicer,
      folds,
      modelFitAndEval,
      savedModels = if (USE_SAVED_MODELS) { readRDS(rdsfile)} else { NULL},
      hidden_sizes = 64 / 2^seq(0,nhidden-1),
      early_stop = TRUE,
      patience = patience,
      max_epoch = 10000 + nhidden * 10000,
      verbose = TRUE
    )
    mean(cvPerf$testErr)
    train_cv_err = write_column(
      train_cv_err, cvPerf$trainErr, modelname)
    test_cv_err = write_column(
      test_cv_err, cvPerf$testErr, modelname)
    if (!USE_SAVED_MODELS) saveRDS(cvPerf$models, rdsfile)
  }
}

# res <- modelFitAndEval(
#   trainData, testData,
#   hidden_sizes = c(64, 32), max_epoch = 10000,
#   # pretrain = res$model$model,
#   # early_stop = 200,
#   verbose = TRUE
# )
# par(mfrow = c(1, 2))
# plot(res$yhatTrain, trainData$y, main = "Train", xlab = "Fitted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# plot(res$yhatTest, testData$y, main = "Test", xlab = "Predicted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# par(mfrow = c(1, 1))
# fnn_y_fit <- res$yhatTrain
# fnn_y_pred <- res$yhatTest


# {fdapace} FLM ---------------------------------------------------------------
message("================ Start fdapace::FLM ================")
rdsfile <- file.path(save_dir, paste0("cvFLMpace-sd", seed, ".rds"))

# define helper functions for cross-validation evaluation
dataConvert = function(dataList) {
  n = length(dataList$y)
  out <- list(
    Y = dataList$y,
    X = list(list(
      Ly = lapply(1:n, \(i) dataList$X[i,]),
      Lt = rep(list(tvals), n)
    )))
  out
}
paceOptns <- list(
  dataType = "Dense",
  FVEthreshold=0.999,
  methodBwMu = "GCV",
  methodBwCov = "GCV"
)
modelFitAndEval = function(trainData, testData, model = NULL, ...) {
  trainDataC = dataConvert(trainData)
  testDataC = dataConvert(testData)
  flm = if (is.null(model)) {
    fdapace::FLM(
      trainDataC$Y, trainDataC$X, testDataC$X,
      optnsListX = paceOptns
    )
  } else {model}
  list(
    model = flm,
    trainErr = mean((flm$yHat - trainDataC$Y)^2),
    testErr = mean((flm$yPred - testDataC$Y)^2),
    yhatTrain = flm$yHat,
    yhatTest = flm$yPred
  )
}

# evaluate mean CV MSE
cvPerf = cvWrapper(
  dataList,
  dataSlicer,
  folds,
  modelFitAndEval,
  savedModels = if (USE_SAVED_MODELS) { readRDS(rdsfile)} else { NULL}
)
mean(cvPerf$testErr)
train_cv_err = write_column(train_cv_err, cvPerf$trainErr, "FLMpace")
test_cv_err = write_column(test_cv_err, cvPerf$testErr, "FLMpace")
if (!USE_SAVED_MODELS) saveRDS(cvPerf$models, rdsfile)

# res = modelFitAndEval(trainData, testData)
# par(mfrow=c(1,2))
# plot(res$yhatTrain, trainData$y, main = "Train",
#   xlab = "Fitted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# plot(res$yhatTest, testData$y, main = "Test",
#   xlab = "Predicted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# par(mfrow=c(1,1))


# {fdapace} FAM -----------------------------------------------
message("================ Start fdapace::FAM ================")
rdsfile <- file.path(save_dir, paste0("cvFAMpace-sd", seed, ".rds"))

# Prepare data for FAM: list of function values and time grids for each subject
dataConvert <- function(dataList) {
  n = length(dataList$y)
  list(
    Y = dataList$y,
    Lx = lapply(1:n, function(i) dataList$X[i, ]),
    Lt = rep(list(tvals), n)
  )
}
paceOptns <- list(
  dataType = "Dense",
  FVEthreshold=0.999,
  methodBwMu = "GCV",
  methodBwCov = "GCV"
)
modelFitAndEval <- function(trainData, testData, model = NULL, ...) {
  # Fit FAM on training fold and predict on testData fold
  trainDataC = dataConvert(trainData)
  testDataC = dataConvert(testData)
  fam <- if (is.null(model)) {
    fdapace::FAM(
      trainDataC$Y, trainDataC$Lx, trainDataC$Lt,
      newLx = trainDataC$Lx, newLt = trainDataC$Lt,
      optns = paceOptns
    )
  } else {model}
  yhat <- fam$mu + rowSums(fam$fam)
  # Predicted values for testData set: mu + sum of component functions
  fam <- fdapace::FAM(
    trainDataC$Y, trainDataC$Lx, trainDataC$Lt,
    newLx = testDataC$Lx, newLt = testDataC$Lt,
    optns = paceOptns
  )
  ypred <- fam$mu + rowSums(fam$fam)
  list(
    model = fam,
    trainErr = mean((yhat - trainData$y)^2),
    testErr = mean((ypred - testData$y)^2),
    yhatTrain = yhat,
    yhatTest = ypred
  )
}

# 5-fold cross-validation MSE for FAM
cvPerf <- cvWrapper(
  dataList, dataSlicer, folds, modelFitAndEval,
  savedModels = if (USE_SAVED_MODELS) { readRDS(rdsfile)} else { NULL}
)
mean(cvPerf$testErr)  # mean CV MSE for FAM
train_cv_err = write_column(train_cv_err, cvPerf$trainErr, "FAMpace")
test_cv_err = write_column(test_cv_err, cvPerf$testErr, "FAMpace")
if (!USE_SAVED_MODELS) saveRDS(cvPerf$models, rdsfile)

# res = modelFitAndEval(trainData, testData)
# par(mfrow=c(1,2))
# plot(res$yhatTrain, trainData$y, main = "Train",
#   xlab = "Fitted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# plot(res$yhatTest, testData$y, main = "Test",
#   xlab = "Predicted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# par(mfrow=c(1,1))


# PFR Functional Linear Model (spline-based FLM) -------------------------
message("========== Start refund::pfr for FLM ============")
rdsfile <- file.path(save_dir, paste0("cvFLMpfr-sd", seed, ".rds"))

# Prepare data for pfr: we'll pass functional predictor as a matrix column
dataConvert <- function(dataList) {
  data.frame(
    Y = dataList$y,
    X = I(dataList$X)
  )
}
modelFitAndEval <- function(trainData, testData, model = NULL, ...) {
  # Prepare data frames for mgcv::gam (pfr)
  nbasis <- smoothBsplGCV(trainData$X, tvals, nbasis.min = 10)$nbasis
  train_df <- dataConvert(trainData)
  test_df <- dataConvert(testData)
  # Fit penalized FLM with B-spline basis for beta(t)
  flm <- if (is.null(model)) {
    pfr(
      Y ~ lf(X, argvals = tvals, integration = "trapezoidal",
            bs = "ps", k = nbasis),
      data = train_df, method = "REML"
    )
  } else {model}
  ypred <- predict(flm, newdata = test_df)
  list(
    model = flm,
    trainErr = mean((flm$fitted.values - train_df$Y)^2),
    testErr = mean((ypred - test_df$Y)^2),
    yhatTrain = flm$fitted.values,
    yhatTest = ypred
  )
}

# 5-fold cross-validation MSE for spline FLM
cvPerf <- cvWrapper(
  dataList, dataSlicer, folds, modelFitAndEval,
  savedModels = if (USE_SAVED_MODELS) { readRDS(rdsfile)} else { NULL}
)
mean(cvPerf$testErr)  # mean CV MSE for FLM (pfr linear)
train_cv_err = write_column(train_cv_err, cvPerf$trainErr, "FLMpfr")
test_cv_err = write_column(test_cv_err, cvPerf$testErr, "FLMpfr")
if (!USE_SAVED_MODELS) saveRDS(cvPerf$models, rdsfile)

# res <- modelFitAndEval(trainData, testData)
# par(mfrow=c(1,2))
# plot(res$yhatTrain, trainData$y, main = "Train",
#      xlab = "Fitted", ylab = "True")
# abline(0, 1, col = 2)
# plot(res$yhatTest, testData$y, main = "Test",
#      xlab = "Predicted", ylab = "True")
# abline(0, 1, col = 2)
# par(mfrow=c(1,1))
# flm_y_fit <- res$yhatTrain
# flm_y_pred <- res$yhatTest


# PFR Functional Additive Model (FGAM) -----------------------------------
message("========== Start refund::pfr for FAM ============")
rdsfile <- file.path(save_dir, paste0("cvFAMpfr-sd", seed, ".rds"))

# We can reuse dataList and dataSlicer from above (same X matrix structure)
dataConvert <- function(dataList) {
  data.frame(
    Y = dataList$y,
    X = I(dataList$X)
  )
}
modelFitAndEval <- function(trainData, testData, model = NULL, ...) {
  nbasis <- smoothBsplGCV(trainData$X, tvals, nbasis.min = 10)$nbasis
  nbasis.t <- 11
  train_df <- dataConvert(trainData)
  test_df <- dataConvert(testData)
  # Fit FGAM with tensor-product spline on (t, X(t))
  fgam <- if (is.null(model)) {
    pfr(
      Y ~ af(X, argvals = tvals, k = c(nbasis.t, nbasis)),
      data = train_df, method = "REML"
    )
  } else {model}
  # Predict on testData set
  ypred <- predict(fgam, newdata = test_df)
  list(
    model = fgam,
    trainErr = mean((fgam$fitted.values - train_df$Y)^2),
    testErr = mean((ypred - test_df$Y)^2),
    yhatTrain = fgam$fitted.values,
    yhatTest = ypred
  )
}

# 5-fold cross-validation MSE for FGAM
cvPerf <- cvWrapper(
  dataList, dataSlicer, folds, modelFitAndEval,
  savedModels = if (USE_SAVED_MODELS) { readRDS(rdsfile)} else { NULL}
)
mean(cvPerf$testErr)  # mean CV MSE for FGAM (pfr additive)
train_cv_err = write_column(train_cv_err, cvPerf$trainErr, "FAMpfr")
test_cv_err = write_column(test_cv_err, cvPerf$testErr, "FAMpfr")
if (!USE_SAVED_MODELS) saveRDS(cvPerf$models, rdsfile)

# res <- modelFitAndEval(trainData, testData)
# par(mfrow=c(1,2))
# plot(res$yhatTrain, trainData$y, main = "Train",
#      xlab = "Fitted", ylab = "True")
# abline(0, 1, col = 2)
# plot(res$yhatTest, testData$y, main = "Test",
#      xlab = "Predicted", ylab = "True")
# abline(0, 1, col = 2)
# par(mfrow=c(1,1))
# fam_y_fit <- res$yhatTrain
# fam_y_pred <- res$yhatTest


# Compare plots ----------------------------------------------------------

# y_fit <- cbind(fnn_y_fit, flm_y_fit, fam_y_fit)
# y_pred <- cbind(fnn_y_pred, flm_y_pred, fam_y_pred)
# saveRDS(y_fit, file = "results/sof-tecator/y_fit.rds")
# saveRDS(y_pred, file = "results/sof-tecator/y_pred.rds")
y_fit <- readRDS("results/sof-tecator/y_fit.rds")
y_pred <- readRDS("results/sof-tecator/y_pred.rds")
colnames(y_fit) <- colnames(y_pred) <- c("FNN", "FLM", "FAM")
pdf("results/sof-tecator/tecator-compare.pdf", width = 8, height = 5)
pa <- par(no.readonly = TRUE)
par(mfrow = c(2,3),
    oma   = c(0, 2, 2, 0),
    mar   = c(4, 4, 2, 2),
    mgp   = c(2.5, 1, 0)
  )
for (i in 1:3) {
  plot(trainData$y, y_fit[,i],
    xlab="",
    ylab=ifelse(i==1, "Fitted", ""),
    cex = 1.2
  )
  abline(0, 1, col = 2)
  if (i == 1) {
    mtext("Train", line = 4, side = 2, font = 2)
  }
  mtext(colnames(y_fit)[i], side = 3, line = 2, font = 2)
  mtext(paste0("MSE=", round(mean((y_fit[,i] - trainData$y)^2), 3)),
    side=3, line=0.5, cex = 0.8)
}
for (i in 1:3) {
  plot(testData$y, y_pred[,i],
    xlab="True",
    ylab=ifelse(i==1, "Predicted", ""),
    cex = 1.2)
  abline(0, 1, col = 2)
  if (i == 1) {
    mtext("Test", line = 4, side = 2, font = 2)
  }
  mtext(paste0("MSE=", round(mean((y_pred[,i] - testData$y)^2), 3)),
    side=3, line=0.5, cex = 0.8)
}
par(pa)
dev.off()


# Save results -----------------------------------------------------------

readr::write_csv(data.frame(train_cv_err), "results/sof-tecator/cvmse-train.csv")
readr::write_csv(data.frame(test_cv_err), "results/sof-tecator/cvmse-test.csv")
