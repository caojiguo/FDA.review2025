message("============ Experiment: SoF - DTI ===============")

# Requirements:
# set the working directory to "Scalar-on-Function Regression"

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

save_dir <- file.path("results", "sof-dti")
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

USE_SAVED_MODELS <- FALSE
# FIXME: cannot save a torch model to RDS

# Load data --------------------------------------------------------------

data(DTI)

DTI_subset <- subset(DTI, case == 1)
complete_id <- (rowSums(is.na(DTI_subset$cca)) < 4) & (rowSums(is.na(DTI_subset$rcst)) < 4)
DTI_subset <- DTI_subset[complete_id, ]

DTI_subset <- DTI_subset |>
  filter(pasat > quantile(pasat, 0.01)) |>
  mutate(subj_visit = paste0("s", ID, "v", visit))

n <- nrow(DTI_subset)

# scalar response: pasat
y <- DTI_subset$pasat
# functional covariates: cca, rcst
X <- list(
  cca = DTI_subset$cca,
  rcst = DTI_subset$rcst
)
colnames(X$cca) <- 1:ncol(X$cca)
colnames(X$rcst) <- 1:ncol(X$rcst)
# scalar covariate: sex
# Z = cbind(
#   sex = if_else(DTI_subset$sex == "male", 1, 0),
#   DTI_subset$visit
# )
tgrid <- list(
  cca = 1:ncol(X$cca),
  rcst = 1:ncol(X$rcst)
)



library(patchwork)

# first tidy your two datasets separately
cca_tidy <- cbind(
  subj = DTI_subset$ID,
  visit = DTI_subset$visit,
  X$cca, PASAT = y
) |>
  as_tibble() |>
  mutate(sv = DTI_subset$subj_visit) |>
  filter(visit == 1) |>
  pivot_longer(
    cols      = as.character(tgrid$cca),
    names_to  = "Location",
    values_to = "Value"
  ) |>
  mutate(
    Location = as.numeric(Location),
    Measure = "CCA"
  )

rcst_tidy <- cbind(
  subj = DTI_subset$ID,
  visit = DTI_subset$visit,
  X$rcst, PASAT = y
) |>
  as_tibble() |>
  mutate(sv = DTI_subset$subj_visit) |>
  filter(visit == 1) |>
  pivot_longer(
    cols      = as.character(tgrid$rcst),
    names_to  = "Location",
    values_to = "Value"
  ) |>
  mutate(
    Location = as.numeric(Location),
    Measure = "RCST"
  )

# a shared colour scale object
pasat_breaks <- c(0, 20, 40, 60) # include 0 here
pasat_limits <- range(pasat_breaks) # c(0, 60)

library(viridis) # if not already installed, run: install.packages("viridis")

shared_scale <- scale_colour_viridis_c(
  name      = "PASAT",
  breaks    = pasat_breaks,
  limits    = pasat_limits,
  option    = "viridis", # "cividis", “viridis”, “magma”, “plasma”, “inferno”, etc.
  direction = 1
)

# two panels, each gets the same scale
p1 <- ggplot(
  cca_tidy,
  aes(Location, Value, colour = PASAT, group = sv)
) +
  geom_line(alpha = 0.7) +
  labs(x = "Location", y = "CCA") +
  shared_scale +
  theme_bw() +
  theme(
    panel.grid   = element_blank(),
    axis.title   = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

p2 <- ggplot(rcst_tidy, aes(Location, Value, colour = PASAT, group = sv)) +
  geom_line(alpha = 0.7) +
  labs(x = "Location", y = "RCST") +
  shared_scale +
  theme_bw() +
  theme(
    panel.grid   = element_blank(),
    axis.title   = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )


# patch together, collect the legend, and force equal panel sizes
(p1 + p2) +
  plot_layout(
    ncol = 2, # side by side
    guides = "collect"
  ) & # share legend
  theme(legend.position = "right") # or "right"

# ggsave(file.path(save_dir, "DTI.pdf"), device = "pdf", width = 7, height = 3)


# Train-Test Split --------------------------------------------------------

seed <- 1234
set.seed(seed)
nfold <- 5
folds <- sample(rep(seq_len(nfold), length.out = n))

# define data collection and a slicing helper
dataList <- list(y = y, X = X, Z = NULL)
dataSlicer <- function(dataList, idx) {
  list(
    y = dataList$y[idx],
    X = lapply(dataList$X, \(Xj) Xj[idx, , drop = F]),
    Z = NULL
  )
}

# train and test from the 1st fold
trainData <- dataSlicer(dataList, folds != 1)
testData <- dataSlicer(dataList, folds == 1)

# FNN  ----------------------------------------------

message("================ Start FNNs ================")

# define helper functions for cross-validation evaluation
modelFitAndEval <- function(trainData, testData, model = NULL, ...) {
  # fill missing values in cca and rcst
  trainData$X$cca <- fpcaImpute(trainData$X$cca, tgrid$cca)$X
  trainData$X$rcst <- fpcaImpute(trainData$X$rcst, tgrid$rcst)$X
  testData$X$cca <- fpcaImpute(testData$X$cca, tgrid$cca)$X
  testData$X$rcst <- fpcaImpute(testData$X$rcst, tgrid$rcst)$X
  nbasis.cca <- smoothBsplGCV(trainData$X$cca, tgrid$cca, nbasis.min = 10)$nbasis # %/% 2
  nbasis.rcst <- smoothBsplGCV(trainData$X$rcst, tgrid$rcst, nbasis.min = 10)$nbasis # %/% 2
  sofnn <- if (is.null(model)) {
    fit.sofnn(
      trainData$y, trainData$X, trainData$Z,
      tgrid = tgrid,
      nbasis = c(nbasis.cca, nbasis.rcst),
      lambda = 1e-6,
      ...
    )
  } else {
    model
  }
  ypred <- predict.sofnn.fit(
    sofnn,
    testData$X, testData$Z,
    tgrid = tgrid
  )
  list(
    model = sofnn,
    trainSEs = c(sofnn$ypred - trainData$y)^2,
    testSEs = c((ypred - testData$y)^2),
    trainErr = mean((sofnn$ypred - trainData$y)^2),
    testErr = mean((ypred - testData$y)^2),
    yhatTrain = sofnn$ypred,
    yhatTest = ypred
  )
}


torch_manual_seed(1234)
res_fnn <- modelFitAndEval(
  trainData, testData,
  hidden_sizes = c(128, 64, 32, 16),
  max_epoch = 5000,
  early_stop = TRUE,
  patience = 500,
  learning_rate = 1e-2,
  # pretrain = res_fnn$model$model,
  verbose = TRUE
)

# par(mfrow = c(1, 2))
# plot(res_fnn$yhatTrain, trainData$y, main = "Train", xlab = "Fitted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# plot(res_fnn$yhatTest, testData$y, main = "Test", xlab = "Predicted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# par(mfrow = c(1, 1))

se_fnn_train <- res_fnn$trainSEs
se_fnn_test <- res_fnn$testSEs
mse_fnn_train <- res_fnn$trainErr
mse_fnn_test <- res_fnn$testErr
yhat_fnn <- res_fnn$yhatTrain
ypred_fnn <- res_fnn$yhatTest
# save(
#   se_fnn_train, se_fnn_test,
#   mse_fnn_train, mse_fnn_train,
#   yhat_fnn, ypred_fnn,
#   file = file.path(save_dir, "FNNres-sd1234.RData")
# )


# {fdapace} FLM ---------------------------------------------------------------
message("================ Start fdapace::FLM ================")
# rdsfile <- file.path(save_dir, paste0("cvFLMpace-sd", seed, ".rds"))

# define helper functions for cross-validation evaluation
dataConvert <- function(dataList) {
  n <- length(dataList$y)
  out <- list(
    Y = dataList$y,
    X = lapply(
      names(dataList$X), \(v) list(
        Ly = lapply(1:n, \(i) X[[v]][i, ]),
        Lt = rep(list(tgrid[[v]]), n)
      )
    )
  )
  names(out$X) <- names(dataList$X)
  out
}
paceOptns <- list(
  dataType = "Dense",
  FVEthreshold = 0.95,
  methodBwMu = "GCV",
  methodBwCov = "GCV"
)
modelFitAndEval <- function(trainData, testData, model = NULL, ...) {
  trainData$X$cca <- fpcaImpute(trainData$X$cca, tgrid$cca)$X
  trainData$X$rcst <- fpcaImpute(trainData$X$rcst, tgrid$rcst)$X
  testData$X$cca <- fpcaImpute(testData$X$cca, tgrid$cca)$X
  testData$X$rcst <- fpcaImpute(testData$X$rcst, tgrid$rcst)$X
  trainDataC <- dataConvert(trainData)
  testDataC <- dataConvert(testData)
  flm <- if (is.null(model)) {
    fdapace::FLM(
      trainDataC$Y, trainDataC$X, testDataC$X,
      optnsListX = paceOptns
    )
  } else {
    model
  }
  list(
    model = flm,
    trainSEs = c((flm$yHat - trainDataC$Y)^2),
    testSEs = c((flm$yPred - testDataC$Y)^2),
    trainErr = mean((flm$yHat - trainDataC$Y)^2),
    testErr = mean((flm$yPred - testDataC$Y)^2),
    yhatTrain = flm$yHat,
    yhatTest = flm$yPred
  )
}

res_pcflm <- modelFitAndEval(trainData, testData)
se_pcflm_train <- res_pcflm$trainSEs
se_pcflm_test <- res_pcflm$testSEs
mse_pcflm_train <- res_pcflm$trainErr
mse_pcflm_test <- res_pcflm$testErr
yhat_pcflm <- res_pcflm$yhatTrain
ypred_pcflm <- res_pcflm$yhatTest
# save(
#   se_pcflm_train, se_pcflm_test,
#   mse_pcflm_train, mse_pcflm_train,
#   yhat_pcflm, ypred_pcflm,
#   file = file.path(save_dir, "PCFLMres-sd1234.RData")
# )

# par(mfrow=c(1,2))
# plot(res_pcflm$yhatTrain, trainData$y, main = "Train",
#   xlab = "Fitted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# plot(res_pcflm$yhatTest, testData$y, main = "Test",
#   xlab = "Predicted", ylab = "True")
# abline(coef = c(0, 1), col = 2)
# par(mfrow=c(1,1))


# PFR Functional Linear Model (spline-based FLM) -------------------------
message("========== Start refund::pfr for FLM ============")

# Prepare data for pfr: we'll pass functional predictor as a matrix column
dataConvert <- function(dataList) {
  data.frame(
    Y = dataList$y,
    X.cca = I(dataList$X$cca),
    X.rcst = I(dataList$X$rcst)
  )
}
modelFitAndEval <- function(trainData, testData, model = NULL, ...) {
  # Prepare data frames for mgcv::gam (pfr)
  trainData$X$cca <- fpcaImpute(trainData$X$cca, tgrid$cca)$X
  trainData$X$rcst <- fpcaImpute(trainData$X$rcst, tgrid$rcst)$X
  testData$X$cca <- fpcaImpute(testData$X$cca, tgrid$cca)$X
  testData$X$rcst <- fpcaImpute(testData$X$rcst, tgrid$rcst)$X
  nbasis.cca <- smoothBsplGCV(trainData$X$cca, tgrid$cca, nbasis.min = 10)$nbasis
  nbasis.rcst <- smoothBsplGCV(trainData$X$rcst, tgrid$rcst, nbasis.min = 10)$nbasis
  train_df <- dataConvert(trainData)
  test_df <- dataConvert(testData)
  # Fit penalized FLM with B-spline basis for beta(t)
  flm <- if (is.null(model)) {
    refund::pfr(
      Y ~ lf(X.cca,
        argvals = tgrid$cca, integration = "trapezoidal",
        bs = "ps", k = nbasis.cca
      ) +
        lf(X.rcst,
          argvals = tgrid$rcst, integration = "trapezoidal",
          bs = "ps", k = nbasis.rcst
        ),
      data = train_df, method = "REML"
    )
  } else {
    model
  }
  ypred <- predict(flm, newdata = test_df)
  list(
    model = flm,
    trainSEs = c((flm$fitted.values - train_df$Y)^2),
    testSEs = c((ypred - test_df$Y)^2),
    trainErr = mean((flm$fitted.values - train_df$Y)^2),
    testErr = mean((ypred - test_df$Y)^2),
    yhatTrain = flm$fitted.values,
    yhatTest = ypred
  )
}

res_flm <- modelFitAndEval(trainData, testData)
# par(mfrow=c(1,2))
# plot(res_flm$yhatTrain, trainData$y, main = "Train",
#      xlab = "Fitted", ylab = "True")
# abline(0, 1, col = 2)
# plot(res_flm$yhatTest, testData$y, main = "Test",
#      xlab = "Predicted", ylab = "True")
# abline(0, 1, col = 2)
# par(mfrow=c(1,1))

se_flm_train <- res_flm$trainSEs
se_flm_test <- res_flm$testSEs
mse_flm_train <- res_flm$trainErr
mse_flm_test <- res_flm$testErr
yhat_flm <- res_flm$yhatTrain
ypred_flm <- res_flm$yhatTest
# save(
#   se_flm_train, se_flm_test,
#   mse_flm_train, mse_flm_train,
#   yhat_flm, ypred_flm,
#   file = file.path(save_dir, "FLMres-sd1234.RData")
# )

# PFR Functional Additive Model (FGAM) -----------------------------------
message("========== Start refund::pfr for FAM ============")
# rdsfile <- file.path(save_dir, paste0("cvFAMpfr-sd", seed, ".rds"))

# We can reuse dataList and dataSlicer from above (same X matrix structure)
dataConvert <- function(dataList) {
  data.frame(
    Y = dataList$y,
    X.cca = I(dataList$X$cca),
    X.rcst = I(dataList$X$rcst)
  )
}
modelFitAndEval <- function(trainData, testData, model = NULL, ...) {
  trainData$X$cca <- fpcaImpute(trainData$X$cca, tgrid$cca)$X
  trainData$X$rcst <- fpcaImpute(trainData$X$rcst, tgrid$rcst)$X
  testData$X$cca <- fpcaImpute(testData$X$cca, tgrid$cca)$X
  testData$X$rcst <- fpcaImpute(testData$X$rcst, tgrid$rcst)$X
  nbasis.cca <- smoothBsplGCV(trainData$X$cca, tgrid$cca, nbasis.min = 10)$nbasis
  nbasis.rcst <- smoothBsplGCV(trainData$X$rcst, tgrid$rcst, nbasis.min = 10)$nbasis
  nbasis.t <- 10
  # large nbasis makes refund's FAM extremely large
  train_df <- dataConvert(trainData)
  test_df <- dataConvert(testData)
  # Fit FGAM with tensor-product spline on (t, X(t))
  fgam <- if (is.null(model)) {
    pfr(
      Y ~
        af(
          X.cca,
          argvals = tgrid$cca, Qtransform = TRUE,
          # k = c(nbasis.t, nbasis.cca)
          k = c(nbasis.t, nbasis.cca %/% 2)
        ) +
        af(
          X.rcst,
          argvals = tgrid$rcst, Qtransform = TRUE,
          # k = c(nbasis.t, nbasis.rcst)
          k = c(nbasis.t, nbasis.rcst %/% 2)
        ),
      data = train_df, method = "REML"
    )
  } else {
    model
  }
  # Predict on testData set
  ypred <- predict(fgam, newdata = test_df)
  list(
    model = fgam,
    trainSEs = c((fgam$fitted.values - train_df$Y)^2),
    testSEs = c((ypred - test_df$Y)^2),
    trainErr = mean((fgam$fitted.values - train_df$Y)^2),
    testErr = mean((ypred - test_df$Y)^2),
    yhatTrain = fgam$fitted.values,
    yhatTest = ypred
  )
}

res_fam <- modelFitAndEval(trainData, testData)
# par(mfrow=c(1,2))
# plot(res_fam$yhatTrain, trainData$y, main = "Train",
#      xlab = "Fitted", ylab = "True")
# abline(0, 1, col = 2)
# plot(res_fam$yhatTest, testData$y, main = "Test",
#      xlab = "Predicted", ylab = "True")
# abline(0, 1, col = 2)
# par(mfrow=c(1,1))
# vis.pfr(fam)

se_fam_train <- res_fam$trainSEs
se_fam_test <- res_fam$testSEs
mse_fam_train <- res_fam$trainErr
mse_fam_test <- res_fam$testErr
yhat_fam <- res_fam$yhatTrain
ypred_fam <- res_fam$yhatTest
# save(
#   se_fam_train, se_fam_test,
#   mse_fam_train, mse_fam_train,
#   yhat_fam, ypred_fam,
#   file = file.path(save_dir, "FAMres-sd1234.RData")
# )





# Compare plots -----------------------------------------------------------

### True vs. Predicted plots

# y_fit <- cbind(res_fnn$yhatTrain, res_flm$yhatTrain, res_fam$yhatTrain)
# y_pred <- cbind(res_fnn$yhatTest, res_flm$yhatTest, res_fam$yhatTest)
# colnames(y_fit) <- colnames(y_pred) <- c("FNN", "FLM", "FAM")
# pdf("results/sof-dti/dti-compare.pdf", width = 8, height = 5)
# pa <- par(no.readonly = TRUE)
# par(mfrow = c(2,3),
#     oma   = c(0, 2, 2, 0),
#     mar   = c(4, 4, 2, 2),
#     mgp   = c(2.5, 1, 0)
#   )
# for (i in 1:3) {
#   plot(trainData$y, y_fit[,i],
#     xlab="",
#     ylab=ifelse(i==1, "Fitted", ""),
#     cex = 1.2
#   )
#   abline(0, 1, col = 2)
#   if (i == 1) {
#     mtext("Train", line = 4, side = 2, font = 2)
#   }
#   mtext(colnames(y_fit)[i], side = 3, line = 2, font = 2)
#   mtext(paste0("MSE=", round(mean((y_fit[,i] - trainData$y)^2), 3)),
#     side=3, line=0.5, cex = 0.8)
# }
# for (i in 1:3) {
#   plot(testData$y, y_pred[,i],
#     xlab="True",
#     ylab=ifelse(i==1, "Predicted", ""),
#     cex = 1.2)
#   abline(0, 1, col = 2)
#   if (i == 1) {
#     mtext("Test", line = 4, side = 2, font = 2)
#   }
#   mtext(paste0("MSE=", round(mean((y_pred[,i] - testData$y)^2), 3)),
#     side=3, line=0.5, cex = 0.8)
# }
# par(pa)
# dev.off()


rbind(
  c(mse_fnn_train, mse_pcflm_train, mse_flm_train, mse_fam_train),
  c(mse_fnn_test, mse_pcflm_test, mse_flm_test, mse_fam_test)
) |> round(2)


# Save results -----------------------------------------------------------

readr::write_csv(data.frame(train_cv_err), file.path(save_dir, "cvmse-train.csv"))
readr::write_csv(data.frame(test_cv_err), file.path(save_dir, "cvmse-test.csv"))
