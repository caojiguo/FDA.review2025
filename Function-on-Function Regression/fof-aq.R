message("============ Experiment: FoF - Air Quality ===============")

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

source("R/FoFNN.R")
source("R/helpers.R")

save_dir <- file.path("results", "fof-aq") 
if (!dir.exists(save_dir)) {
  dir.create(save_dir, recursive = TRUE)
}

USE_SAVED_MODELS <- FALSE
# FIXME: cannot save a torch model to RDS


# Load data --------------------------------------------------------------

dat_dir <- file.path("data", "canadian_climate_aq")

read_data <- function(name) {
  date.start <- as.character.Date("2023-07-01")
  date.end <- as.character.Date("2023-08-31")
  readRDS(file.path(dat_dir, paste0(name, ".rds"))) |> 
    filter(Date >= date.start & Date <= date.end)
}

dat.NO <- read_data("NO")
# dat.NO2 <- read_data("NO2")
dat.O3 <- read_data("O3")
dat.PM25 <- read_data("PM2.5")
# dat.maxhumid <- read_data("MaxRelHumidity")
dat.minhumid <- read_data("MinRelHumidity")
dat.temp <- read_data("MeanTemperature")

dates <- dat.NO |> pull(Date)
sites <- names(dat.NO |> select(-Date))

get_val_mat <- function(dat) {
  t(as.matrix(dat |> select(all_of(sites))))
}


# Format model variables --------------------------------------------------

Y = get_val_mat(dat.PM25) |> log1p()
N = nrow(Y)
X = list(
  NO = get_val_mat(dat.NO) |> log1p() |> log1p(),
  # NO2 = get_val_mat(dat.NO2) |> log1p(),
  O3 = get_val_mat(dat.O3),
  # maxhumid = get_val_mat(dat.maxhumid) |> log(),
  minhumid = get_val_mat(dat.minhumid),
  temp = get_val_mat(dat.temp)
)
varXnames <- c(
  "NO",
  # "NO2",
  "O3",
  # "maxhumid",
  "minhumid",
  "temp"
)

tvals0 = as.integer(dates - dates[1])
tvals = tvals0 / diff(range(tvals0))
xtgrid = rep(list(tvals), length(X))
ytgrid = tvals


# Train-Test Split --------------------------------------------------------

seed = 1234
set.seed(seed)
nfold <- 5
folds <- sample(rep(seq_len(nfold), length.out = N))

# define data collection and a slicing helper
dataList = list(Y = Y, X = X, N = nrow(Y))
dataSlicer = function(dataList, idx) {
  list(
    Y = dataList$Y[idx, , drop = F],
    X = lapply(dataList$X, \(Xj) Xj[idx, , drop = F]),
    N = if (all(is.logical(idx))) {sum(idx)} else {length(idx)}
  )
}

# train and test from the 1st fold
trainData = dataSlicer(dataList, folds != 1)
testData = dataSlicer(dataList, folds == 1)

train_cv_err = c()
test_cv_err = c()

trainDataImp <- trainData
testDataImp <- testData
for (j in seq_along(trainDataImp$X)) {
  trainDataImp$X[[j]] <- fpcaImpute(trainDataImp$X[[j]], xtgrid[[j]])$X
  testDataImp$X[[j]] <- fpcaImpute(testDataImp$X[[j]], xtgrid[[j]])$X
}
trainDataImp$Y <- fpcaImpute(trainDataImp$Y, ytgrid)$X
testDataImp$Y <- fpcaImpute(testDataImp$Y, ytgrid)$X

dataConvert <- function(dataList) {
  as.data.frame(c(
    list(Y = I(dataList$Y)),
    lapply(dataList$X, I)
  ))
}
train_df = dataConvert(trainDataImp)
test_df = dataConvert(testDataImp)

# FNN  ----------------------------------------------

message("================ Start FNNs ================")

nbasis.fnn <- 19
# nbasis.fnn <- 31

# # define helper functions for cross-validation evaluation
# modelFitAndEval = function(trainData, testData, model=NULL, ...) {
#   # xnbasis = numeric(length(trainData$X))
#   for (j in seq_along(trainData$X)) {
#     trainData$X[[j]] <- fpcaImpute(trainData$X[[j]], xtgrid[[j]])$X
#     testData$X[[j]] <- fpcaImpute(testData$X[[j]], xtgrid[[j]])$X
#     # xnbasis[j] <- smoothBsplGCV(trainData$X[[j]], xtgrid[[j]], nbasis.min = 7)$nbasis
#   }
#   trainData$Y <- fpcaImpute(trainData$Y, ytgrid)$X
#   testData$Y <- fpcaImpute(testData$Y, ytgrid)$X
#   # ynbasis <- smoothBsplGCV(trainData$Y, ytgrid, nbasis.min = 7)$nbasis
#   xnbasis <- rep(nbasis.fnn, length(trainData$X))
#   ynbasis <- nbasis.fnn
#   fofnn = if (is.null(model)) {
#     fit.fofnn(
#       trainData$Y, trainData$X, trainData$Z,
#       xtgrid = xtgrid,
#       ytgrid = ytgrid,
#       xnbasis = xnbasis,
#       ynbasis = ynbasis,
#       xlambda = 1e-8,
#       ylambda = 1e-8,
#       ...
#     )
#   } else {model}
#   Ypred = predict.fofnn.fit(
#     fofnn,
#     testData$X, testData$Z,
#     xtgrid = xtgrid,
#     ytgrid = ytgrid
#   )
#   list(
#     model = fofnn,
#     trainErr = mean((fofnn$Ypred[[1]] - trainData$Y)^2),
#     testErr = mean((Ypred[[1]] - testData$Y)^2),
#     yhatTrain = fofnn$Ypred[[1]],
#     yhatTest = Ypred[[1]],
#     xnbasis = xnbasis,
#     ynbasis = ynbasis
#   )
# }

# torch_manual_seed(1234)
# res_fnn <- modelFitAndEval(
#   trainData, testData,
#   hidden_sizes = c(128,64,32),
#   max_epoch = 10000,
#   early_stop = TRUE,
#   patience = 200,
#   # pretrain = res_fnn$model$model,
#   learning_rate = 1e-3,
#   verbose = TRUE
# )
# yhat_fnn <- res_fnn$yhatTrain
# ypred_fnn <- res_fnn$yhatTest
# se_fnn_train <- rowMeans((yhat_fnn - trainDataImp$Y)^2)
# se_fnn_test <- rowMeans((ypred_fnn - testDataImp$Y)^2)
# mse_fnn_train <- res_fnn$trainErr
# mse_fnn_test <- res_fnn$testErr
# save(
#   yhat_fnn,
#   ypred_fnn,
#   se_fnn_train,
#   se_fnn_test,
#   mse_fnn_train,
#   mse_fnn_test,
#   file=file.path(
#     save_dir, paste0("FNNres-sd", seed, "-nb", nbasis.fnn, ".RData")
#   )
# )

# PFR Functional Linear Model (spline-based FLM) -------------------------
message("========== Start refund::pffr for FLM ============")

k_flm <- 11
# k_flm <- 23
# NOTE: the smoothness of pffr's output is a bit complicated
# In general, setting k=nbasis in pffr model gives rougher functional outputs
# that need more than `nbasis` B-spline basis functions to be approximated
# In this experiment, nbasis.fnn=19 roughly matches k_flm=11

# dataConvert <- function(dataList) {
#   as.data.frame(c(
#     list(Y = I(dataList$Y)),
#     lapply(dataList$X, I)
#   ))
# }
# modelFitAndEval = function(trainData, testData, model=NULL, ...) {
#   # xnbasis = numeric(length(trainData$X))
#   for (j in seq_along(trainData$X)) {
#     trainData$X[[j]] <- fpcaImpute(trainData$X[[j]], xtgrid[[j]])$X
#     testData$X[[j]] <- fpcaImpute(testData$X[[j]], xtgrid[[j]])$X
#     # xnbasis[j] <- smoothBsplGCV(trainData$X[[j]], xtgrid[[j]], nbasis.min = 7)$nbasis %/% 4
#   }
#   trainData$Y <- fpcaImpute(trainData$Y, ytgrid)$X
#   testData$Y <- fpcaImpute(testData$Y, ytgrid)$X
#   # ynbasis <- smoothBsplGCV(trainData$Y, ytgrid, nbasis.min = 7)$nbasis %/% 4
#   xnbasis <- k_flm
#   ynbasis <- k_flm
#   # names(xnbasis) <- names(trainData$X)
#   train_df = dataConvert(trainData)
#   test_df = dataConvert(testData)
#   flm = if (is.null(model)) {
#     refund::pffr(
#       Y ~
#         ff(NO, splinepars = list(
#           bs="ps", k = c(xnbasis,xnbasis))) +
#         # ff(NO2, splinepars = list(
#         #   bs="ps", k = c(xnbasis,xnbasis))) +
#         ff(O3, splinepars = list(
#           bs="ps", k = c(xnbasis,xnbasis))) +
#         # ff(maxhumid, splinepars = list(
#         #   bs="ps", k = c(xnbasis,xnbasis))) +
#         ff(minhumid, splinepars = list(
#           bs="ps", k = c(xnbasis,xnbasis))) +
#         ff(temp, splinepars = list(
#           bs="ps", k = c(xnbasis,xnbasis))),
#       data = train_df
#     )
#   } else {model}
#   Yhat = predict(flm, newdata = train_df)
#   Ypred = predict(flm, newdata = test_df)
#   list(
#     model = flm,
#     trainErr = mean((Yhat - trainData$Y)^2),
#     testErr = mean((Ypred - testData$Y)^2),
#     yhatTrain = Yhat,
#     yhatTest = Ypred,
#     xnbasis = xnbasis,
#     ynbasis = ynbasis
#   )
# }

# res_flm <- modelFitAndEval(trainData, testData)
# yhat_flm <- res_flm$yhatTrain
# ypred_flm <- res_flm$yhatTest
# se_flm_train <- rowMeans((yhat_flm - trainDataImp$Y)^2)
# se_flm_test <- rowMeans((ypred_flm - testDataImp$Y)^2)
# mse_flm_train <- mean(se_flm_train)
# mse_flm_test <- mean(se_flm_test)
# message("Train error:" , res_flm$trainErr)
# message("Test error:" , res_flm$testErr)
# message("Torch seed:", seed)
# message("Num. basis:", nbasis)
# save(
#   yhat_flm, ypred_flm,
#   se_flm_train, se_flm_test,
#   mse_flm_train, mse_flm_test,
#   file=file.path(
#     save_dir, paste0("FLMres-sd", seed, "-nb", k_flm, ".RData")
#   )
# )
# # saveRDS(res_flm$model, file.path(save_dir, paste0("FLMmodel-sd", seed, "-nb", nbasis, ".rds")))


# fit_flm <- readRDS(file.path(save_dir, paste0("FLMmodel-sd", seed, "-nb", k_flm, ".rds")))
# yhat_flm <- predict(fit_flm, newdata = train_df)
# ypred_flm <- predict(fit_flm, newdata = test_df)
# se_flm_train <- rowMeans((yhat_flm - trainDataImp$Y)^2)
# se_flm_test <- rowMeans((ypred_flm - testDataImp$Y)^2)
# mse_flm_train <- mean(se_flm_train)
# mse_flm_test <- mean(se_flm_test)


# PFR Functional Additive Model (spline-based FAM) -------------------------
message("========== Start refund::pffr for FAM ============")

k_fam <- 11

# k_x <- 7
# modelFitAndEval = function(trainData, testData, model=NULL, ...) {
#   # xnbasis = numeric(length(trainData$X))
#   for (j in seq_along(trainData$X)) {
#     trainData$X[[j]] <- fpcaImpute(trainData$X[[j]], xtgrid[[j]])$X
#     testData$X[[j]] <- fpcaImpute(testData$X[[j]], xtgrid[[j]])$X
#     # xnbasis[j] <- smoothBsplGCV(trainData$X[[j]], xtgrid[[j]], nbasis.min = 7)$nbasis %/% 4
#   }
#   trainData$Y <- fpcaImpute(trainData$Y, ytgrid)$X
#   testData$Y <- fpcaImpute(testData$Y, ytgrid)$X
#   # ynbasis <- smoothBsplGCV(trainData$Y, ytgrid, nbasis.min = 7)$nbasis %/% 4
#   xnbasis <- NA
#   ynbasis <- NA
#   # names(xnbasis) <- names(trainData$X)
#   train_df = dataConvert(trainData)
#   test_df = dataConvert(testData)
#   fam = if (is.null(model)) {
#     refund::pffr(
#       Y ~
#         sff(NO,
#           splinepars = list(bs = "ps", m = c(2,2,2), k = c(k_x, k_fam, k_fam))) +
#         # sff(NO2,
#         #   splinepars = list(bs = "ps", m = c(2,2,2), k = c(k_x, k_fam, k_fam))) +
#         sff(O3,
#           splinepars = list(bs = "ps", m = c(2,2,2), k = c(k_x, k_fam, k_fam))) +
#         # sff(maxhumid,
#         #   splinepars = list(bs = "ps", m = c(2,2,2), k = c(k_x, k_fam, k_fam))),
#         sff(minhumid,
#           splinepars = list(bs = "ps", m = c(2,2,2), k = c(k_x, k_fam, k_fam))) +
#         sff(temp,
#           splinepars = list(bs = "ps", m = c(2,2,2), k = c(k_x, k_fam, k_fam))),
#       data = train_df
#     )
#   } else {model}
#   Yhat = predict(fam, newdata = train_df)
#   Ypred = predict(fam, newdata = test_df)
#   list(
#     model = fam,
#     trainErr = mean((Yhat - trainData$Y)^2),
#     testErr = mean((Ypred - testData$Y)^2),
#     yhatTrain = Yhat,
#     yhatTest = Ypred,
#     xnbasis = xnbasis,
#     ynbasis = ynbasis
#   )
# }

# res_fam <- modelFitAndEval(trainData, testData)
# message("Train error:" , res_fam$trainErr)
# message("Test error:" , res_fam$testErr)
# message("Torch seed:", seed)
# message("Num. basis:", nbasis)
# yhat_fam <- res_fam$yhatTrain
# ypred_fam <- res_fam$yhatTest
# se_fam_train <- rowMeans((yhat_fam - trainDataImp$Y)^2)
# se_fam_test <- rowMeans((ypred_fam - testDataImp$Y)^2)
# mse_fam_train <- mean(res_fam$trainErr)
# mse_fam_test <- mean(res_fam$testErr)
# save(
#   yhat_fam, ypred_fam,
#   se_fam_train, se_fam_test,
#   mse_fam_train, mse_fam_test,
#   file=file.path(
#     save_dir, paste0("FAMres-sd", seed, "-nb", k_fam, ".RData")
#   )
# )
# saveRDS(res_fam$model, file.path(save_dir, paste0("FAMmodel-sd", seed, "-nb", nbasis, ".rds")))


# fit_fam <- readRDS(file.path(save_dir, paste0("FAMmodel-sd", seed, "-nb", k_fam, ".rds")))
# yhat_fam <- predict(fit_fam, newdata = train_df)
# ypred_fam <- predict(fit_fam, newdata = test_df)
# se_fam_train <- rowMeans((yhat_fam - trainDataImp$Y)^2)
# se_fam_test <- rowMeans((ypred_fam - testDataImp$Y)^2)
# mse_fam_train <- mean(se_fam_train)
# mse_fam_test <- mean(se_fam_test)



# Plot comparison ---------------------------------------------------------

load(file.path(save_dir, paste0("FNNres-sd1234-nb", nbasis.fnn, ".RData")))
load(file.path(save_dir, paste0("FLMres-sd1234-nb", k_flm, ".RData")))
load(file.path(save_dir, paste0("FAMres-sd1234-nb", k_fam, ".RData")))

sitesinfo <- readr::read_csv(
  "data/canadian_climate_aq/raw_files/ca_climate_airqlty_2023.csv",
  show_col_types = FALSE
) |> distinct(NAPSID, City, Province)


expm1 <- \(x) exp(x) - 1

# pdf(file.path(save_dir, "aq-compare.pdf"), width = 7, height=3)
pa <- par(no.readonly = TRUE)
colset <- RColorBrewer::brewer.pal(3, "Set1")
perfskey <- order(se_flm_test / se_fnn_test, decreasing = TRUE)
# idx = head(perfskey, 5)[1:3]
# idx = tail(perfskey, 5)[1:3]
idx = c(head(perfskey, 5)[1:2], tail(perfskey, 5)[1])
subj_id <- which(folds == 1)[idx] 
naps_id <- names(dat.PM25 |> select(-Date))[subj_id]
city <- sapply(naps_id, \(i) sitesinfo$City[which(sitesinfo$NAPSID == i)])
# layout: 2 rows × 3 cols; top row=plots (1), bottom row=legend (2)
layout(
  mat = matrix(c(1,2,3,4,4,4),
    nrow = 2, byrow = TRUE),
  heights = c(1, 0.2)  # bottom row shorter
)
# margins for the plots: bottom, left, top, right
# oma for a common left margin (y-axis label)
par(
  mar = c(2, 2, 3, 2),
  oma = c(0, 3, 0, 0)
)
for (jj in 1:3) {
  yj_true <- testDataImp$Y[idx[jj], ]
  yj_fnn <- ypred_fnn[idx[jj], ]
  yj_flm <- ypred_flm[idx[jj], ]
  yj_fam <- ypred_fam[idx[jj], ]
  # rg <- range(c(yj_true, yj_fnn, yj_flm))
  rg <- range(c(yj_true, yj_fnn, yj_flm, yj_fam))
  plot(
    dates, yj_true,
    type = "l", xlab = "", ylab = "",
    col  = 1, lty = 2, lwd = 0.7,
    main = bquote(Site~.(jj)*","~.(city[jj])),
    ylim = rg
  )
  lines(
    dates, yj_fnn,
    lwd = 2, lty = 1, col = colset[1]
  )
  lines(
    dates, yj_flm,
    lwd = 1.5, lty = 1, col = colset[2]
  )
  lines(
    dates, yj_fam,
    lwd = 1.5, lty = 1, col = colset[3]
  )
  lines(
    dates, yj_true,
    col  = 1, lty = 2, lwd = 0.7
  )
  if (jj==1) {
    # common y-axis label
    mtext(
      "log(PM2.5)",
      side   = 2,
      line   = 1.5,
      outer  = TRUE,
      cex.lab= 1.1
    )
  }
}
# bottom row: blank plot + horizontal legend
par(mar = c(0, 0, 0, 0))
plot.new()
legend(
  "center",
  legend = c("True", "FNN", "FLM", "FAM"),
  lty    = c(2,    1,     1,     1),
  lwd    = c(1,    2,     2,     2),
  col = c(1, colset[1], colset[2], colset[3]),
  ncol   = 4,
  bty    = "n",
  xpd    = NA,
  cex    = 1.2
)
# Restore graphics settings
par(pa)
# dev.off()

