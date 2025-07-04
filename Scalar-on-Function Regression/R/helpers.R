
# Variable formatter ----------------------------------------------------

format_func_variates <- function(X) {
  if (!is.list(X)) {
    X = list(X)
  }
  p = length(X)
  n = nrow(X[[1]])
  for (j in seq_along(X)) {
    X[[j]] = as.matrix(X[[j]])
    stopifnot(nrow(X[[j]]) == n)
  }
  return(X)
}

format_t_grid <- function(tgrid, ms, p) {
  if (is.null(tgrid)) {
    tgrid = lapply(ms, \(m) seq(0, 1, length.out = m))
  } else {
    if (!is.list(tgrid)) {
      tgrid = list(tgrid)
    }
    stopifnot(length(tgrid) == p)
    stopifnot(all(sapply(tgrid, is.vector)))
    stopifnot(!any(sapply(tgrid, is.unsorted)))
    stopifnot(all(sapply(tgrid, length) == ms))
  }
  tgrid
}

prepare_basisobj = function(
  basisobj,
  basistype,
  nbasis,
  trange,
  p
) {
  if (!is.null(basisobj)) {
    if (is(basisobj, "basisfd")) {
      basisobj = list(basisobj)
    } else {
      stopifnot(is.list(basisobj))
      stopifnot(all(sapply(basisobj, \(basis) is(basis, "basisfd"))))
    }
    if (length(basisobj) == 1) {
      basisobj = rep(basisobj, p)
    } else {
      stopifnot(length(basisobj) == p)
    }
    nbasis = sapply(basisobj, \(basis) basis$nbasis)
  } else {
    if (length(basistype) == 1) {
      basistype = rep(basistype, p)
    } else {
      stopifnot(length(basistype) == p)
    }
    stopifnot(all(
      basistype %in% c("bspline", "fourier", "monomial")
    ))
    if (length(nbasis) == 1) {
      nbasis = rep(nbasis, p)
    } else {
      stopifnot(length(nbasis) == p)
    }
    basisobj = mapply(
      \(type, rg, nb) {
        do.call(
          paste0("create.", type, ".basis"),
          list(rangeval = rg, nbasis = nb)
        )
      },
      basistype,
      trange,
      nbasis,
      SIMPLIFY = FALSE
    )
  }
  basisobj
}

# Cross-validation Wrapper ------------------------------------------------

cvWrapper = function(
  dataList,
  dataSlicer,
  folds,
  modelFitAndEval,
  savedModels = NULL,
  ...
) {
  # dataList: a named list of predictors and responses
  # dataSlicer: a function that takes dataList and integer IDs and
  #   returns data subset
  # folds: a integer vector of fold IDs
  # modelFitAndEval: a function takes training data,
  #   returns a list(model, trainErr, testErr)
  # ...: arguments for modelFitAndEval
  fold_ids = unique(folds)
  nfold = length(fold_ids)

  trainErr = numeric(nfold)
  testErr = numeric(nfold)
  models = rep(list(NULL), nfold)
  for (k in seq_len(nfold)) {
    message("--------------- Testing on fold:", k, "-------------")
    # split into training and testing
    train_idx <- which(folds != k)
    test_idx <- which(folds == k)

    train_data <- dataSlicer(dataList, train_idx)
    test_data <- dataSlicer(dataList, test_idx)

    res <- modelFitAndEval(
      train_data, test_data,
      model = if (is.null(savedModels)) { NULL } else {savedModels[[k]]},
      ...
    )
    trainErr[k] <- res$trainErr
    testErr[k] <- res$testErr
    models[[k]] <- res$model
  }

  return(list(
    models = models,
    trainErr = trainErr,
    testErr = testErr
  ))
}


# FPCA Imputation --------------------------------------------------------

fpcaImpute = function(X, tgrid=NULL, aggressive=FALSE) {
  X = as.matrix(X)
  n = nrow(X)
  m = ncol(X)
  if (!is.null(tgrid)) {
    tgrid = as.vector(tgrid)
    stopifnot(length(tgrid) == m)
  } else {
    tgrid = seq_len(m)
  }
  na_id = is.na(X)
  fit = fdapace::FPCA(
    Ly = lapply(1:n, \(i) X[i,][!na_id[i,]]),
    Lt = lapply(1:n, \(i) tgrid[!na_id[i,]]),
    optns = list(dataType = "Dense", FVEthreshold = 0.999, usergrid = TRUE)
  )
  Xpred = sweep(fit$xiEst %*% t(fit$phi), 2, fit$mu, "+")
  if (aggressive) {
    X = Xpred
  } else {
    X[is.na(X)] <- Xpred[is.na(X)]
  }
  list(
    X = X,
    fpca = fit
  )
}


# Choose Bspline nbasis with GCV ------------------------------------------

smoothBsplGCV = function(X, tgrid, nbasis.min = 5, nbasis.max = NULL) {
  if (is.null(nbasis.max)) nbasis.max <- length(tgrid) - 1
  min_gcv = Inf
  best_smth = NULL
  gcv.all = c()
  for (nbasis in seq(nbasis.min, nbasis.max)) {
    basis = create.bspline.basis(range(tgrid), nbasis)
    smth = smooth.basis(tgrid, t(X), basis)
    gcv = mean(smth$gcv)
    # message("nbasis:", nbasis, "  GCV:", gcv)
    if (gcv < min_gcv) {
      best_smth <- smth
      min_gcv <- gcv
    }
    gcv.all <- c(gcv.all, gcv)
  }
  list(
    fd = best_smth$fd,
    GCVmin = min_gcv,
    GCV.all = gcv.all,
    nbasis.all = seq(nbasis.min, nbasis.max),
    nbasis = best_smth$fd$basis$nbasis
  )
}



# Other utilities --------------------------------------------------------

write_column <- function(dat, values, name) {
  stopifnot(is.data.frame(dat) || is.matrix(dat) || is.null(dat))
  if (name %in% colnames(dat)) {
    dat[,name] <- values
  } else {
    dat = cbind(dat, values)
    colnames(dat)[ncol(dat)] <- name
  }
  dat
}
