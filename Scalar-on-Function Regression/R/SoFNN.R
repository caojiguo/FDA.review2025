source("R/helpers.R")

library(fda)
library(torch)

fit.sofnn = function(
  y,
  X,
  Z = NULL,
  tgrid = NULL,
  basisobj = NULL,
  basistype = "bspline",
  nbasis = 7,
  lambda = 1e-8,
  Lfd = 2,
  hidden_sizes = c(5),
  act_func = "tanh",
  pretrain = NULL,
  learning_rate = 1e-2,
  dropout_rate = 0.2,
  max_epoch = 1000,
  early_stop = FALSE,
  patience = 100,
  msg_period = 100,
  verbose = TRUE
) {
  
  ##### argument validation
  # responses
  y = as.matrix(y)
  N = nrow(y)

  # functional predictors
  X = format_func_variates(X)
  stopifnot(all(sapply(X, \(Xj) nrow(Xj) == N)))
  p = length(X)
  ms = sapply(X, ncol)

  # scalar predictors
  if (is.null(Z)) {
    q = 0
  } else {
    Z = as.matrix(Z)
    stopifnot(nrow(Z) == N)
    q = ncol(Z)
  }

  # domain of functional predictors
  tgrid = format_t_grid(tgrid, ms, p)
  trange = mapply(range, tgrid, SIMPLIFY = FALSE)

  # basis functions
  basisobj = prepare_basisobj(
    basisobj, basistype, nbasis, trange, p)
  nbasis = sapply(basisobj, \(basis) basis$nbasis)
  nbasistot = sum(nbasis)
  xcoef_idx = c(0, cumsum(nbasis))

  # store Gram matrices and penalty matrices
  Gmats = lapply(basisobj, \(basis) torch_tensor(inprod(basis, basis)))
  penmats = lapply(basisobj, \(basis) {
    torch_tensor(do.call(
      "+",
      lapply(
        Lfd,
        \(nderiv) {
          inprod(basis, basis, nderiv, nderiv)
        }
      )
    ))
  })

  # NN settings
  n_hidden_layer = length(hidden_sizes)
  if (length(hidden_sizes) > 0) {
    if (length(act_func) == 1) {
      act_func = rep(act_func, n_hidden_layer)
    } else {
      stopifnot(length(act_func) == n_hidden_layer)
    }
    stopifnot(all(act_func %in% c("relu", "tanh", "sigmoid")))
  }

  ##### preprocess variables
  xfds = mapply(
    \(tval, x, basis) Data2fd(tval, t(x), basis),
    tgrid, X, basisobj,
    SIMPLIFY = FALSE
  )
  xfdMeans = lapply(xfds, mean.fd)
  X_center = mapply(eval.fd, tgrid, xfdMeans, SIMPLIFY = FALSE) |> 
    lapply(c)  # flatten eval.fd's outputs
  X_scale = mapply(
    \(fdobj, muj, G, rg) {
      diff_coefs <- sweep(fdobj$coefs, 1, muj$coefs, "-")
      (diff_coefs * as_array(G) %*% diff_coefs) |> 
        colSums() |> mean() |> sqrt()
    },
    xfds, xfdMeans, Gmats, trange
  )
  X = mapply(
    \(Xj, muj, sigj) sweep(Xj, 2, muj, "-") / sigj,
    X,
    X_center,
    X_scale,
    SIMPLIFY = FALSE
  )

  y_center = colMeans(y)
  y_scale = apply(y, 2, sd)
  y = y |>
    sweep(2, y_center, "-") |>
    sweep(2, y_scale, "/")

  if (!is.null(Z)) {
    Z_center = colMeans(Z)
    Z_scale = apply(Z, 2, sd)
    Z = Z |>
      sweep(2, Z_center, "-") |>
      sweep(2, Z_scale, "/")
  } else {
    Z_center = Z_scale = NULL
  }

  ##### process functional predictors

  xfds = mapply(
    \(tval, x, basis) Data2fd(tval, t(x), basis),
    tgrid, X, basisobj,
    SIMPLIFY = FALSE
  )
  # transformed predictors
  xphi = do.call(
    cbind,
    mapply(
      \(xfd, basis) inprod(xfd, basis),
      xfds,
      basisobj,
      SIMPLIFY = FALSE
    )
  )
  xphi_center = colMeans(xphi)
  xphi_scale = apply(xphi, 2, sd)
  xphi = xphi |> 
    sweep(2, xphi_center, "-") |> 
    sweep(2, xphi_scale, "/")
  stopifnot(nrow(xphi) == N)
  stopifnot(ncol(xphi) == nbasistot)

  # formulate tensor inputs and outputs
  input_size = nbasistot + q
  output_size = ncol(y)
  input = torch_tensor(cbind(xphi, Z))
  y = torch_tensor(y)

  ##### build a neural network model
  
  if (is.null(pretrain)) {
    # create a torch model object
    in_fea = input_size
    layers = list()
  
    for (l in seq_along(hidden_sizes)) {
      out_fea = hidden_sizes[l]
      layers[[length(layers) + 1]] = nn_linear(in_fea, out_fea)
      layers[[length(layers) + 1]] = get(paste0("nn_", act_func[l]))()
      layers[[length(layers) + 1]] <- nn_dropout(dropout_rate)
      in_fea = out_fea
    }
    out_fea = output_size
    layers[[length(layers) + 1]] = nn_linear(in_fea, out_fea)
  
    model <- do.call(nn_sequential, layers)
  } else {
    # if pre-trained model is available:
    # It is better to validate whether model's specifications
    # are consistent with arguments
    # But validation not implemented yet.
    model = pretrain
  }

  ##### Initialize optimizer
  optimizer <- optim_adam(
    model$parameters, lr = learning_rate,
    # amsgrad = TRUE,
    # weight_decay = 1e-5
  )

  ##### model fitting with training history logging
  loss_vec <- numeric(max_epoch)
  best_loss   <- Inf
  no_improve  <- 0
  if (verbose) {
    cat("Starting training...\n")
  }
  for (epoch in 1:max_epoch) {
    model$train() # set model to training mode
    optimizer$zero_grad() # reset gradients

    # Forward pass
    y_pred <- model(input)

    # Base loss (MSE for regression)
    base_loss <- nnf_mse_loss(y_pred, y)

    # Extract first layer weights and compute roughness penalty.
    w <- model$modules[['0']]$weight # tensor of shape [out_features, in_features]

    # Compute roughness penalty for functional weights
    roughness_penalty <- torch_tensor(0)
    for (j in seq_len(p)) {
      wj = w[, (xcoef_idx[j] + 1):xcoef_idx[j + 1]]
      penj = sum(wj$matmul(penmats[[j]])$mul(wj))
      roughness_penalty = roughness_penalty + penj
    }

    # Roughness penalty added to loss
    loss <- base_loss + lambda * roughness_penalty

    # Backpropagation
    loss$backward()
    optimizer$step()

    # Current loss
    loss_value <- loss_vec[epoch] <- as.numeric(loss$item())
    
    # Check stopping criteria
    if (loss_value < best_loss) {
      best_loss  <- loss_value
      no_improve <- 0
    } else {
      no_improve <- no_improve + 1
    }
    
    # Print training loss
    if (verbose && epoch %% msg_period == 0) {
      cat(sprintf("Epoch %d: loss = %.6f\n", epoch, loss_vec[epoch]))
    }
    
    # Early stopping check
    if (early_stop && no_improve >= patience) {
      if (verbose) {
        cat(sprintf(
          "Early stopping at epoch %d (no improvement for %d epochs)\n",
          epoch, patience
        ))
      }
      break
    }
  }
  if (verbose) {
    cat("Training completed.\n")
  }

  # Extract functional weights from first layer for each functional covariate
  w <- model$modules[['0']]$weight
  func_weights = lapply(
    seq_len(p),
    \(j) {
      wj = as_array(w[, (xcoef_idx[j] + 1):xcoef_idx[j + 1]])
      return(fd(t(wj), basisobj[[j]]))
    }
  )
  
  # Fitted training responses
  ypred = torch::as_array(model(input)) |> 
    sweep(2, y_scale, "*") |> 
    sweep(2, y_center, "+")

  out = list(
    model = model,
    optimizer = optimizer,
    func_weights = func_weights,
    ypred = ypred,
    basisobj = basisobj,
    func_input_size = p,
    scalar_input_size = q,
    preprocess_params = list(
      'y' = list(center = y_center, scale = y_scale),
      'X' = list(center = X_center, scale = X_scale),
      'Z' = list(center = Z_center, scale = Z_scale),
      'xphi' = list(center = xphi_center, scale = xphi_scale)
    ),
    tgrid = tgrid,
    loss_history = loss_vec # store training loss history
  )

  class(out) = "sofnn.fit"
  return(out)
}


predict.sofnn.fit = function(
  object,
  X,
  Z = NULL,
  tgrid = NULL
) {
  ##### argument validation
  stopifnot(is(object, "sofnn.fit"))

  # functional predictors
  X = format_func_variates(X)
  N = nrow(X[[1]])
  p = length(X)
  stopifnot(p == object$func_input_size)
  ms = sapply(X, ncol)
  stopifnot(all(ms == sapply(object$tgrid, length)))

  # scalar predictors
  if (is.null(Z)) {
    q = 0
  } else {
    Z = as.matrix(Z)
    stopifnot(nrow(Z) == N)
    q = ncol(Z)
  }
  stopifnot(q == object$scalar_input_size)

  # domain of functional predictors
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
  trange = mapply(range, tgrid, SIMPLIFY = FALSE)

  # basis functions
  basisobj = object$basisobj

  ##### preprocess variables
  preproc_params = object$preprocess_params
  X = mapply(
    \(Xj, muj, sigj) sweep(Xj, 2, muj, "-") / sigj,
    X,
    preproc_params$X$center,
    preproc_params$X$scale,
    SIMPLIFY = FALSE
  )
  if (!is.null(Z)) {
    Z = Z |>
      sweep(2, preproc_params$Z$center, "-") |>
      sweep(2, preproc_params$Z$scale, "/")
  }

  ##### process functional predictors
  xfds = mapply(
    \(tval, x, basis) Data2fd(tval, t(x), basis),
    tgrid,
    X,
    basisobj,
    SIMPLIFY = FALSE
  )

  # transformed predictors
  xphi = do.call(
    cbind,
    mapply(
      \(xfd, basis) inprod(xfd, basis),
      xfds,
      basisobj,
      SIMPLIFY = FALSE
    )
  )
  xphi = xphi |> 
    sweep(2, preproc_params$xphi$center, "-") |> 
    sweep(2, preproc_params$xphi$scale, "/")

  input = torch_tensor(cbind(xphi, Z))
  yhat = as_array(object$model(input))
  y = yhat |>
    sweep(2, preproc_params$y$scale, "*") |>
    sweep(2, preproc_params$y$center, "+")

  return(y)
}

## cv.sofnn: cross-validation wrapper for tuning lambda
# y: response matrix (N x d)
# X: list of functional predictor matrices, each N x m_j
# Z: optional scalar predictors matrix N x q
# tgrid, basisobj, basistype, nbasis, Lfd: same as in fit.sofnn
# lambdas: numeric vector of smoothing parameters to evaluate
# nfolds: number of CV folds
# other arguments passed to fit.sofnn via ...

cv.sofnn <- function(
  y,
  X,
  Z = NULL,
  tgrid = NULL,
  basisobj = NULL,
  basistype = "bspline",
  nbasis = 7,
  lambdas,
  Lfd = 2,
  hidden_sizes = c(5),
  act_func = "tanh",
  learning_rate = 0.01,
  max_epoch = 100,
  early_stop = FALSE,
  patience = 100,
  nfolds = 5,
  folds = NULL,
  msg_period = 100,
  verbose = FALSE
) {
  stopifnot(is.numeric(lambdas), length(lambdas) > 1)
  lambdas = sort(lambdas, decreasing = TRUE)

  ##### argument validation
  # responses
  y = as.matrix(y)
  N = nrow(y)

  # functional predictors
  X = format_func_variates(X)
  stopifnot(all(sapply(X, \(Xj) nrow(Xj) == N)))
  p = length(X)
  ms = sapply(X, ncol)

  # scalar predictors
  if (is.null(Z)) {
    q = 0
  } else {
    Z = as.matrix(Z)
    stopifnot(nrow(Z) == N)
    q = ncol(Z)
  }

  # create fold assignments
  if (is.null(folds)) {
    folds <- sample(rep(seq_len(nfolds), length.out = N))
  } else {
    stopifnot(length(folds) == N)
    stopifnot(all(is.integer(folds)))
    stopifnot(all(folds <= nfolds & folds >= 1))
  }

  # store mean CV error for each lambda
  cv_err <- numeric(length(lambdas))

  for (i in seq_along(lambdas)) {
    if (verbose) {
      message("> lambda:", lambdas[i])
    }
    lambda_val <- lambdas[i]
    fold_errs <- numeric(nfolds)
    if (verbose) {
      cat(sprintf("Evaluating lambda = %g\n", lambda_val))
    }

    for (k in seq_len(nfolds)) {
      if (verbose) {
        message("Validation fold:", k)
      }
      # split into training and validation
      train_idx <- which(folds != k)
      valid_idx <- which(folds == k)

      y_train <- y[train_idx, , drop = FALSE]
      y_valid <- y[valid_idx, , drop = FALSE]

      X_train <- lapply(X, function(Xj) Xj[train_idx, , drop = FALSE])
      X_valid <- lapply(X, function(Xj) Xj[valid_idx, , drop = FALSE])

      if (!is.null(Z)) {
        Z_train <- as.matrix(Z)[train_idx, , drop = FALSE]
        Z_valid <- as.matrix(Z)[valid_idx, , drop = FALSE]
      } else {
        Z_train <- Z_valid <- NULL
      }

      # fit model on training fold
      # TODO: collect args into a list to avoid redundance
      fit_k <- fit.sofnn(
        y = y_train,
        X = X_train,
        Z = Z_train,
        tgrid = tgrid,
        basisobj = basisobj,
        basistype = basistype,
        nbasis = nbasis,
        lambda = lambda_val,
        Lfd = Lfd,
        hidden_sizes = hidden_sizes,
        act_func = act_func,
        pretrain = NULL,
        learning_rate = learning_rate,
        max_epoch = max_epoch,
        early_stop = early_stop,
        patience = patience,
        msg_period = msg_period,
        verbose = FALSE
      )

      # predict on validation fold
      preds <- predict.sofnn.fit(fit_k, X_valid, Z_valid, tgrid)
      # compute MSE
      fold_errs[k] <- mean((preds - y_valid)^2)
      if (verbose) cat(sprintf("  Fold %d MSE: %.5f\n", k, fold_errs[k]))
    }
    cv_err[i] <- mean(fold_errs)
    if (verbose) {
      cat(sprintf("Lambda %.5g CV MSE = %.5f\n", lambda_val, cv_err[i]))
    }
  }

  # select best lambda
  best_idx <- which.min(cv_err)
  best_lambda <- lambdas[best_idx]

  # refit on full data using best lambda
  final_fit <- fit.sofnn(
    y = y,
    X = X,
    Z = Z,
    tgrid = tgrid,
    basisobj = basisobj,
    basistype = basistype,
    nbasis = nbasis,
    lambda = best_lambda,
    Lfd = Lfd,
    hidden_sizes = hidden_sizes,
    act_func = act_func,
    learning_rate = learning_rate,
    max_epoch = max_epoch,
    early_stop = early_stop,
    patience = patience,
    msg_period = msg_period,
    verbose = verbose
  )

  # return results
  return(list(
    cv_error = cv_err,
    min_cv_error = min(cv_err),
    lambdas = lambdas,
    best_lambda = best_lambda,
    fit = final_fit
  ))
}

# Example usage:
# res <- cv.sofnn(y, X, Z, tgrid, basisobj=NULL, basistype="bspline", nbasis=7,
#                lambdas = c(1e-6,1e-4,1e-2,1), nfolds=5,
#                hidden_sizes=c(10,5), max_epoch=200, verbose=TRUE)
# cat("Best lambda:", res$best_lambda, "\n")
