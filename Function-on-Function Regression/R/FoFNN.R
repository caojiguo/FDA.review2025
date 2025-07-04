source("R/helpers.R")

library(fda)
library(torch)

fit.fofnn = function(
  Y,
  X,
  Z = NULL,
  xtgrid = NULL,
  ytgrid = NULL,
  xbasisobj = NULL,
  xbasistype = "bspline",
  xnbasis = 7,
  ybasisobj = NULL,
  ybasistype = "bspline",
  ynbasis = 7,
  xlambda = 1e-8,
  ylambda = 1e-8,
  Lfd = 2,
  hidden_sizes = c(1),
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
  # TODO: only support a single functional resp. for now.
  Y = format_func_variates(Y)
  N = nrow(Y[[1]])
  ym = ncol(Y[[1]])

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
  xtgrid = format_t_grid(xtgrid, ms, p)
  xtrange = mapply(range, xtgrid, SIMPLIFY = FALSE)
  ytgrid = format_t_grid(ytgrid, ym, 1)
  ytrange = mapply(range, ytgrid, SIMPLIFY = FALSE)

  # basis functions
  xbasisobj = prepare_basisobj(
    xbasisobj,
    xbasistype,
    xnbasis,
    xtrange,
    p
  )
  xnbasis = sapply(xbasisobj, \(basis) basis$nbasis)
  xnbasistot = sum(xnbasis)
  xcoef_idx = c(0, cumsum(xnbasis))
  ybasisobj = prepare_basisobj(
    ybasisobj,
    ybasistype,
    ynbasis,
    ytrange,
    1
  )
  ynbasis = sapply(ybasisobj, \(basis) basis$nbasis)
  ynbasistot = sum(ynbasis)
  ycoef_idx = c(0, cumsum(ynbasis))

  # store Gram matrices and penalty matrices
  xGmats = lapply(xbasisobj, \(basis) torch_tensor(inprod(basis, basis)))
  xpenmats = lapply(xbasisobj, \(basis) {
    torch_tensor(do.call(
      "+", lapply(
        Lfd, \(nderiv) inprod(basis, basis, nderiv, nderiv)
      )
    ))
  })
  yGmats = lapply(ybasisobj, \(basis) torch_tensor(inprod(basis, basis)))
  ypenmats = lapply(ybasisobj, \(basis) {
    torch_tensor(do.call(
      "+", lapply(
        Lfd, \(nderiv) inprod(basis, basis, nderiv, nderiv)
      )
    ))
  })

  yBmat = mapply(
    \(basis, tvals) torch_tensor(eval.basis(tvals, basis)),
    ybasisobj,
    ytgrid,
    SIMPLIFY = FALSE
  )

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
    xtgrid, X, xbasisobj,
    SIMPLIFY = FALSE
  )
  xfdMeans = lapply(xfds, mean.fd)
  X_center = mapply(eval.fd, xtgrid, xfdMeans, SIMPLIFY = FALSE) |> 
    lapply(c)  # flatten eval.fd's outputs
  X_scale = mapply(
    \(fdobj, muj, G, rg) {
      diff_coefs <- sweep(fdobj$coefs, 1, muj$coefs, "-")
      (diff_coefs * as_array(G) %*% diff_coefs) |> 
        colSums() |> mean() |> sqrt()
    },
    xfds, xfdMeans, xGmats, xtrange
  )
  X = mapply(
    \(Xj, muj, sigj) sweep(Xj, 2, muj, "-") / sigj,
    X,
    X_center,
    X_scale,
    SIMPLIFY = FALSE
  )
  xfds = mapply(
    \(tval, x, basis) Data2fd(tval, t(x), basis),
    xtgrid, X, xbasisobj,
    SIMPLIFY = FALSE
  )
  
  yfds = mapply(
    \(tval, y, basis) Data2fd(tval, t(y), basis),
    ytgrid, Y, ybasisobj,
    SIMPLIFY = FALSE
  )
  yfdMeans = lapply(yfds, mean.fd)
  Y_center = mapply(eval.fd, ytgrid, yfdMeans, SIMPLIFY = FALSE) |> 
    lapply(c)  # flatten eval.fd's outputs
  Y_scale = mapply(
    \(fdobj, muj, G, rg) {
      diff_coefs <- sweep(fdobj$coefs, 1, muj$coefs, "-")
      (diff_coefs * as_array(G) %*% diff_coefs) |> 
        colSums() |> mean() |> sqrt()
    },
    yfds, yfdMeans, yGmats, ytrange
  )
  Y = mapply(
    \(Yj, muj, sigj) sweep(Yj, 2, muj, "-") / sigj,
    Y,
    Y_center,
    Y_scale,
    SIMPLIFY = FALSE
  )
  yfds = mapply(
    \(tval, y, basis) Data2fd(tval, t(y), basis),
    ytgrid, Y, ybasisobj,
    SIMPLIFY = FALSE
  )
  
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

  # transformed predictors
  xphi = do.call(
    cbind,
    mapply(
      \(xfd, basis) inprod(xfd, basis),
      xfds,
      xbasisobj,
      SIMPLIFY = FALSE
    )
  )
  xphi_center = colMeans(xphi)
  xphi_scale = apply(xphi, 2, sd)
  xphi = xphi |> 
    sweep(2, xphi_center, "-") |> 
    sweep(2, xphi_scale, "/")
  stopifnot(nrow(xphi) == N)
  stopifnot(ncol(xphi) == xnbasistot)

  # formulate tensor inputs and outputs
  input_size = xnbasistot + q
  output_size = ynbasistot
  input = torch_tensor(cbind(xphi, Z))
  Y = lapply(Y, torch_tensor)

  ##### build a neural network model

  if (is.null(pretrain)) {
    in_fea = input_size
    layers = list()
  
    for (l in seq_along(hidden_sizes)) {
      out_fea = hidden_sizes[l] * ynbasistot
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
  
  nlayer = 1 + # output layer
    length(hidden_sizes) * 3  # hidden + act + dropout

  ##### Initialize optimizer (important!)
  optimizer <- optim_adam(model$parameters, lr = learning_rate)

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
    Cpred <- model(input)  # (N, output nbasis)

    # Recover output functional form
    Ypred = lapply(
      seq_along(Y), \(k) {
        torch_matmul(
          Cpred[, (ycoef_idx[k] + 1):ycoef_idx[k + 1]],
          yBmat[[k]]$t()
        )
      }
    )
    
    # Base loss (MSE for regression)
    base_loss = 0
    for (k in seq_along(Y)) {
      base_loss <- base_loss + nnf_mse_loss(Ypred[[k]], Y[[k]])
    }

    # Extract first layer weights and compute roughness penalty.
    w <- model$modules[['0']]$weight  # tensor of shape [out_features, in_features]
    
    # Compute roughness penalty for functional weights
    roughness_penalty <- torch_tensor(0)
    for (j in seq_len(p)) {
      wj = w[,(xcoef_idx[j]+1):xcoef_idx[j+1]]
      penj = sum(wj$matmul(xpenmats[[j]])$mul(wj))
      roughness_penalty = roughness_penalty + xlambda * penj
    }
    
    # Roughness penalty of the output
    for (k in seq_along(Y)) {
      Ck = Cpred[,(ycoef_idx[k]+1):ycoef_idx[k+1]]
      roughness_penalty = roughness_penalty + 
        ylambda * sum(Ck$matmul(ypenmats[[k]])$mul(Ck))
    }

    # Roughness penalty added to loss
    loss <- base_loss + xlambda * roughness_penalty

    # TODO: add penalty for output as well
    # model$modules[[as.character(length(layers)-1)]]$weight

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

  # Predicted output coefficients
  Cpred <- model(input)

  # Recover output functional form
  Ypred = lapply(
    seq_along(Y), \(k) {
      torch_matmul(
        Cpred[, (ycoef_idx[k] + 1):ycoef_idx[k + 1]],
        yBmat[[k]]$t()
      )
    }
  )

  # Rescale outputs back
  Ypred = mapply(
    \(Yk, muk, sigk) {
      sweep(sigk * as_array(Yk), 2, muk, "+")
    }, Ypred, Y_center, Y_scale,
    SIMPLIFY = FALSE
  )

  out = list(
    model = model,
    optimizer = optimizer,
    Ypred = Ypred,
    Cpred = as_array(Cpred),
    xphi = xphi,
    xbasisobj = xbasisobj,
    ybasisobj = ybasisobj,
    ytgrid = ytgrid,
    func_input_size = p,
    scalar_input_size = q,
    func_output_size = length(Y),
    preprocess_params = list(
      'Y' = list(center = Y_center, scale = Y_scale),
      'X' = list(center = X_center, scale = X_scale),
      'Z' = list(center = Z_center, scale = Z_scale),
      'xphi' = list(center = xphi_center, scale = xphi_scale)
    ),
    xtgrid = xtgrid,
    ytgrid = ytgrid,
    loss_history = loss_vec # store training loss history
  )

  class(out) = "fofnn.fit"
  return(out)
}


predict.fofnn.fit = function(
  object,
  X,
  Z = NULL,
  xtgrid = NULL,
  ytgrid = NULL
) {
  ##### argument validation
  stopifnot(is(object, "fofnn.fit"))

  # functional predictors
  X = format_func_variates(X)
  N = nrow(X[[1]])
  for (Xj in X) {
    stopifnot(is.matrix(Xj))
    stopifnot(nrow(Xj) == N)
  }
  p = length(X)
  stopifnot(p == object$func_input_size)
  ms = sapply(X, ncol)
  stopifnot(all(ms == sapply(object$xtgrid, length)))

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
  xtgrid = format_t_grid(xtgrid, ms, p)
  xtrange = mapply(range, xtgrid, SIMPLIFY = FALSE)
  if (is.null(ytgrid)) {
    ytgrid = object$ytgrid
  } else {
    ytgrid = format_t_grid(ytgrid, NULL, 1)
  }
  ytrange = mapply(range, ytgrid, SIMPLIFY = FALSE)

  # basis functions
  xbasisobj = object$xbasisobj
  ybasisobj = object$ybasisobj
  ynbasis = sapply(ybasisobj, \(basis) basis$nbasis)
  ynbasistot = sum(ynbasis)
  ycoef_idx = c(0, cumsum(ynbasis))

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
    xtgrid,
    X,
    xbasisobj,
    SIMPLIFY = FALSE
  )

  # transformed predictors
  xphi = do.call(
    cbind,
    mapply(
      \(xfd, basis) inprod(xfd, basis),
      xfds,
      xbasisobj,
      SIMPLIFY = FALSE
    )
  )
  
  xphi = xphi |> 
    sweep(2, preproc_params$xphi$center, "-") |> 
    sweep(2, preproc_params$xphi$scale, "/")

  yBmat = mapply(
    \(basis, tvals) torch_tensor(eval.basis(tvals, basis)),
    ybasisobj,
    ytgrid,
    SIMPLIFY = FALSE
  )

  input = torch_tensor(cbind(xphi, Z))
  Cpred = as_array(object$model(input))

  Ypred = mapply(
    \(k) {
      torch_matmul(
        Cpred[, (ycoef_idx[k] + 1):ycoef_idx[k + 1]],
        yBmat[[k]]$t()
      )
    },
    seq_len(object$func_output_size),
    SIMPLIFY = FALSE
  )

  Y = mapply(
    \(Yj, muj, sigj) sweep(sigj * as_array(Yj), 2, muj, "+"),
    Ypred,
    preproc_params$Y$center,
    preproc_params$Y$scale,
    SIMPLIFY = FALSE
  )

  return(Y)
}
