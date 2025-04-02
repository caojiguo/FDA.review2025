library(fda)
library(torch)

fit.fofnn = function(
  Y, X, Z=NULL, xtgrid=NULL, ytgrid=NULL,
  xbasisobj=NULL, xbasistype="bspline", xnbasis=7,
  ybasisobj=NULL, ybasistype="bspline", ynbasis=7,
  lambda=1e-8, Lfd=2,
  hidden_sizes=c(1), act_func="tanh",
  learning_rate=0.01, max_epoch=100,
  verbose=TRUE
) {
  
  ##### argument validation
  # responses
  Y = as.matrix(Y)
  N = nrow(Y)
  ym = ncol(Y)
  Y = .prepare.fd.grid(Y, N)
  
  # functional predictors 
  X = .prepare.fd.grid(X, N)
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
  xtgrid = .prepare.tgrid(xtgrid, ms, p)
  xtrange = mapply(range, xtgrid, SIMPLIFY = FALSE)
  ytgrid = .prepare.tgrid(ytgrid, ym, 1)
  ytrange = mapply(range, ytgrid, SIMPLIFY = FALSE)
  
  # basis functions
  xbasisobj = .prepare.basisobj(
    p, xbasisobj, xbasistype, xnbasis, xtrange)
  xnbasis=  sapply(xbasisobj, \(basis) basis$nbasis)
  xnbasistot = sum(xnbasis)
  xcoef_idx = c(0, cumsum(xnbasis))
  ybasisobj = .prepare.basisobj(
    1, ybasisobj, ybasistype, ynbasis, ytrange)
  ynbasis = sapply(ybasisobj, \(basis) basis$nbasis)
  ynbasistot = sum(ynbasis)
  ycoef_idx = c(0, cumsum(ynbasis))
  
  # # store Gram matrices and penalty matrices
  # xGmats = lapply(xbasisobj, \(basis) torch_tensor(inprod(basis, basis)))
  # xpenmats = lapply(xbasisobj, \(basis) {
  #   torch_tensor(do.call(
  #     "+", lapply(
  #       Lfd, \(nderiv) {
  #         inprod(basis, basis, nderiv, nderiv)
  #       }
  #     )
  #   ))
  # })
  
  yBmat = mapply(
    \(basis, tvals) torch_tensor(eval.basis(tvals, basis)),
    ybasisobj, ytgrid, SIMPLIFY = FALSE)
  
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
  Y_center = lapply(Y, colMeans)
  Y_scale = mapply(
    \(Yj, muj, rg) sqrt(mean((Yj-muj)^2) * diff(rg)),
    Y, Y_center, ytrange)
  Y = mapply(
    \(Yj, muj, sigj) sweep(Yj, 2, muj, "-") / sigj,
    Y, Y_center, Y_scale, SIMPLIFY = FALSE)
  X_center = lapply(X, colMeans)
  X_scale = mapply(
    \(Xj, muj, rg) sqrt(mean((Xj-muj)^2) * diff(rg)),
    X, X_center, xtrange)
  X = mapply(
    \(Xj, muj, sigj) sweep(Xj, 2, muj, "-") / sigj,
    X, X_center, X_scale, SIMPLIFY = FALSE)
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
    xtgrid, X, xbasisobj,
    SIMPLIFY = FALSE
  )
  
  # transformed predictors
  xphi = do.call(
    cbind, mapply(
      \(xfd, basis) inprod(xfd, basis),
      xfds, xbasisobj,
      SIMPLIFY = FALSE
    )
  )
  stopifnot(nrow(xphi) == N)
  stopifnot(ncol(xphi) == xnbasistot)
  
  # formulate tensor inputs and outputs
  input_size = xnbasistot + q
  output_size = ynbasistot
  input = torch_tensor(cbind(xphi, Z))
  Y = lapply(Y, torch_tensor)
  
  ##### build a neural network model
  
  in_fea = input_size
  layers = list()
  
  for (l in seq_along(hidden_sizes)) {
    out_fea = hidden_sizes[l] * ynbasistot
    layers[[length(layers)+1]] = nn_linear(in_fea, out_fea)
    layers[[length(layers)+1]] = get(paste0("nn_", act_func[l]))()
    in_fea = out_fea
  }
  out_fea = output_size
  layers[[length(layers)+1]] = nn_linear(in_fea, out_fea)
  
  model <- do.call(nn_sequential, layers)
  
  ##### Initialize optimizer (important!)
  optimizer <- optim_adam(model$parameters, lr = learning_rate)
  
  ##### model fitting with training history logging
  loss_vec <- numeric(max_epoch)
  if (verbose) cat("Starting training...\n")
  for (epoch in 1:max_epoch) {
    model$train()                 # set model to training mode
    optimizer$zero_grad()         # reset gradients
    
    # Forward pass
    Cpred <- model(input)
    
    loss = 0
    for (k in seq_along(Y)) {
      Ykpred = torch_matmul(
        Cpred[,(ycoef_idx[k]+1):ycoef_idx[k+1]],
        yBmat[[k]]$t()
      )
      loss <- loss + nnf_mse_loss(Ykpred, Y[[k]])
    }
    
    # Base loss (MSE for regression)
    
    # # Extract first layer weights and compute roughness penalty.
    # w <- model$modules[['0']]$weight  # tensor of shape [out_features, in_features]
    # 
    # # Compute roughness penalty for functional weights
    # roughness_penalty <- torch_tensor(0)
    # for (j in seq_len(p)) {
    #   wj = w[,(xcoef_idx[j]+1):xcoef_idx[j+1]]
    #   penj = sum(wj$matmul(penmats[[j]])$mul(wj))
    #   roughness_penalty = roughness_penalty + penj
    # }
    
    # Roughness penalty added to loss
    # loss <- loss + lambda * roughness_penalty
    
    # Backpropagation
    loss$backward()             
    optimizer$step()            
    
    loss_vec[epoch] <- as.numeric(loss$item())
    # Print training loss every 50 epochs (adjust frequency as needed)
    if (verbose && epoch %% 50 == 0) {
      cat(sprintf("Epoch %d: loss = %.4f\n", epoch, loss_vec[epoch]))
    }
  }
  if (verbose) cat("Training completed.\n")
  
  out = list(
    model = model,
    ypred = as_array(model(input)),
    xbasisobj = xbasisobj,
    ybasisobj = ybasisobj,
    ytgrid = ytgrid,
    func_input_size = p,
    scalar_input_size = q,
    func_output_size = length(Y),
    preprocess_params = list(
      'Y' = list(center = Y_center, scale = Y_scale),
      'X' = list(center = X_center, scale = X_scale),
      'Z' = list(center = Z_center, scale = Z_scale)
    ),
    xtgrid = xtgrid,
    ytgrid = ytgrid,
    loss_history = loss_vec  # store training loss history
  )
  
  class(out) = "fofnn.fit"
  return(out)
}


predict.fofnn.fit = function(
  object, X, Z=NULL, xtgrid=NULL, ytgrid=NULL
) {
  
  ##### argument validation
  stopifnot(is(object, "fofnn.fit"))
  
  # functional predictors 
  if (!is.list(X)) {
    X = list(X)
  }
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
  xtgrid = .prepare.tgrid(xtgrid, ms, p)
  xtrange = mapply(range, xtgrid, SIMPLIFY = FALSE)
  if (is.null(ytgrid)) {
    ytgrid = object$ytgrid
  } else {
    ytgrid = .prepare.tgrid(ytgrid, NULL, 1)
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
    X, preproc_params$X$center, preproc_params$X$scale,
    SIMPLIFY = FALSE)
  if (!is.null(Z)) {
    Z = Z |> 
      sweep(2, preproc_params$Z$center, "-") |> 
      sweep(2, preproc_params$Z$scale, "/")
  }
  
  ##### process functional predictors
  xfds = mapply(
    \(tval, x, basis) Data2fd(tval, t(x), basis),
    xtgrid, X, xbasisobj,
    SIMPLIFY = FALSE
  )
  
  # transformed predictors
  xphi = do.call(
    cbind, mapply(
      \(xfd, basis) inprod(xfd, basis),
      xfds, xbasisobj,
      SIMPLIFY = FALSE
    )
  )
  
  yBmat = mapply(
    \(basis, tvals) torch_tensor(eval.basis(tvals, basis)),
    ybasisobj, ytgrid, SIMPLIFY = FALSE)
  
  input = torch_tensor(cbind(xphi, Z))
  Cpred = as_array(object$model(input))
  
  Ypred = mapply(
    \(k) torch_matmul(
      Cpred[,(ycoef_idx[k]+1):ycoef_idx[k+1]],
      yBmat[[k]]$t()
    ), seq_len(object$func_output_size), SIMPLIFY = FALSE
  )

  Y = mapply(
    \(Yj, muj, sigj) sweep(sigj * as_array(Yj), 2, muj, "+"),
    Ypred, preproc_params$Y$center, preproc_params$Y$scale,
    SIMPLIFY = FALSE
  )
  
  return(Y)
}



.prepare.fd.grid = function(X, N=NULL) {
  if (!is.list(X)) {
    X = list(X)
  }
  for (Xj in X) {
    stopifnot(is.matrix(Xj))
    if (!is.null(N)) stopifnot(nrow(Xj) == N)
  }
  return(X)
}


.prepare.tgrid = function(tgrid, ms=NULL, p) {
  if (is.null(tgrid)) {
    tgrid = lapply(ms, \(m) seq(0,1,length.out=m))
  } else {
    if (!is.list(tgrid)) tgrid = list(tgrid)
    stopifnot(length(tgrid) == p)
    stopifnot(all(sapply(tgrid, is.vector)))
    stopifnot(!any(sapply(tgrid, is.unsorted)))
    if (!is.null(ms))
      stopifnot(all(sapply(tgrid, length) == ms))
  }
  return(tgrid)
}

.prepare.basisobj = function(
    p, basisobj, basistype, nbasis, trange) {
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
      \(type, rg, nb) do.call(
        paste0("create.",type,".basis"),
        list(rangeval=rg, nbasis=nb)
      ),
      basistype, trange, nbasis,
      SIMPLIFY = FALSE
    )
  }
  return(basisobj)
}
