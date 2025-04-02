library(fda)
library(torch)

fit.sofnn = function(
  y, X, Z=NULL, tgrid=NULL,
  basisobj=NULL, basistype="bspline", nbasis=7,
  lambda=1e-8, Lfd=2,
  hidden_sizes=c(5), act_func="tanh",
  learning_rate=0.01, max_epoch=100,
  verbose=TRUE
) {
  
  # TODO: allow X's grid to vary between train/test set 
  
  ##### argument validation
  # responses
  y = as.matrix(y)
  n = nrow(y)
  
  # functional predictors 
  if (!is.list(X)) {
    X = list(X)
  }
  for (Xj in X) {
    stopifnot(is.matrix(Xj))
    stopifnot(nrow(Xj) == n)
  }
  p = length(X)
  ms = sapply(X, ncol)
  
  # scalar predictors
  if (is.null(Z)) {
    q = 0
  } else {
    Z = as.matrix(Z)
    stopifnot(nrow(Z) == n)
    q = ncol(Z)
  }
  
  # domain of functional predictors
  if (is.null(tgrid)) {
    tgrid = lapply(ms, \(m) seq(0,1,length.out=m))
  } else {
    if (!is.list(tgrid)) tgrid = list(tgrid)
    stopifnot(length(tgrid) == p)
    stopifnot(all(sapply(tgrid, is.vector)))
    stopifnot(!any(sapply(tgrid, is.unsorted)))
    stopifnot(all(sapply(tgrid, length) == ms))
  }
  trange = mapply(range, tgrid, SIMPLIFY = FALSE)
  
  # basis functions
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
  nbasistot = sum(nbasis)
  xcoef_idx = c(0, cumsum(nbasis))
  
  # store Gram matrices and penalty matrices
  Gmats = lapply(basisobj, \(basis) torch_tensor(inprod(basis, basis)))
  penmats = lapply(basisobj, \(basis) {
    torch_tensor(do.call(
      "+", lapply(
        Lfd, \(nderiv) {
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
  y_center = colMeans(y)
  y_scale = apply(y, 2, sd)
  y = y |> 
    sweep(2, y_center, "-") |> 
    sweep(2, y_scale, "/")
  X_center = lapply(X, colMeans)
  X_scale = mapply(
    \(Xj, muj, rg) sqrt(mean((Xj-muj)^2) * diff(rg)),
    X, X_center, trange)
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
    tgrid, X, basisobj,
    SIMPLIFY = FALSE
  )
  
  # transformed predictors
  xphi = do.call(
    cbind, mapply(
      \(xfd, basis) inprod(xfd, basis),
      xfds, basisobj,
      SIMPLIFY = FALSE
    )
  )
  stopifnot(nrow(xphi) == n)
  stopifnot(ncol(xphi) == nbasistot)
  
  # formulate tensor inputs and outputs
  input_size = nbasistot + q
  output_size = ncol(y)
  input = torch_tensor(cbind(xphi, Z))
  y = torch_tensor(y)
  
  ##### build a neural network model
  
  in_fea = input_size
  layers = list()
  
  for (l in seq_along(hidden_sizes)) {
    out_fea = hidden_sizes[l]
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
    y_pred <- model(input)
    
    # Base loss (MSE for regression)
    base_loss <- nnf_mse_loss(y_pred, y)
    
    # Extract first layer weights and compute roughness penalty.
    w <- model$modules[['0']]$weight  # tensor of shape [out_features, in_features]
    
    # Compute roughness penalty for functional weights
    roughness_penalty <- torch_tensor(0)
    for (j in seq_len(p)) {
      wj = w[,(xcoef_idx[j]+1):xcoef_idx[j+1]]
      penj = sum(wj$matmul(penmats[[j]])$mul(wj))
      roughness_penalty = roughness_penalty + penj
    }
    
    # Roughness penalty added to loss
    loss <- base_loss + lambda * roughness_penalty
    
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
  
  # Extract functional weights from first layer for each functional covariate
  w <- model$modules[['0']]$weight
  func_weights = lapply(
    seq_len(p), \(j) {
      wj = as_array(w[,(xcoef_idx[j]+1):xcoef_idx[j+1]])
      return(fd(t(wj), basisobj[[j]]))
    }
  )
  
  out = list(
    model = model,
    func_weights = func_weights,
    ypred = as_array(model(input)),
    basisobj = basisobj,
    func_input_size = p,
    scalar_input_size = q,
    preprocess_params = list(
      'y' = list(center = y_center, scale = y_scale),
      'X' = list(center = X_center, scale = X_scale),
      'Z' = list(center = Z_center, scale = Z_scale)
    ),
    tgrid = tgrid,
    loss_history = loss_vec  # store training loss history
  )
  
  class(out) = "sofnn.fit"
  return(out)
}


predict.sofnn.fit = function(
  object, X, Z=NULL, tgrid=NULL
) {
  
  ##### argument validation
  stopifnot(is(object, "sofnn.fit"))
  
  # functional predictors 
  if (!is.list(X)) {
    X = list(X)
  }
  n = nrow(X[[1]])
  for (Xj in X) {
    stopifnot(is.matrix(Xj))
    stopifnot(nrow(Xj) == n)
  }
  p = length(X)
  stopifnot(p == object$func_input_size)
  ms = sapply(X, ncol)
  stopifnot(all(ms == sapply(object$tgrid, length)))
  
  # scalar predictors
  if (is.null(Z)) {
    q = 0
  } else {
    Z = as.matrix(Z)
    stopifnot(nrow(Z) == n)
    q = ncol(Z)
  }
  stopifnot(q == object$scalar_input_size)
  
  # domain of functional predictors
  if (is.null(tgrid)) {
    tgrid = lapply(ms, \(m) seq(0,1,length.out=m))
  } else {
    if (!is.list(tgrid)) tgrid = list(tgrid)
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
    tgrid, X, basisobj,
    SIMPLIFY = FALSE
  )
  
  # transformed predictors
  xphi = do.call(
    cbind, mapply(
      \(xfd, basis) inprod(xfd, basis),
      xfds, basisobj,
      SIMPLIFY = FALSE
    )
  )
  
  input = torch_tensor(cbind(xphi, Z))
  yhat = as_array(object$model(input))
  y = yhat |> 
    sweep(2, preproc_params$y$scale, "*") |> 
    sweep(2, preproc_params$y$center, "+")
  
  return(y)
}