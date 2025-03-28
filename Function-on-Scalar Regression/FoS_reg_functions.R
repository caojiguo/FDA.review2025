# Functions defined for manuscript titled Neural Netowrks for Scalar Input and Fucntiaonl Output

### --- Basis Functions --- ###

# 'split' function: Create the training set ID's for splitting purposes
split <- function(n, split.rate, seeds){
  len = length(split.rate)
  if (len > 1){
    train.no = split.rate
  }
  else if(split.rate > 0 &split.rate < 1){
    set.seed(seeds)
    train.no = sample(1:n, round(n*split.rate), replace = F)
  }
  return(train.no)
}

# 'rescale' function: Rescaling inputs of neural networks
rescale <- function(x, scale.type){
  if (scale.type == 0){
    x = scale(x)
  }
  if (scale.type == 1){
    var.no = ncol(x)
    scalar_range = matrix(NA, nrow = var.no, ncol = 2)
    for (i in 1:var.no){
      scalar_range[i, 1] = min(x[,i])
      scalar_range[i, 2] = max(x[,i])
    }
    
    for (i in 1:ncol(x)){
      if(diff(scalar_range[i,])==0){
        x[,i] = (x[,i]-scalar_range[i, 1])
      }else{
        x[,i] = (x[,i]-scalar_range[i, 1])/(diff(scalar_range[i,]))
      }
    }
  }
  return(x)
}

# 'basis.rep' function: Create basis functions and roughness penalty term for smoothing
basis.rep <- function(basis.type, nbasis, norder, tpts){
  if (basis.type == 'B-spline'){
    data.basis = create.bspline.basis(rangeval = range(tpts), nbasis, norder)
    Lfdobj <- int2Lfd(max(0, norder-2))
  }else{
    data.basis = create.fourier.basis(rangeval = range(tpts), nbasis)
    Lfdobj <-vec2Lfd(c(0,(2*pi/diff(range(tpts)))^2,0),range(tpts))
  }
  return(list(data.basis=data.basis, Lfdobj=Lfdobj))
}

# 'plotting' funtion: Create PDF file with plots to compare the raw and predicted curves
plotting <- function(y_raw, y_pred_plot, tpts, plot.tpts){
  matplot(tpts, t(y_raw), type = "l", xlab = "time points (t)", ylab = "Y(t)", 
          main = paste0("Observed Y(t) in Testing Set"))
  matplot(plot.tpts, t(y_pred_plot), type = "l", xlab = "time points (t)", ylab = "Y(t)", 
          main = paste0("Predicted Y(t) in Testing Set") )
  
  if (nrow(y_raw) <= 100){
    plot_samples = sample(1:nrow(y_raw), round(nrow(y_raw)*2/3), replace = F)
    matplot(tpts, t(y_raw)[,plot_samples], type = "l", xlab = "time points (t)", ylab = "Y(t)", 
            main = "Observed Y(t) - Sample")
    matplot(plot.tpts, t(y_pred_plot)[,plot_samples], type = "l", xlab = "time points (t)", ylab = "Y(t)", 
            main = "Predicted Y(t) - Sample" )
    
    obs_samples = sample(1:nrow(y_raw), round(nrow(y_raw)/2), replace = F)
    for (i in obs_samples){
      plot(tpts, (y_raw[i,]), main = paste0("Obs ", i), xlab = "time points (t)", ylab = "Y(t)")
      lines(plot.tpts, y_pred_plot[i,], col = "red", lwd = 2)
    }
  }else{
    for (i in 1:5){
      plot_samples = sample(1:nrow(y_raw), 20, replace = F)
      matplot(tpts, t(y_raw)[,plot_samples], type = "l", xlab = "time points (t)", ylab = "Y(t)", 
              main = paste0("Observed Y(t) - Sample", i))
      matplot(plot.tpts, t(y_pred_plot)[,plot_samples], type = "l", xlab = "time points (t)", ylab = "Y(t)", 
              main = paste0("Predicted Y(t) - Sample", i) )
    }
    
    obs_samples = sample(1:nrow(y_raw), 20, replace = F)
    for (i in obs_samples){
      plot(tpts, (y_raw[i,]), main = paste0("Obs ", i), xlab = "time points (t)", ylab = "Y(t)")
      lines(plot.tpts, y_pred_plot[i,], col = "red", lwd = 2)
    }
  }
}

my.plot.fd <- function(x, y, Lfdobj=0, href=TRUE, titles=NULL,
                       xlim=NULL, ylim=NULL, xlab=NULL,
                       ylab=NULL, ask=FALSE, nx=NULL, axes=NULL, ...) {
  
  #  -----------------------------------------------------------------------
  #       plot for fd class
  #  -----------------------------------------------------------------------
  
  #  Plot a functional data object fdobj.
  #  Arguments:
  #  fdobj     ... a functional data object
  #  Lfdobj    ... linear differental operator to be applied to fdobj before
  #             plotting
  #  HREF   ... If TRUE, a horizontal dotted line through 0 is plotted.
  #  The argument ASK, if TRUE, causes the curves to be displayed one at a time.
  #  NX     ... The number of sampling points to use for
  #             plotting.  (default 101)
  
  #  The remaining optional arguments are the same as those available
  #     in the regular "plot" function.
  
  #  Note that for multivariate fdobj, a suitable matrix of plots
  #    must be set up before calling plot by using something such as
  #    par(mfrow=c(1,nvar),pty="s")
  
  # last modified 16 January 2020
  
  ##
  ## 1.  Basic checks
  ##
  fdobj <- x
  if (!(is.fd(fdobj) || is.fdPar(fdobj)))  stop(paste(
    "First argument is neither a functional data or a ",
    "functional parameter object."))
  if (is.fdPar(fdobj)) fdobj <- fdobj$fd
  
  # process axes
  
  if(is.null(axes)) {
    if(is.null(fdobj$basis$axes)) {
      Axes <- TRUE
      axFun <- FALSE
    } else {
      if(!inherits(fdobj$basis$axes, 'list'))
        stop('fdobj$basis$axes must be a list;  ',
             'class(fdobj$basis$axes) = ', class(fdobj$basis$axes))
      if(!(inherits(fdobj$basis$axes[[1]], 'character') ||
           inherits(fdobj$basis$axes[[1]], 'function') ) )
        stop('fdobj$basis$axes[[1]] must be either a function or the ',
             'name of a function;  class(fdobj$basis$axes[[1]]) = ',
             class(fdobj$basis$axes[[1]]) )
      Axes <- FALSE
      axFun <- TRUE
      axList <- c(fdobj$basis$axes, ...)
    }  
  } else {
    if(is.logical(axes)){
      Axes <- axes
      axFun <- FALSE
    } else {
      if(!inherits(axes, 'list'))
        stop('axes must be a logical or a list;  class(axes) = ',
             class(axes))
      if(!(inherits(axes[[1]], 'character') ||
           inherits(axes[[1]], 'function') ) )
        stop('axes[[1]] must be either a function or the ',
             'name of a function;  class(axes[[1]]) = ',
             class(axes[[1]]) )
      Axes <- FALSE
      axFun <- TRUE
      axList <- c(axes, ...)
    }
  }
  
  #  check Lfdobj
  
  Lfdobj <- int2Lfd(Lfdobj)
  if (!inherits(Lfdobj, "Lfd")) stop(
    "Second argument is not a linear differential operator.")
  
  #  extract dimension information
  
  coef   <- fdobj$coefs
  coefd  <- dim(coef)
  ndim   <- length(coefd)
  # Number of basis functions
  nbasis    <- coefd[1]
  if(is.null(nx)) nx <- max(c(501,10*nbasis + 1))
  # Number of functional observations
  nrep   <- coefd[2]
  if (ndim > 2) nvar <- coefd[3] else nvar <- 1
  
  #  get basis information
  
  basisobj <- fdobj$basis
  rangex   <- basisobj$rangeval
  #  set up a set of argument values for the plot
  
  if (missing(y)) {
    y <- nx
  } else {
    if(is.numeric(y)) y <- as.vector(y)
  }
  
  Y <- y
  if (length(y) == 1) {
    if (y >= 1) {
      y <- seq(rangex[1],rangex[2],len=round(y))
    } else {
      stop("'y' a single number less than one.")
    }
  }
  if (min(y) < rangex[1] || max(y) > rangex[2])
    stop("Values in Y are outside the basis range.")
  if (is.null(xlim)){
    xlim <- rangex
  } else {
    rangex[1] <- max(rangex[1], xlim[1])
    rangex[2] <- min(rangex[2], xlim[2])
    if(length(Y)==1)
      y <- seq(rangex[1],rangex[2],len=round(Y))
  }
  
  #  evaluate LFDOBJ(FDOBJ) at the argument values
  
  fdmat    <- eval.fd(y, fdobj, Lfdobj)
  rangey   <- range(fdmat)
  if (is.null(ylim)) ylim <- rangey
  
  #  set up axis labels and,
  #  optionally, caselabels and variable labels
  
  fdnames      = fdobj$fdnames
  fdlabelslist = fdlabels(fdnames, nrep, nvar)
  
  # Ramsay 2008.08.26
  xlabel    = fdlabelslist$xlabel
  ylabel    = fdlabelslist$ylabel
  casenames = fdlabelslist$casenames
  varnames  = fdlabelslist$varnames
  
  #  check xlab and ylab
  if (is.null(xlab)) xlab <- xlabel
  if (is.null(ylab)) ylab <- ylabel
  #  if (missing(xlab)) xlab <- xlabel
  #  if (missing(ylab)) ylab <- ylabel
  #  crvnames <- fdobj$fdnames[[2]]
  #  varnames <- fdobj$fdnames[[3]]
  # Don't ask for the first plot, but do for later plots if(ask)
  #  op <- par(ask=FALSE)
  # Don't ask for the first plot,
  # but if ask==TRUE, do ask for succeeding plots
  #  on.exit(par(op))
  # A single line?
  
  # A single line?
  if (ndim < 2) {
    plot (y, fdmat, type="l", xlim=xlim, ylim=ylim,
          xlab=xlab, ylab=ylab, axes=Axes, ...)
    if(axFun)
      do.call(axList[[1]], axList[-1])
    #   Ramsay 2008.08.26
    if (zerofind(fdmat) && href) abline(h=0,lty=2)
    #   Graves 2008.07.04
    #    if (zerofind(ylim) && href) abline(h=0,lty=2)
  }
  # Several copies of one function?
  if (ndim ==2 ) {
    if (!ask) {
      matplot(y, fdmat, type="l",
              xlim=xlim,   ylim=ylim,
              xlab=xlab, ylab=ylab, axes=Axes, ...)
      if(axFun)
        do.call(axList[[1]], axList[-1])
      #   Ramsay 2008.08.26
      if (zerofind(fdmat) && href) abline(h=0,lty=2)
      #   Graves 2008.07.04
      #    if (zerofind(ylim) && href) abline(h=0,lty=2)
    } else  {
      #   Graves 2008.07.04:  par, cat absent from Ramsay 2008.08.26
      op <- par(ask=FALSE)
      # Don't ask for the first plot,
      # but if ask==TRUE, do ask for succeeding plots
      on.exit(par(op))
      cat('Multiple plots:  Click in the plot to advance to the next')
      #      op <- par(ask = TRUE)
      #      on.exit(par(op))
      for (irep in 1:nrep) {
        plot (y, fdmat[,irep], type="l",
              xlim=xlim, ylim=ylim,
              xlab=xlab, ylab=ylab, axes=Axes, ...)
        if(axFun)
          do.call(axList[[1]], axList[-1])
        if(irep<2) par(ask=ask)
        
        #        if (zerofind(ylim) && href) abline(h=0,lty=2)
        #        if (!is.null(titles)) title(titles[irep])
        #        else title(paste(crvnames[irep]))
        
        if (!is.null(casenames)) title(casenames[irep])
        else                     title(paste("Case",irep))
        #        if (zerofind(fdmat[,irep]) && href) abline(h=0,lty=2)
        if (zerofind(ylim) && href) abline(h=0,lty=2)
        
        #        mtext("Click in graph to see next plot", side=3, outer=FALSE)
        #        text("",locator(1))
      }
    }
  }
  # Possibly multiple copies of different functions
  if (ndim == 3) {
    if (!ask) {
      for (ivar in 1:nvar) {
        matplot (y, fdmat[,,ivar], type="l",
                 xlim=xlim, ylim=ylim,
                 xlab=xlab, ylab=ylab, ask=FALSE, axes=Axes, ...)
        if(axFun)
          do.call(axList[[1]], axList[-1])
        if (!is.null(varnames)) title(varnames[ivar])
        else                    title(paste("Variable",ivar))
        #        if (zerofind(fdmat[,,ivar]) && href) abline(h=0,lty=2)
        if (zerofind(ylim) && href) abline(h=0,lty=2)
      }
    } else {
      op <- par(ask=FALSE)
      # Don't ask for the first plot,
      # but if ask==TRUE, do ask for succeeding plots
      on.exit(par(op))
      cat('Multiple plots:  Click in the plot to advance to the next')
      
      for (irep in 1:nrep) {
        for (ivar in 1:nvar) {
          plot(y,fdmat[,irep,ivar],type="l",
               xlim=xlim, ylim=ylim,
               xlab=xlab, ylab=ylab, axes=Axes, ...)
          if(axFun)
            do.call(axList[[1]], axList[-1])
          if (!is.null(casenames)) titlestr = casenames[irep]
          else                     titlestr = paste("Case",irep)
          if (!is.null(varnames)) {
            titlestr = paste(titlestr,"  ",varnames[ivar])
          } else {
            titlestr = paste(titlestr,"  ","Variable",ivar)
          }
          title(titlestr)
          #          if (zerofind(fdmat[,irep,ivar]) && href) abline(h=0,lty=2)
          if (zerofind(ylim) && href) abline(h=0,lty=2)
          #          if (!is.null(titles)) title(titles[irep])
          #          else title(paste("Curve", irep, varnames[ivar]))
          
          #          mtext("Click in graph to see next plot", side=3, outer=FALSE)
          #          text("",locator(1))
        }
      }
    }
  }
  #  invisible(NULL)
  # This used to return 'invisible(NULL)'.
  # However, with R 2.7.0 under XEmacs with ESS,
  # it sometimes failed to plot.  I changed the return value,
  # and the problem disappeared.
  'done'
}

# 'missing_indicator' function: Indicate if an obs is missing or not
missing_indicator <- function(y_true, y_pred){
  # Create missing value indicator matrix
  miss_value_ind = !is.na(y_true)
  tpts_wegihts = rowSums(miss_value_ind)
  # Replace NA with 0 in y_true and fill in 0 for the corresponding element in y_pred
  y_pred = y_pred*miss_value_ind
  y_true[is.na(y_true)] <- 0
  
  return(list(y_true = y_true, y_pred = y_pred, weights = tpts_wegihts))
}

# 'basis.lambda.select' function: Select the roughness penalty parameter for basis representaion
basis.lambda.select <- function(data.basis, Lfdobj, tpts, data.y){
  lam.candidate <- 0:15
  nlam = length(lam.candidate)
  
  gcvsave = rep(NA,nlam)
  for (i in 1:nlam) {
    lambda  = lam.candidate[i]
    data.fdPar = fdPar(data.basis, Lfdobj, lambda)
    smoothlist = smooth.basis(tpts, t(data.y), data.fdPar)     
    gcvsave[i] = sum(smoothlist$gcv)
  }
  lambda <- lam.candidate[which.min(gcvsave)]
  return(lambda)
}

### --- Functions for Proposed Methods & Existing Models --- ###

# 'NNBR' function (penalty is optional)
#- data.y = dataset for functional response, dim = #subjects X #time points
#- data.x = dataset for scalar predictors, dim = #subjects X #predictors
#- tpts = vector of observed time points
#- sclae.type = 1 for Normalization (with range [0,1]), 0 for Standardization (Z-score Normalization)
#- basis.type = basis system for functional response, either 'Fourier' or 'B-spline'
#- nbasis = # of basis functions (Fourier: odd + integer only; B-spline: nbasis = length(knots) + norder - 2)
#- norder = # of order (for B-spline basis only)
#- split.rate = proportion of training data
#- val.rate = proportion of validation data taken from training data
#- seeds = for splitting (# of seeds = # of interation)
#- iter = # of iteration
#- activations = list of activation functions for each hidden layers
#- batch.no = batch size used in NN training
#- epochs.no = # of epochs used in NN training
#- optimizer = optimizer used for NN learning
#- early.stopping = True/False, whether to activate early stopping
#- early.patience = an integer, patience value for early stopping process, only active when early.stopping = T
#- penalty = "basis.coef" (penalize directly on basis coefficients, for "B-spline system) 
#            or "2nd.deriv" (penalize on the 2nd derivative)
#- penalty.rate = roughness parameter (+ value)
#- plot = if plot the raw and predicted Y(t)
NNBR <- function(data.x, data.y, tpts=NULL, scale.type=1,
                 basis.type, nbasis, norder,
                 split.rate, val.rate, seed, iter,
                 hidden.nodes, activations, batch.no, epochs.no, optimizer = "adam",
                 early.stopping=F, early.patience, verbose=1,
                 penalty=NULL, penalty.rate=0, plot=F){
  
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
    # Preparation for the roughness penalty
    penalty.tpts.no = 100*tpts.no
    penalty.tpts = seq(0, 1, length.out = penalty.tpts.no)
  }else{
    tpts.no = length(tpts)
    # Preparation for the roughness penalty
    penalty.tpts.no = 100*tpts.no
    penalty.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = penalty.tpts.no)
  }
  
  n = nrow(data.x)
  q = ncol(data.x)
  # Get train.no for training set
  train.no = split(n = n, split.rate = split.rate, seeds = seed[iter])
  
  # Create basis functions for functional response
  basis.rep.result = basis.rep(basis.type, nbasis, norder, tpts)
  data.basis = basis.rep.result$data.basis
  
  # Get basis functions evaluated at all observed time stamps
  basis_fct = t(eval.basis(tpts, data.basis))
  basis_fct_deriv2 = t(eval.basis(penalty.tpts[-1], data.basis, Lfdobj = 2))
  
  # Split training & test sets
  x_train = data.x[train.no,]
  y_train = data.y[train.no,]
  
  x_test = data.x[-train.no,]
  y_test = data.y[-train.no,]
  
  #rescale
  x_scaled = rescale(data.x, scale.type)
  x_train_scaled = x_scaled[train.no,]
  x_test_scaled = x_scaled[-train.no,]
  
  #x_train_scaled = x_train
  #x_test_scaled = x_test
  
  # Define NN model
  hidden.no = length(hidden.nodes)
  model <- keras_model_sequential()
  model%>% 
    layer_dense(units = hidden.nodes[1], activation=activations[1], input_shape = dim(x_train)[2])
  if (hidden.no > 1){for (i in 2:hidden.no){
    model%>%
      layer_dense(units = hidden.nodes[i], activation=activations[i])
  }}
  model%>%  
    layer_dense(units = nbasis, activation='linear') 
  
  # Define objective function
  if(is.null(penalty)){
    myloss <- function(y_true, model_output){
      # Evaluate pred y at 100 obseved t
      y_pred = tf$matmul(model_output, basis_fct)
      # Create missing value indicator matrix
      miss_value_ind = tf$cast(tf$math$logical_not(tf$math$is_nan(y_true)), tf$float32)
      # Replace NA with 0 in y_true and fill in 0 for the corresponding element in y_pred
      y_pred = tf$multiply(y_pred, miss_value_ind)
      y_true = tf$math$multiply_no_nan(y_true, miss_value_ind)
      # Calcuate MSE between true y and pred y
      loss = tf$reduce_mean(tf$square(y_true-y_pred))
      return(loss)
    }
  }else{
    if (basis.type == 'Fourier'){
      myloss <- function(y_true, model_output){
        # Evaluate pred y at 100 obseved t
        y_pred = tf$matmul(model_output, basis_fct)
        # Evaluate pred y at penalty.tpts for the roughness penalty
        y_pred_deriv2 = tf$matmul(model_output, basis_fct_deriv2)
        # Create missing value indicator matrix
        miss_value_ind = tf$cast(tf$math$logical_not(tf$math$is_nan(y_true)), tf$float32)
        # Replace NA with 0 in y_true and fill in 0 for the corresponding element in y_pred
        y_pred = tf$multiply(y_pred, miss_value_ind)
        y_true = tf$math$multiply_no_nan(y_true, miss_value_ind)
        # Calcuate MSE between true y and pred y
        loss = tf$reduce_mean(tf$square(y_true-y_pred)) + 
          penalty.rate*diff(range(penalty.tpts))*(tf$reduce_mean(tf$square(y_pred_deriv2)))
        return(loss)
      }
    }
    if (basis.type == 'B-spline' & penalty == '2nd.deriv'){
      myloss <- function(y_true, model_output){
        # Evaluate pred y at 100 obseved t
        y_pred = tf$matmul(model_output, basis_fct)
        # Evaluate pred y at penalty.tpts for the roughness penalty
        y_pred_deriv2 = tf$matmul(model_output, basis_fct_deriv2)
        # Create missing value indicator matrix
        miss_value_ind = tf$cast(tf$math$logical_not(tf$math$is_nan(y_true)), tf$float32)
        # Replace NA with 0 in y_true and fill in 0 for the corresponding element in y_pred
        y_pred = tf$multiply(y_pred, miss_value_ind)
        y_true = tf$math$multiply_no_nan(y_true, miss_value_ind)
        # Calcuate MSE between true y and pred y
        loss = tf$reduce_mean(tf$square(y_true-y_pred)) + 
          penalty.rate*diff(range(penalty.tpts))*(tf$reduce_mean(tf$square(y_pred_deriv2)))
        return(loss)
      }
    }
    if (basis.type == 'B-spline' & penalty == 'basis.coef'){
      myloss <- function(y_true, model_output){
        # Evaluate pred y at 100 obseved t
        y_pred = tf$matmul(model_output, basis_fct)
        # Create missing value indicator matrix
        miss_value_ind = tf$cast(tf$math$logical_not(tf$math$is_nan(y_true)), tf$float32)
        # Replace NA with 0 in y_true and fill in 0 for the corresponding element in y_pred
        y_pred = tf$multiply(y_pred, miss_value_ind)
        y_true = tf$math$multiply_no_nan(y_true, miss_value_ind)
        # Penalized on the differences of the basis coefficients of adjacent B-splines
        delta_coef_squared = 0
        for (i in 3:nbasis){
          delta_coef_squared = delta_coef_squared + tf$square(tf$reduce_mean(model_output[,i]-2*model_output[,i-1]+model_output[,i-2]))
        }
        # Calcuate MSE between true y and pred y
        loss = tf$reduce_mean(tf$square(y_true-y_pred)) + penalty.rate*delta_coef_squared
        return(loss)
      }
    }
  }
  
  # Compile the model
  if (optimizer == "adam"){model %>% compile(loss=myloss, optimizer=optimizer_adam())}
  if (optimizer == "adamax"){model %>% compile(loss=myloss, optimizer=optimizer_adamax())}
  if (optimizer == "nadam"){model %>% compile(loss=myloss, optimizer=optimizer_nadam())}
  if (optimizer == "sgd"){model %>% compile(loss=myloss, optimizer=optimizer_sgd())}
  
  # Train the model
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = early.patience, mode = "auto")
  if (early.stopping == T){
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no,
      callbacks = list(early_stop), verbose = verbose)
  }else{
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose)
  }
  
  #Evaluate the model
  model_output = model %>% predict(x_test_scaled) # dim(model_output) = no_test.set*nbasis
  y_pred = model_output %*% basis_fct
  #Replacing NA with 0
  #y_test = missing_indicator(y_test, y_pred)$y_true
  #y_pred = missing_indicator(y_test, y_pred)$y_pred
  NNBR.mise = mean((y_test - y_pred)^2, na.rm = T)
  #NNBR.mise = mean(rowMeans((y_test - y_pred)^2, na.rm = T)*tpts.no)
  
  # Plot the raw & predicted Y(t)
  if (plot == TRUE){
    plot.tpts.no = tpts.no*10
    plot.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = plot.tpts.no)
    plot.basis_fct = t(eval.basis(plot.tpts, data.basis))
    y_pred_plot = model_output %*% plot.basis_fct
    
    pdf(paste0("NNBR_PredictionPlot_iter",iter,"_",basis.type,nbasis,"_penalty.rate=",penalty.rate,".pdf"))
    plotting(y_raw =y_test, y_pred_plot=y_pred_plot, tpts=tpts, plot.tpts=plot.tpts)
    dev.off()
  }
  
  K <- backend()
  K$clear_session()
  #######################################
  return(list(train.no = train.no, train.history = history, model = model,
              NNBR.mise = NNBR.mise, y_pred = y_pred, basiscoef_pred= model_output))
}

# 'NNSR.fda' function
#- data.y = dataset for functional response, dim = #subjects X #time points
#- data.x = dataset for scalar predictors, dim = #subjects X #predictors
#- tpts = vector of observed time points
#- sclae.type = 1 for Normalization (with range [0,1]), 0 for Standardization (Z-score Normalization)
#- basis.type = basis system for functional response, either 'Fourier' or 'B-spline'
#- nbasis = # of basis functions (Fourier: odd + integer only; B-spline: nbasis = length(knots) + norder - 2)
#- norder = # of order (for B-spline basis only)
#- split.rate = proportion of training data
#- val.rate = proportion of validation data taken from training data
#- seeds = for splitting (# of seeds = # of interation)
#- iter = # of iteration
#- hidden.nodes = list of nodes for each hidden layers
#- activations = list of activation functions for each hidden layers
#- batch.no = batch size used in NN training
#- epochs.no = # of epochs used in NN training
#- optimizer = optimizer used for NN learning
#- early.stopping = True/False, whether to activate early stopping
#- early.patience = an integer, patience value for early stopping process, only active when early.stopping = T
#- plot = if plot the raw and predicted Y(t)
NNSR.fda <- function(data.x, data.y, tpts=NULL, scale.type=1,
                     basis.type, nbasis, norder, nfpc, explained.var, 
                     split.rate, val.rate, seed, iter,
                     hidden.nodes, activations, batch.no, epochs.no, optimizer = "adam",
                     early.stopping=F, early.patience=10, 
                     verbose=1, plot=F){
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
  }else{
    tpts.no = length(tpts)
  }
  n = nrow(data.x)
  q = ncol(data.x)
  # Get train.no for training set
  train.no = split(n = n, split.rate = split.rate, seeds = seed[iter])
  
  # Create basis functions for functional response
  basis.rep.result = basis.rep(basis.type, nbasis, norder, tpts)
  data.basis = basis.rep.result$data.basis
  Lfdobj = basis.rep.result$Lfdobj
  
  # Get basis functions evaluated at all observed time stamps
  basis_fct = t(eval.basis(tpts, data.basis))
  
  # Select the best lambda for basis representation
  basis.lambda = basis.lambda.select(data.basis, Lfdobj, tpts, data.y)
  data.fdPar = fdPar(data.basis, Lfdobj, lambda=basis.lambda)
  data.smooth = smooth.basis(tpts, t(data.y), data.fdPar)
  data.fd = data.smooth$fd
  data.fd.coef = t(data.smooth$fd$coef)
  
  #data.smoothed = matrix(data = NA, nrow= nrow(data.y),ncol = ncol(data.y))
  #for (i in 1:nrow(data.smoothed)){data.smoothed[i,] = eval.fd(tpts, data.smooth$fd[i])}
  
  ## FPCA 
  train.fpca = pca.fd(data.fd[train.no], nharm = nfpc, harmfdPar = data.fdPar)
  # Get the FPCs
  train.fpca.harms = train.fpca$harmonics$coefs
  # Get the est. mean curve for tpts
  train.fpca.meanfd = eval.fd(tpts, train.fpca$meanfd)
  # Variance explained by top FPCs
  train.fpca.var = vector("logical")
  for (i in 1:length(train.fpca$varprop)){
    train.fpca.var[i] = sum(train.fpca$varprop[1:i])/sum(train.fpca$varprop)
  }
  no.fpc = max(2, min(which(train.fpca.var >= explained.var)))
  
  # Get FPCs and meand fucntions for calculating the true output of NN training 
  fpcs_fct = train.fpca.harms[,1:no.fpc]
  y_train_meanfd = matrix(rep(train.fpca.meanfd, length(train.no)), ncol = length(train.no))
  y_test_meanfd = matrix(rep(train.fpca.meanfd, (n-length(train.no))), ncol = (n-length(train.no)))
  
  # Split training & test sets
  x_train = data.x[train.no,]
  y_train = data.y[train.no,]
  y_train_centered = y_train-t(y_train_meanfd)
  
  x_test = data.x[-train.no,]
  y_test = data.y[-train.no,]
  y_test_centered = y_test-t(y_test_meanfd)
  
  #rescale
  x_scaled = rescale(data.x, scale.type)
  x_train_scaled = x_scaled[train.no,]
  x_test_scaled = x_scaled[-train.no,]
  
  # Define NN model
  hidden.no = length(hidden.nodes)
  model <- keras_model_sequential()
  model%>% 
    layer_dense(units = hidden.nodes[1], activation=activations[1], input_shape = dim(x_train)[2])
  if (hidden.no > 1){for (i in 2:hidden.no){
    model%>%
      layer_dense(units = hidden.nodes[i], activation=activations[i])
  }}
  model%>%  
    layer_dense(units = no.fpc, activation='linear') 
  
  myloss <- function(y_true, model_output){
    # Evaluate pred y at all observed t
    y_obs_fd_coef = tf$matmul(model_output, t(fpcs_fct))
    y_pred_centered = tf$matmul(y_obs_fd_coef, basis_fct)
    #y_pred = tf$matmul(y_obs_fd_coef, basis_fct)+t(train.fpca.meanfd)
    # Calcuate MSE between true y and pred y
    loss = tf$reduce_mean(tf$square(y_true-y_pred_centered))
    return(loss)
  }
  
  # Compile the model
  if (optimizer == "adam"){model %>% compile(loss=myloss, optimizer=optimizer_adam())}
  if (optimizer == "adamax"){model %>% compile(loss=myloss, optimizer=optimizer_adamax())}
  if (optimizer == "nadam"){model %>% compile(loss=myloss, optimizer=optimizer_nadam())}
  if (optimizer == "sgd"){model %>% compile(loss=myloss, optimizer=optimizer_sgd())}
  
  # Train the model
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = early.patience, mode = "auto")
  if (early.stopping == T){
    history <- model %>% fit(
      x = x_train_scaled, y = y_train_centered, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose,
      callbacks = list(early_stop))
  }else{
    history <- model %>% fit(
      x = x_train_scaled, y = y_train_centered, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose)
  }
  
  #Evaluate the model
  model_output = model %>% predict(x_test_scaled)
  y_pred_coef = model_output %*% t(fpcs_fct) #no_test.set x nbasis
  y_pred_centered = y_pred_coef %*% basis_fct
  y_pred = t(y_pred_centered) + y_test_meanfd
  NNSR.fda.mise = mean((y_test_centered - y_pred_centered)^2, na.rm = T)
  #NNSR.fda.mise = mean(rowSums((y_test_centered - y_pred_centered)^2))
  
  
  # Plot the raw & predicted Y(t)
  if (plot == TRUE){
    plot.tpts.no = tpts.no*10
    plot.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = plot.tpts.no)
    plot.basis_fct = t(eval.basis(plot.tpts, data.basis))
    plot.fpca.meanfd = eval.fd(plot.tpts, train.fpca$meanfd)
    plot.y_pred_centered = y_pred_coef %*% plot.basis_fct
    plot.y_pred = plot.y_pred_centered + t(matrix(rep(plot.fpca.meanfd, nrow(y_test)), ncol = nrow(y_test)))
    
    pdf(paste0("NNSR(fda)_PredictionPlot_iter",iter,"_",basis.type,nbasis,".pdf"))
    plotting(y_raw=y_test, y_pred_plot=plot.y_pred, tpts=tpts, plot.tpts=plot.tpts)
    dev.off()
  }
  
  K <- backend()
  K$clear_session()
  #######################################
  return(list(train.no = train.no, train.history = history, model = model,
              NNSR.fda.mise = NNSR.fda.mise, y_pred = y_pred))
}

# 'NNSR.fdapace' function
#- data.y = dataset for functional response, dim = #subjects X #time points
#- data.x = dataset for scalar predictors, dim = #subjects X #predictors
#- tpts = vector of observed time points
#- sclae.type = 1 for Normalization (with range [0,1]), 0 for Standardization (Z-score Normalization)
#- basis.type = basis system for functional response, either 'Fourier' or 'B-spline'
#- nbasis = # of basis functions (Fourier: odd + integer only; B-spline: nbasis = length(knots) + norder - 2)
#- norder = # of order (for B-spline basis only)
#- split.rate = proportion of training data
#- val.rate = proportion of validation data taken from training data
#- seeds = for splitting (# of seeds = # of interation)
#- iter = # of iteration
#- hidden.nodes = list of nodes for each hidden layers
#- activations = list of activation functions for each hidden layers
#- batch.no = batch size used in NN training
#- epochs.no = # of epochs used in NN training
#- optimizer = optimizer used for NN learning
#- early.stopping = True/False, whether to activate early stopping
#- early.patience = an integer, patience value for early stopping process, only active when early.stopping = T
#- plot = if plot the raw and predicted Y(t)
NNSR.fdapace <- function(data.x, data.y, tpts=NULL, scale.type=1,
                         explained.var, split.rate, val.rate, seed, iter,
                         hidden.nodes, activations, batch.no, epochs.no, optimizer = "adam",
                         early.stopping=F, early.patience=10,
                         verbose=1, plot=F){
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
  }else{
    tpts.no = length(tpts)
  }
  n = nrow(data.x)
  q = ncol(data.x)
  # Get train.no for training set
  train.no = split(n = n, split.rate = split.rate, seeds = seed[iter])
  
  train.L3 = MakeFPCAInputs(IDs = rep(train.no, each = tpts.no), tVec = rep(tpts, length(train.no)), t(data.y[train.no,]))
  # FPCA
  train.FPCA = FPCA(train.L3$Ly, train.L3$Lt, list(dataType = 'Dense', nRegGrid = tpts.no, usergrid = T)) 
  # train.FPCA = FPCA(train.L3$Ly, train.L3$Lt, list(dataType = 'Dense', FVEthreshold = explained.var, maxK = nfpc))
  
  # Get the FPCs
  train.FPCA.phi = train.FPCA$phi
  # Get the est. mean curve
  train.FPCA.mean = train.FPCA$mu
  # Variance explained by top FPCs
  train.FPCA.var = train.FPCA$cumFVE/100
  
  # Get selected FPCs evaluated at all observed time stamps
  no.fpc = max(2, min(which(train.FPCA.var >= explained.var)))
  fpcs_fct = t(train.FPCA.phi[,1:no.fpc])
  
  # Split training & test sets
  x_train = data.x[train.no,]
  y_train = data.y[train.no,]
  
  x_test = data.x[-train.no,]
  y_test = data.y[-train.no,]
  
  #rescale
  x_scaled = rescale(data.x, scale.type)
  x_train_scaled = x_scaled[train.no,]
  x_test_scaled = x_scaled[-train.no,]
  
  # Define NN model
  hidden.no = length(hidden.nodes)
  model <- keras_model_sequential()
  model%>% 
    layer_dense(units = hidden.nodes[1], activation=activations[1], input_shape = dim(x_train)[2])
  if (hidden.no > 1){for (i in 2:hidden.no){
    model%>%
      layer_dense(units = hidden.nodes[i], activation=activations[i])
  }}
  model%>%  
    layer_dense(units = no.fpc, activation='linear') 
  
  myloss <- function(y_true, model_output){
    # Evaluate pred y at all observed t
    y_pred = tf$matmul(model_output, fpcs_fct)+train.FPCA.mean
    # Calcuate MSE between true y and pred y
    loss = tf$reduce_mean(tf$square(y_true-y_pred))
    return(loss)
  }
  
  # Compile the model
  if (optimizer == "adam"){model %>% compile(loss=myloss, optimizer=optimizer_adam())}
  if (optimizer == "adamax"){model %>% compile(loss=myloss, optimizer=optimizer_adamax())}
  if (optimizer == "nadam"){model %>% compile(loss=myloss, optimizer=optimizer_nadam())}
  if (optimizer == "sgd"){model %>% compile(loss=myloss, optimizer=optimizer_sgd())}
  
  # Train the model
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = early.patience, mode = "auto")
  if (early.stopping == T){
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose,
      callbacks = list(early_stop))
  }else{
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose)
  }
  
  #Evaluate the model
  model_output = model %>% predict(x_test_scaled)
  y_pred = model_output %*% fpcs_fct +t(matrix(rep(train.FPCA.mean, (n-length(train.no))), ncol=(n-length(train.no))))
  NNSR.fdapace.mise = mean((y_test - y_pred)^2, na.rm = T)
  #NNSR.fdapace.mise = mean(rowSums((y_test - y_pred)^2))
  
  # Plot the raw & predicted Y(t)
  if (plot == TRUE){
    plot.tpts.no = tpts.no*10
    plot.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = plot.tpts.no)
    plot.train.FPCA = FPCA(train.L3$Ly, train.L3$Lt, list(dataType = 'Dense', nRegGrid=plot.tpts.no))
    plot.fpcs_fct = t(plot.train.FPCA$phi[,1:no.fpc])
    plot.FPCA.mean = plot.train.FPCA$mu
    plot.y_pred = model_output %*% plot.fpcs_fct + t(matrix(rep(plot.FPCA.mean, (n-length(train.no))), ncol=(n-length(train.no))))
    
    pdf(paste0("NNSR(fdapace)_PredictionPlot_iter",iter,".pdf"))
    plotting(y_raw=y_test, y_pred_plot=y_pred, tpts=tpts, plot.tpts=tpts)
    #plotting(y_raw=y_test, y_pred_plot=plot.y_pred, tpts=tpts, plot.tpts=plot.tpts)
    dev.off()
  }
  
  K <- backend()
  K$clear_session()
  #######################################
  return(list(train.no = train.no, train.history = history, 
              NNSR.fdapace.mise = NNSR.fdapace.mise,
              y_pred = y_pred))
}

# 'NNBB' function
#- data.y = dataset for functional response, dim = #subjects X #time points
#- data.x = dataset for scalar predictors, dim = #subjects X #predictors
#- tpts = vector of observed time points
#- sclae.type = 1 for Normalization (with range [0,1]), 0 for Standardization (Z-score Normalization)
#- basis.type = basis system for functional response, either 'Fourier' or 'B-spline'
#- nbasis = # of basis functions (Fourier: odd + integer only; B-spline: nbasis = length(knots) + norder - 2)
#- norder = # of order (for B-spline basis only)
#- split.rate = proportion of training data
#- val.rate = proportion of validation data taken from training data
#- seeds = for splitting (# of seeds = # of interation)
#- iter = # of iteration
#- hidden.nodes = list of nodes for each hidden layers
#- activations = list of activation functions for each hidden layers
#- batch.no = batch size used in NN training
#- epochs.no = # of epochs used in NN training
#- optimizer = optimizer used for NN learning
#- early.stopping = True/False, whether to activate early stopping
#- early.patience = an integer, patience value for early stopping process, only active when early.stopping = T
#- plot = if plot the raw and predicted Y(t)
NNBB <- function(data.x, data.y, tpts=NULL, scale.type=1,
                 basis.type, nbasis, norder, 
                 split.rate, val.rate, seed, iter,
                 hidden.nodes, activations, batch.no, epochs.no, optimizer = "adam",
                 early.stopping=F, early.patience=10,
                 verbose=1, plot=F){
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
  }else{
    tpts.no = length(tpts)
  }
  n = nrow(data.x)
  q = ncol(data.x)
  
  # Create basis functions for functional response
  basis.rep.result = basis.rep(basis.type, nbasis, norder, tpts)
  data.basis = basis.rep.result$data.basis
  Lfdobj = basis.rep.result$Lfdobj
  
  # Get basis functions evaluated at all observed time stamps
  basis_fct = t(eval.basis(tpts, data.basis))
  
  # Select the best lambda for basis representation
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
    
    data.smoothed = matrix(data = NA, nrow= nrow(data.y), ncol = ncol(data.y))
    for (i in 1:nrow(data.smoothed)){
      data.smoothed[i,] = eval.fd(tpts, data.smooth$fd[i])
    }
  }
  
  # Get train.no for training set
  train.no = split(n = n, split.rate = split.rate, seeds = seed[iter])
  
  x_train = data.x[train.no,]
  y_train = data.fd.coef[train.no,]
  y_train_obs = data.y[train.no,]
  
  x_test = data.x[-train.no,]
  y_test = data.fd.coef[-train.no,]
  y_test_obs = data.y[-train.no,]
  
  #rescale
  x_scaled = rescale(data.x, scale.type)
  x_train_scaled = x_scaled[train.no,]
  x_test_scaled = x_scaled[-train.no,]
  
  #x_train_scaled = x_train
  #x_test_scaled = x_test
  
  # Define NN model
  hidden.no = length(hidden.nodes)
  model <- keras_model_sequential()%>% 
    layer_dense(units = hidden.nodes[1], activation=activations[1], input_shape = dim(x_train)[2])
  if (hidden.no > 1){for (i in 2:hidden.no){
    model%>%
      layer_dense(units = hidden.nodes[i], activation=activations[i])
  }}
  model%>%  
    layer_dense(units = nbasis, activation='linear') 
  
  # Compile the model
  if (optimizer == "adam"){model %>% compile(loss="mse", optimizer=optimizer_adam())}
  if (optimizer == "adamax"){model %>% compile(loss="mse", optimizer=optimizer_adamax())}
  if (optimizer == "nadam"){model %>% compile(loss="mse", optimizer=optimizer_nadam())}
  if (optimizer == "sgd"){model %>% compile(loss="mse", optimizer=optimizer_sgd())}
  
  # Train the model
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = early.patience, mode = "auto")
  if (early.stopping == T){
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose,
      callbacks = list(early_stop))
  }else{
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose)
  }
  
  #Evaluate the model
  model_output = model %>% predict(x_test_scaled)
  y_pred = model_output %*% basis_fct
  NNBB.mise = mean((y_test_obs - y_pred)^2, na.rm = T)
  #NNBB.mise = mean(rowSums((y_test_obs - y_pred)^2))/tpts.no
  #NNBB.mise = mean(rowSums((y_test_obs - y_pred)^2))
  
  #### Smoothness Plotting
  if (plot == TRUE){
    plot.tpts.no = tpts.no*10
    plot.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = plot.tpts.no)
    plot.basis_fct = t(eval.basis(plot.tpts, data.basis))
    y_pred_plot = model_output %*% plot.basis_fct
    
    pdf(paste0("NNBB_PredictionPlot_iter",iter,"_",basis.type,nbasis,".pdf"))
    plotting(y_raw=y_test_obs, y_pred_plot=y_pred_plot, tpts=tpts, plot.tpts=plot.tpts)
    dev.off()
  }
  
  K <- backend()
  K$clear_session()
  #######################################
  return(list(train.no = train.no, train.history = history, model = model,
              y_pred = y_pred, NNBB.mise = NNBB.mise, basiscoef_pred = model_output))
}

# 'NNSS.fda' function
#- data.y = dataset for functional response, dim = #subjects X #time points
#- data.x = dataset for scalar predictors, dim = #subjects X #predictors
#- tpts = vector of observed time points
#- sclae.type = 1 for Normalization (with range [0,1]), 0 for Standardization (Z-score Normalization)
#- basis.type = basis system for functional response, either 'Fourier' or 'B-spline'
#- nbasis = # of basis functions (Fourier: odd + integer only; B-spline: nbasis = length(knots) + norder - 2)
#- norder = # of order (for B-spline basis only)
#- nfpc = # of principal components to compute
#- explained.var = wanted proportion of variance explained by top FPCs
#- split.rate = proportion of training data
#- val.rate = proportion of validation data taken from training data
#- seeds = for splitting (# of seeds = # of interation)
#- iter = # of iteration
#- hidden.nodes = list of nodes for each hidden layers
#- activations = list of activation functions for each hidden layers
#- batch.no = batch size used in NN training
#- epochs.no = # of epochs used in NN training
#- optimizer = optimizer used for NN learning
#- early.stopping = True/False, whether to activate early stopping
#- early.patience = an integer, patience value for early stopping process, only active when early.stopping = T
#- plot = if plot the raw and predicted Y(t)
NNSS.fda <- function(data.x, data.y, tpts=NULL, scale.type=1,
                     basis.type, nbasis, norder, nfpc, explained.var, 
                     split.rate, val.rate, seed, iter,
                     hidden.nodes, activations, batch.no, epochs.no, optimizer = "adam",
                     early.stopping=F, early.patience=10, 
                     verbose=1, plot=F){
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
  }else{
    tpts.no = length(tpts)
  }
  n = nrow(data.x)
  q = ncol(data.x)
  
  # Create basis functions for functional response
  basis.rep.result = basis.rep(basis.type, nbasis, norder, tpts)
  data.basis = basis.rep.result$data.basis
  Lfdobj = basis.rep.result$Lfdobj
  
  # Get basis functions evaluated at all observed time stamps
  basis_fct = t(eval.basis(tpts, data.basis))
  
  # Select the best lambda for basis representation
  basis.lambda = basis.lambda.select(data.basis, Lfdobj, tpts, data.y)
  data.fdPar = fdPar(data.basis, Lfdobj, lambda=basis.lambda)
  data.smooth = smooth.basis(tpts, t(data.y), data.fdPar)
  data.fd = data.smooth$fd
  data.fd.coef = t(data.smooth$fd$coef)
  
  #data.smoothed = matrix(data = NA, nrow= nrow(data.y),ncol = ncol(data.y))
  #for (i in 1:nrow(data.smoothed)){data.smoothed[i,] = eval.fd(tpts, data.smooth$fd[i])}
  
  # Get train.no for training set
  train.no = split(n = n, split.rate = split.rate, seeds = seed[iter])
  
  ## FPCA 
  train.fpca = pca.fd(data.fd[train.no], nharm = nfpc, harmfdPar = data.fdPar)
  # Get the FPCs
  train.fpca.harms = train.fpca$harmonics$coefs
  # Get the est. mean curve for tpts
  train.fpca.meanfd = eval.fd(tpts, train.fpca$meanfd)
  # Get the FPC socres
  train.fpca.scores = train.fpca$scores
  # Variance explained by top FPCs
  train.fpca.var = vector("logical")
  for (i in 1:length(train.fpca$varprop)){
    train.fpca.var[i] = sum(train.fpca$varprop[1:i])/sum(train.fpca$varprop)
  }
  # Get FPC score (NN output) for NN training 
  no.fpc = max(2, min(which(train.fpca.var >= explained.var)))
  y_train = train.fpca.scores[,1:no.fpc]
  
  # Get the FPC-smoothed Y(t)
  y_obs_fd_coef = y_train %*% t(train.fpca.harms[, 1:no.fpc]) #no_test.set x nbasis
  y_obs_fd = fd(t(y_obs_fd_coef), data.basis, data.fd$fdnames)
  y_centered_obs = eval.fd(tpts, y_obs_fd) #tpts.no x no_test.set
  y_train_obs = t(y_centered_obs + matrix(rep(train.fpca.meanfd, length(train.no)),
                                          ncol = length(train.no))) #no_test.set x tpts.no
  
  # Split training & testing sets
  x_train = data.x[train.no,]
  
  x_test = data.x[-train.no,]
  y_test_obs = data.y[-train.no,]
  
  #rescale
  x_scaled = rescale(data.x, scale.type)
  x_train_scaled = x_scaled[train.no,]
  x_test_scaled = x_scaled[-train.no,]
  
  # Define NN model
  hidden.no = length(hidden.nodes)
  model <- keras_model_sequential()%>% 
    layer_dense(units = hidden.nodes[1], activation=activations[1], input_shape = dim(x_train)[2])
  if (hidden.no > 1){for (i in 2:hidden.no){
    model%>%
      layer_dense(units = hidden.nodes[i], activation=activations[i])
  }}
  model%>%  
    layer_dense(units = no.fpc, activation='linear') 
  
  # Compile the model
  if (optimizer == "adam"){model %>% compile(loss="mse", optimizer=optimizer_adam())}
  if (optimizer == "adamax"){model %>% compile(loss="mse", optimizer=optimizer_adamax())}
  if (optimizer == "nadam"){model %>% compile(loss="mse", optimizer=optimizer_nadam())}
  if (optimizer == "sgd"){model %>% compile(loss="mse", optimizer=optimizer_sgd())}
  
  # Train the model
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = early.patience, mode = "auto")
  if (early.stopping == T){
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose,
      callbacks = list(early_stop))
  }else{
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose)
  }
  
  #Evaluate the model
  model_output = model %>% predict(x_test_scaled) #no_test.set x no.fpc
  y_pred_fd_coef = model_output %*% t(train.fpca.harms[, 1:no.fpc]) #no_test.set x nbasis
  y_pred_fd = fd(t(y_pred_fd_coef), data.basis, data.fd$fdnames)
  y_centered_pred = eval.fd(tpts, y_pred_fd) #tpts.no x no_test.set
  y_pred = t(y_centered_pred + matrix(rep(train.fpca.meanfd, nrow(y_test_obs)), 
                                      ncol = nrow(y_test_obs))) #no_test.set x tpts.no
  
  NNSS.fda.mise = mean((y_test_obs - y_pred)^2, na.rm = T)
  #NNSS.fda.mise = mean(rowSums((y_test_obs - y_pred)^2))
  #NNSS.fda.mise = mean(rowSums((y_test_obs - y_pred)^2))/tpts.no
  
  #### Smoothness Plotting
  if (plot == TRUE){
    plot.tpts.no = tpts.no*10
    plot.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = plot.tpts.no)
    plot.basis_fct = t(eval.basis(plot.tpts, data.basis))
    plot.fpca.meanfd = eval.fd(plot.tpts, train.fpca$meanfd)
    plot.y_pred_centered = y_pred_fd_coef%*% plot.basis_fct
    plot.y_pred = plot.y_pred_centered + t(matrix(rep(plot.fpca.meanfd, nrow(y_test_obs)), 
                                                  ncol = nrow(y_test_obs)))
    
    pdf(paste0("NNSS(fda)_PredictionPlot_iter",iter,"_",basis.type,nbasis,".pdf"))
    plotting(y_raw=y_test_obs, y_pred_plot=plot.y_pred, tpts=tpts, plot.tpts=plot.tpts)
    dev.off()
  }
  
  K <- backend()
  K$clear_session()
  #######################################
  return(list(train.no = train.no, train.history = history, model = model,
              y_pred = y_pred, NNSS.fda.mise = NNSS.fda.mise))
}

# 'NNSS.fdapace' function
#- data.y = dataset for functional response, dim = #subjects X #time points
#- data.x = dataset for scalar predictors, dim = #subjects X #predictors
#- tpts = vector of observed time points
#- sclae.type = 1 for Normalization (with range [0,1]), 0 for Standardization (Z-score Normalization)
#- explained.var = wanted proportion of variance explained by top FPCs
#- split.rate = proportion of training data
#- val.rate = proportion of validation data taken from training data
#- seeds = for splitting (# of seeds = # of interation)
#- iter = # of iteration
#- hidden.nodes = list of nodes for each hidden layers
#- activations = list of activation functions for each hidden layers
#- batch.no = batch size used in NN training
#- epochs.no = # of epochs used in NN training
#- optimizer = optimizer used for NN learning
#- early.stopping = True/False, whether to activate early stopping
#- early.patience = an integer, patience value for early stopping process, only active when early.stopping = T
#- plot = if plot the raw and predicted Y(t)
NNSS.fdapace <- function(data.x, data.y, tpts=NULL, scale.type=1, explained.var,
                         split.rate, val.rate, seed, iter,
                         hidden.nodes, activations, batch.no, epochs.no, optimizer = "adam",
                         early.stopping=F, early.patience=10,
                         verbose=1, plot=F){
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
  }else{
    tpts.no = length(tpts)
  }
  n = nrow(data.x)
  q = ncol(data.x)
  
  # Get train.no for training set
  train.no = split(n = n, split.rate = split.rate, seeds = seed[iter])
  
  train.L3 = MakeFPCAInputs(IDs = rep(train.no, each = tpts.no), tVec = rep(tpts, length(train.no)), t(data.y[train.no,]))
  # FPCA
  train.FPCA = FPCA(train.L3$Ly, train.L3$Lt, list(dataType = 'Dense', nRegGrid = tpts.no, usergrid = T)) 
  # train.FPCA = FPCA(train.L3$Ly, train.L3$Lt, list(dataType = 'Dense', FVEthreshold = explained.var, maxK = nfpc))
  
  # Get the FPCs
  train.FPCA.phi = train.FPCA$phi
  # Get the FPC scores
  train.FPCA.xi = train.FPCA$xiEst
  # Get the est. mean curve
  train.FPCA.mean = train.FPCA$mu
  # Variance explained by top FPCs
  train.FPCA.var = train.FPCA$cumFVE/100
  # Get FPC score (NN output) for NN training 
  no.fpc = max(2, min(which(train.FPCA.var >= explained.var)))
  y_train = train.FPCA.xi[,1:no.fpc]
  
  # Get the FPC-smoothed Y(t)
  y_centered_obs = y_train %*% t(train.FPCA.phi[, 1:no.fpc])
  y_train_obs = y_centered_obs + t(matrix(rep(train.FPCA.mean, length(train.no)),
                                          ncol = length(train.no))) #no_test.set x tpts.no
  
  # Split training & testing sets
  x_train = data.x[train.no,]
  
  x_test = data.x[-train.no,]  
  y_test_obs = data.y[-train.no,]
  
  #rescale
  x_scaled = rescale(data.x, scale.type)
  x_train_scaled = x_scaled[train.no,]
  x_test_scaled = x_scaled[-train.no,]
  
  # Define NN model
  hidden.no = length(hidden.nodes)
  model <- keras_model_sequential()%>% 
    layer_dense(units = hidden.nodes[1], activation=activations[1], input_shape = dim(x_train)[2])
  if (hidden.no > 1){for (i in 2:hidden.no){
    model%>%
      layer_dense(units = hidden.nodes[i], activation=activations[i])
  }}
  model%>%  
    layer_dense(units = no.fpc, activation='linear') 
  
  # Compile the model
  if (optimizer == "adam"){model %>% compile(loss="mse", optimizer=optimizer_adam())}
  if (optimizer == "adamax"){model %>% compile(loss="mse", optimizer=optimizer_adamax())}
  if (optimizer == "nadam"){model %>% compile(loss="mse", optimizer=optimizer_nadam())}
  if (optimizer == "sgd"){model %>% compile(loss="mse", optimizer=optimizer_sgd())}
  
  # Train the model
  early_stop <- callback_early_stopping(monitor = "val_loss", patience = early.patience, mode = "auto")
  if (early.stopping == T){
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose,
      callbacks = list(early_stop))
  }else{
    history <- model %>% fit(
      x = x_train_scaled, y = y_train, validation_split = val.rate,
      batch_size = batch.no, epochs = epochs.no, verbose = verbose)
  }
  
  #Evaluate the model
  model_output = model %>% predict(x_test_scaled) #no_test.set x no.fpc
  y_centered_pred = model_output %*% t(train.FPCA.phi[, 1:no.fpc]) #no_test.set x nbasis
  y_pred = y_centered_pred + t(matrix(rep(train.FPCA.mean, nrow(y_test_obs)), 
                                      ncol = nrow(y_test_obs))) #no_test.set x tpts.no
  NNSS.fdapace.mise = mean((y_test_obs - y_pred)^2, na.rm = T)
  #NNSS.mise = mean(rowSums((y_test_obs - y_pred)^2))/tpts.no
  #NNSS.fdapace.mise = mean(rowSums((y_test_obs - y_pred)^2))
  
  #### Smoothness Plotting
  if (plot == TRUE){
    plot.tpts.no = tpts.no*10
    plot.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = plot.tpts.no)
    plot.train.FPCA = FPCA(train.L3$Ly, train.L3$Lt, list(dataType = 'Dense', nRegGrid=plot.tpts.no))
    plot.fpcs_fct = t(plot.train.FPCA$phi[,1:no.fpc])
    plot.FPCA.mean = plot.train.FPCA$mu
    plot.y_pred = model_output %*% plot.fpcs_fct + t(matrix(rep(plot.FPCA.mean, (n-length(train.no))), ncol=(n-length(train.no))))
    
    pdf(paste0("NNSS(fdapace)_PredictionPlot_iter",iter,".pdf"))
    plotting(y_raw=y_test_obs, y_pred_plot=y_pred, tpts=tpts, plot.tpts=tpts)
    #plotting(y_raw=y_test_obs, y_pred_plot=plot.y_pred, tpts=tpts, plot.tpts=plot.tpts)
    dev.off()
  }
  
  K <- backend()
  K$clear_session()
  #######################################
  return(list(train.no = train.no, train.history = history, y_pred = y_pred, 
              NNSS.fdapace.mise = NNSS.fdapace.mise))
}

# 'fos' function: Function-on-Scalar Regresssion 
#- data.y = dataset for functional response, dim = #subjects X #time points
#- data.x = dataset for scalar predictors, dim = #subjects X #predictors
#- tpts = vector of observed time points
#- basis.type = basis system for functional response, either 'Fourier' or 'B-spline'
#- nbasis = # of basis functions , note nbasis = length(knots) + norder - 2
#- norder = # of order (for B-spline basis)
#- split.rate = proportion of training data
#- seeds = for splitting (# of seeds = # of interation)
#- iter = # of iteration
#- plot = if plot the raw and predicted Y(t)
fos <- function(data.x, data.y, tpts=NULL, basis.type, nbasis, norder, split.rate, seed, iter, plot=F){
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
  }else{
    tpts.no = length(tpts)
  }
  n = nrow(data.x)
  
  data.cov = cbind(1, data.x)
  q = ncol(data.cov)
  
  # Create basis functions for functional response
  basis.rep.result = basis.rep(basis.type, nbasis, norder, tpts)
  data.basis = basis.rep.result$data.basis
  Lfdobj = basis.rep.result$Lfdobj
  
  # Select the best lambda for basis representation
  if (anyNA(data.y)){
    data.fdPar = fdPar(data.basis, Lfdobj, lambda=0)
    
    data.smooth = smooth.basis(tpts[!is.na(data.y[1,])], as.matrix(data.y[1,!is.na(data.y[1,])]), data.fdPar)
    data.fd = data.smooth$fd
    data.fd$fdnames$time = 1:tpts.no
    for (i in 2:n){
      data.smooth = smooth.basis(tpts[!is.na(data.y[i,])], as.matrix(data.y[i,!is.na(data.y[i,])]), data.fdPar)
      data.fd$coefs = cbind(coef(data.fd), coef(data.smooth$fd))
      data.fd$fdnames$reps = append(data.fd$fdnames$reps, paste0("rep", i))
    }
  }else{
    basis.lambda = basis.lambda.select(data.basis, Lfdobj, tpts, data.y)
    data.fdPar = fdPar(data.basis, Lfdobj, lambda=basis.lambda)
    data.smooth = smooth.basis(tpts, t(data.y), data.fdPar)
    data.fd = data.smooth$fd
    data.fd.coef = t(data.smooth$fd$coef)
    
    data.smoothed = matrix(data = NA, nrow= nrow(data.y), ncol = ncol(data.y))
    for (i in 1:nrow(data.smoothed)){
      data.smoothed[i,] = eval.fd(tpts, data.smooth$fd[i])
    }
  }
  
  # Get train.no for training set
  train.no = split(n = n, split.rate = split.rate, seeds = seed[iter])
  
  cov_train = vector("list", q)
  for (i in 1:q){cov_train[[i]] = data.cov[train.no, i]}
  y_train = data.y[train.no,]
  
  cov_test = as.matrix(data.cov[-train.no,])
  y_test = data.y[-train.no,]
  
  # Set up Beta list for Function-on-Scalar Linear Model
  if (basis.type == 'B-spline'){
    beta.basis = create.bspline.basis(rangeval = range(tpts), nbasis, norder)
  }else{
    beta.basis = create.fourier.basis(rangeval = range(tpts), nbasis)
  }
  beta.fdPar = fdPar(beta.basis)
  beta.List = vector("list", q)
  for (j in 1:q){beta.List[[j]] = beta.fdPar}
  
  # Carry out Function-on-Scalar Linear Regression Model with training set
  data.fRegress = fRegress(data.fd[train.no], cov_train, beta.List)
  
  # data.cov = cbind(1, data.x.reduced)
  # if (basis.type == 'B-spline'){
  #   data.fRegress = fosr(fdobj = data.fd[train.no], X = data.cov[train.no,], method = "OLS",
  #                        nbasis = nbasis, norder = norder, pen.order = 2)
  # }else{
  #   data.fRegress = fosr(fdobj = data.fd[train.no], X = data.cov[train.no,], method = "OLS")
  # }
  
  # Prediction on the testing set (Calculate est value of beta on obs time points & then times the covariates)
  eval.beta = matrix(data=NA, nrow = tpts.no, ncol = q)
  for (i in 1:q){
    eval.beta[,i] = eval.fd(tpts, data.fRegress$betaestlist[[i]]$fd) 
  }
  
  test.yhat = cov_test %*% t(eval.beta)
  
  fos.mise = mean((test.yhat - y_test)^2, na.rm = T)
  #fos.mise = mean(rowSums((test.yhat - y_test)^2))/tpts.no
  #fos.mise = mean(rowSums((test.yhat - y_test)^2))
  
  ####################################### 
  # Plot the raw & predicted Y(t)
  if (plot == TRUE){
    plot.tpts.no = tpts.no*10
    plot.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = plot.tpts.no)
    plot.eval.beta = matrix(data=NA, nrow = plot.tpts.no, ncol = q)
    for (i in 1:q){
      plot.eval.beta[,i] = eval.fd(plot.tpts, data.fRegress$betaestlist[[i]]$fd) 
    }
    plot.yhat = cov_test %*% t(plot.eval.beta)
    
    pdf(paste0("FOS_PredictionPlot_iter",iter,"_",basis.type,nbasis,".pdf"))
    plotting(y_raw=y_test, y_pred_plot=plot.yhat, tpts=tpts, plot.tpts=plot.tpts)
    dev.off()
    
    pdf(paste0("Smoothed_Y(t)_", basis.type, "_basis#", nbasis,".pdf"))
    matplot(tpts, t(data.y), type = "l", xlab = "time points (t)", ylab = "Raw Y(t)", 
            main = paste0("Observed Y(t) for all Subjects"))
    my.plot.fd(data.fd, xlab = "time points (t)", ylab = "Smoothed Y(t)", 
               main = "Smoothed Y(t) for all Subjects")
    
    if (nrow(data.y) <= 100){
      plot_samples = sample(1:n, round(n*2/3), replace = F)
      matplot(tpts, t(data.y)[,plot_samples], type = "l", xlab = "time points (t)", ylab = "Raw Y(t)", 
              main = "Observed Y(t) - Sample")
      my.plot.fd(data.fd[plot_samples], xlab = "time points (t)", ylab = "Smoothed Y(t)", 
                 main = "Smoothed Y(t) - Sample")
      
      obs_samples = sample(1:n, round(n/2), replace = F)
      for (i in obs_samples){
        my.plot.fd(data.fd[i], col = "red", lwd = 1.5, main = paste0("Obs ", i), xlab = "time points (t)", ylab = "Y(t)")
        points(tpts, (data.y[i,]))
      }
      #print(paste("Sample", i))
      #print(plot_samples)
    }else{
      for (i in 1:5){
        plot_samples = sample(1:n, 20, replace = F)
        matplot(tpts, t(data.y)[,plot_samples], type = "l", xlab = "time points (t)", ylab = "Raw Y(t)", 
                main = "Observed Y(t) - Sample")
        my.plot.fd(data.fd[plot_samples], xlab = "time points (t)", ylab = "Smoothed Y(t)",
                   main = "Smoothed Y(t) - Sample")
        #print(paste("Sample", i))
        #print(plot_samples)
      }
      
      obs_samples = sample(1:n, 20, replace = F)
      for (i in obs_samples){
        my.plot.fd(data.fd[i], col = 2, lwd = 2, main = paste0("Obs ", i), xlab = "time points (t)", ylab = "Y(t)")
        points(tpts, (data.y[i,]))
      }
    }
    dev.off()
  }
  
  return(list(train.no = train.no, model = data.fRegress, fos.mise = fos.mise, y_pred = test.yhat, beta = eval.beta))
}

# 'fam.iter' function: Functional Additive Mixed Model
#- data.y = dataset for functional response, dim = #subjects X #time points
#- data.x = dataset for scalar predictors, dim = #subjects X #predictors
#- tpts = vector of observed time points
#- spline.basis = type of spline of the smooth class used
#- split.rate = proportion of training data
#- seeds = for splitting (# of seeds = # of interation)
#- iter = # of iteration
#- plot = if plot the raw and predicted Y(t)
fam <- function(data.x, data.y, tpts=NULL, spline.basis='cr', split.rate, seed, iter, plot=F){
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
  }else{
    tpts.no = length(tpts)
  }
  n = nrow(data.x)
  
  # Centered-scale scalar covariates
  data.cov = scale(data.x)
  q = ncol(data.cov)
  
  # Get train.no for training set
  train.no = split(n = n, split.rate = split.rate, seeds = seed[iter])
  
  # Create train data (list)
  train_cov = test_cov = list()
  for (i in 1:q){
    train_cov[[i]] = data.cov[train.no, i]
    test_cov[[i]] = data.cov[-train.no, i]}
  
  # if (is.null(colnames(data.x))){
  #   for (i in 1:length(train_cov)){names(train_cov)[i] = names(test_cov)[i] = paste0("X", i)}
  # }else{
  #   names(train_cov) = names(test_cov) = colnames(data.x)
  # }
  for (i in 1:length(train_cov)){names(train_cov)[i] = names(test_cov)[i] = paste0("X", i)}
  train_data = train_cov
  train_data$Y = data.y[train.no,]
  
  test_data = test_cov
  test_data$Y = y_test = data.y[-train.no,]
  
  # Create model for pffr()
  basis.dim = 10
  if ((length(train_cov)*10) >= length(train.no)){basis.dim = max(3, floor(length(train.no)/length(train_cov))-1)}
  if (length(unique(train_cov[[1]])) > 10){
    pffr_model = paste0("Y~s(", names(train_cov[1]), ", bs='", spline.basis,"', k=", basis.dim, ")")
    #pffr_model = paste0("Y~s(", names(train_cov[1]), ", bs='cr', k=", basis.dim, ")")
  }else if(length(unique(train_cov[[1]])) <= 2){
    pffr_model = paste0("Y~", names(train_cov[1]))
  }else{
    cat.basis.dim = max(3, length(unique(train_cov[[1]])-1))
    pffr_model = paste0("Y~s(", names(train_cov[1]), ", bs='re', k=", cat.basis.dim, ")")
  }
  for (i in 2:length(train_cov)){
    if (length(unique(train_cov[[i]])) > 10){
      pffr_model = paste0(pffr_model, "+s(", names(train_cov[i]), ", bs='", spline.basis,"', k=", basis.dim, ")")
      #pffr_model = paste0(pffr_model, "+s(", names(train_cov[i]), ", bs='cr', k=", basis.dim, ")")
    }else if(length(unique(train_cov[[i]])) <= 2){
      pffr_model = paste0(pffr_model, "+", names(train_cov[i]))
    }else{
      cat.basis.dim = max(3, length(unique(train_cov[[1]])-1))
      pffr_model = paste0(pffr_model, "+s(", names(train_cov[i]), ", bs='re', k=", cat.basis.dim, ")")
    }
  }
  
  # Carry out Functional Additive Mixed Model
  data.pffr <- pffr(as.formula(pffr_model), yind = tpts, data = train_data)
  # Prediction on the testing set (Calculate est value of beta on obs time points & then times the covariates)
  y_pred = predict(data.pffr, test_data)
  
  fam.mise = mean((y_pred- y_test)^2, na.rm = T)
  #fos.mise = mean(rowSums((test.yhat - y_test)^2))/tpts.no
  #fos.mise = mean(rowSums((test.yhat - y_test)^2))
  
  ####################################### 
  # Plot the raw & predicted Y(t)
  if (plot == TRUE){
    plot.tpts.no = tpts.no*10
    plot.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = plot.tpts.no)
    plot.yhat = y_pred
    
    pdf(paste0("FAM_PredictionPlot_iter",iter,".pdf"))
    plotting(y_raw=y_test, y_pred_plot=y_pred, tpts=tpts, plot.tpts=tpts)
    dev.off()
  }
  
  return(list(train.no = train.no, model = data.pffr, fam.mise = fam.mise, y_pred = y_pred, beta = coef(data.pffr)))
}

### --- Functions for Hyperparamater Tunning --- ###

# 'NNBR.pen.cv function: kfolds CV for NNSR with penalty
NNBR.pen.cv <- function(data.x, data.y, tpts=NULL, scale.type = 1, nfolds,
                        basis.type, nbasis, norder, 
                        hidden.nodes, activations, batch.no, epochs.no, optimizer = "adam",
                        early.stopping=F, early.patience=10, verbose=1,
                        penalty, penalty.rate.list){
  if (is.null(tpts)){
    tpts.no = ncol(data.y)
    tpts = seq(0, 1, length.out = tpts.no)
    # Preparation for the roughness penalty
    penalty.tpts.no = 100*tpts.no
    penalty.tpts = seq(0, 1, length.out = penalty.tpts.no)
  }else{
    tpts.no = length(tpts)
    # Preparation for the roughness penalty
    penalty.tpts.no = 100*tpts.no
    penalty.tpts = seq(range(tpts)[1], range(tpts)[2], length.out = penalty.tpts.no)
  }
  
  n = nrow(data.x)
  q = ncol(data.x)
  
  # Create basis functions for functional response
  basis.rep.result = basis.rep(basis.type, nbasis, norder, tpts)
  data.basis = basis.rep.result$data.basis
  
  folds = createFolds(tpts, k = nfolds, list = T, returnTrain = F)
  
  cv.mse = vector("logical", length = length(penalty.rate.list))
  cv.mse.matrix = matrix(NA, nrow = nfolds, ncol = length(penalty.rate.list))
  
  for (k in 1:length(penalty.rate.list)){
    penalty.rate = penalty.rate.list[k]
    test.mise = vector("logical", length = tpts.no)
    
    for (t in 1:nfolds){
      print(paste0( "Fold", t ," (Time points",  toString(folds[[t]]),") is left out."))
      tpts.train = tpts[-folds[[t]]]
      tpts.test = tpts[folds[[t]]]
      
      # Get basis functions evaluated at all observed time stamps
      basis_fct = t(eval.basis(tpts.train, data.basis))
      basis_fct_test = t(eval.basis(tpts.test, data.basis))
      basis_fct_deriv2 = t(eval.basis(penalty.tpts[-1], data.basis, Lfdobj = 2))
      
      # Split training & testing sets
      x_train = x_test = data.x
      y_train = data.y[,-folds[[t]]]
      y_test = data.y[,folds[[t]]]
      
      #rescale
      x_scaled = rescale(data.x, scale.type)
      x_train_scaled = x_test_scaled =  x_scaled
      
      # Define NN model
      hidden.no = length(hidden.nodes)
      model <- keras_model_sequential()
      model%>% 
        layer_dense(units = hidden.nodes[1], activation=activations[1], input_shape = dim(x_train)[2])
      if (hidden.no > 1){for (i in 2:hidden.no){
        model%>%
          layer_dense(units = hidden.nodes[i], activation=activations[i])
      }}
      model%>%  
        layer_dense(units = nbasis, activation='linear') 
      
      if (basis.type == 'Fourier'){
        myloss <- function(y_true, model_output){
          # Evaluate pred y at 100 obseved t
          y_pred = tf$matmul(model_output, basis_fct)
          # Evaluate pred y at penalty.tpts for the roughness penalty
          y_pred_deriv2 = tf$matmul(model_output, basis_fct_deriv2)
          # Create missing value indicator matrix
          miss_value_ind = tf$cast(tf$math$logical_not(tf$math$is_nan(y_true)), tf$float32)
          # Replace NA with 0 in y_true and fill in 0 for the corresponding element in y_pred
          y_pred = tf$multiply(y_pred, miss_value_ind)
          y_true = tf$math$multiply_no_nan(y_true, miss_value_ind)
          # Calcuate MSE between true y and pred y
          loss = tf$reduce_mean(tf$square(y_true-y_pred)) + 
            penalty.rate*diff(range(penalty.tpts))*(tf$reduce_mean(tf$square(y_pred_deriv2)))
          return(loss)
        }
      }
      if (basis.type == 'B-spline' & penalty == '2nd.deriv'){
        myloss <- function(y_true, model_output){
          # Evaluate pred y at 100 obseved t
          y_pred = tf$matmul(model_output, basis_fct)
          # Evaluate pred y at penalty.tpts for the roughness penalty
          y_pred_deriv2 = tf$matmul(model_output, basis_fct_deriv2)
          # Create missing value indicator matrix
          miss_value_ind = tf$cast(tf$math$logical_not(tf$math$is_nan(y_true)), tf$float32)
          # Replace NA with 0 in y_true and fill in 0 for the corresponding element in y_pred
          y_pred = tf$multiply(y_pred, miss_value_ind)
          y_true = tf$math$multiply_no_nan(y_true, miss_value_ind)
          # Calcuate MSE between true y and pred y
          loss = tf$reduce_mean(tf$square(y_true-y_pred)) + 
            penalty.rate*diff(range(penalty.tpts))*(tf$reduce_mean(tf$square(y_pred_deriv2)))
          return(loss)
        }
      }
      if (basis.type == 'B-spline' & penalty == 'basis.coef'){
        myloss <- function(y_true, model_output){
          # Evaluate pred y at 100 obseved t
          y_pred = tf$matmul(model_output, basis_fct)
          # Create missing value indicator matrix
          miss_value_ind = tf$cast(tf$math$logical_not(tf$math$is_nan(y_true)), tf$float32)
          # Replace NA with 0 in y_true and fill in 0 for the corresponding element in y_pred
          y_pred = tf$multiply(y_pred, miss_value_ind)
          y_true = tf$math$multiply_no_nan(y_true, miss_value_ind)
          # Penalized on the differences of the basis coefficients of adjacent B-splines
          delta_coef_squared = 0
          for (i in 3:nbasis){
            delta_coef_squared = delta_coef_squared + tf$square(tf$reduce_mean(model_output[,i]-2*model_output[,i-1]+model_output[,i-2]))
          }
          # Calcuate MSE between true y and pred y
          loss = tf$reduce_mean(tf$square(y_true-y_pred)) + penalty.rate*delta_coef_squared
          return(loss)
        }
      }
      
      # Compile the model
      if (optimizer == "adam"){model %>% compile(loss=myloss, optimizer=optimizer_adam())}
      if (optimizer == "adamax"){model %>% compile(loss=myloss, optimizer=optimizer_adamax())}
      if (optimizer == "nadam"){model %>% compile(loss=myloss, optimizer=optimizer_nadam())}
      if (optimizer == "sgd"){model %>% compile(loss=myloss, optimizer=optimizer_sgd())}
      
      # Train the model
      early_stop <- callback_early_stopping(monitor = "val_loss", patience = early.patience, mode = "auto")
      if (early.stopping == T){
        history <- model %>% fit(
          x = x_train_scaled, y = y_train, validation_split = 0.1,
          batch_size = batch.no, epochs = epochs.no, verbose = verbose,
          callbacks = list(early_stop))
      }else{
        history <- model %>% fit(
          x = x_train_scaled, y = y_train, validation_split = 0.1,
          batch_size = batch.no, epochs = epochs.no, verbose = verbose)
      }
      
      #Evaluate the model
      model_output = model %>% predict(x_test_scaled) # dim(model_output) = no_test.set*nbasis
      y_pred = model_output %*% basis_fct_test
      #test.mise[t] = cv.mse.matrix[t, k] = mean(rowSums((y_test - y_pred)^2))/length(folds[[t]])*tpts.no
      test.mise[t] = cv.mse.matrix[t, k] = mean((y_test - y_pred)^2, na.rm = T)
    } 
    
    cv.mse[k] = mean(test.mise)
    print(paste0( "Penalty.rate candidate", k,"is done."))
  }
  
  output = list(cv.mse, cv.mse.matrix)
  names(output) = c("cv.avg.mse", "cv.mse.matrix")
  
  K <- backend()
  K$clear_session()
  #######################################
  return(list(cv.mse = cv.mse, cv.mse.matrix = cv.mse.matrix))
}

# 'NN.cv' function: kfolds CV for all NN-based models
NN.cv <- function(nfolds, NN.model,
                  data.x, data.y, tpts = NULL, scale.type = 1,
                  basis.type, nbasis, norder=NULL, nfpc=NULL, explained.var=NULL,
                  val.rate, hidden.nodes, activations, batch.no, epochs.no, optimizer = "adam",
                  early.stopping, early.patience,
                  penalty=NULL, penalty.rate=0){
  
  folds = createFolds(data.x[,1], k = nfolds, list = T, returnTrain = F)
  seeds = sample(100:10000, nfolds, replace = F)
  
  # Predictions initialization
  predictions = list()
  true_values = list()
  mise_fold = vector("logical", length = nfolds)
  
  # Looping to run model
  if (NN.model == 'NNBR'){
    for (i in 1:nfolds){
      split.rate = c(1:nrow(data.x))[-folds[[i]]]
      output = NNBR(data.x, data.y, tpts = tpts, scale.type = scale.type, 
                    basis.type = basis.type, nbasis = nbasis, norder = norder,
                    split.rate = split.rate, val.rate = val.rate, seed = seeds, iter = i,
                    hidden.nodes = hidden.nodes, activations = activations, 
                    batch.no = batch.no, epochs.no = epochs.no, optimizer = optimizer,
                    early.stopping = early.stopping, early.patience = early.patience, verbose = 0,
                    penalty = penalty,penalty.rate = penalty.rate, plot = F)
      mise_fold[i] = output$NNBR.mise
      predictions[[i]] = output$y_pred
      true_values[[i]] = data.y[-folds[[i]],]
      #print(paste("Fold", i, "is done!"))
    }
  }
  
  if (NN.model == 'NNSR.fda'){
    for (i in 1:nfolds){
      split.rate = c(1:nrow(data.x))[-folds[[i]]]
      output = NNSR.fda(data.x, data.y, tpts = tpts, scale.type = scale.type, 
                        basis.type=basis.type, nbasis = nbasis, norder = norder,
                        nfpc = nfpc, explained.var = explained.var,
                        split.rate = split.rate, val.rate = val.rate, seed = seeds, iter = i,
                        hidden.nodes = hidden.nodes, activations = activations,
                        batch.no = batch.no, epochs.no = epochs.no, optimizer = optimizer, 
                        early.stopping = early.stopping, early.patience = early.patience,
                        verbose = 0, plot = F)
      mise_fold[i] = output$NNSR.fda.mise
      predictions[[i]] = output$y_pred
      true_values[[i]] = data.y[-folds[[i]],]
      #print(paste("Fold", i, "is done!"))
    }
  }
  
  if (NN.model == 'NNSR.fdapace'){
    for (i in 1:nfolds){
      split.rate = c(1:nrow(data.x))[-folds[[i]]]
      output = NNSR.fdapace(data.x, data.y, tpts = tpts, scale.type = scale.type, explained.var = explained.var,
                            split.rate = split.rate, val.rate = val.rate, seed = seeds, iter = i,
                            hidden.nodes = hidden.nodes, activations = activations,
                            batch.no = batch.no, epochs.no = epochs.no, optimizer = optimizer, 
                            early.stopping = early.stopping, early.patience = early.patience,
                            verbose = 0, plot = F)
      mise_fold[i] = output$NNSR.fdapace.mise
      predictions[[i]] = output$y_pred
      true_values[[i]] = data.y[-folds[[i]],]
      #print(paste("Fold", i, "is done!"))
    }
  }
  
  if (NN.model == 'NNBB'){
    for (i in 1:nfolds){
      split.rate = c(1:nrow(data.x))[-folds[[i]]]
      output = NNBB(data.x, data.y, tpts = tpts, scale.type = scale.type, 
                    basis.type=basis.type, nbasis = nbasis, norder = norder, 
                    split.rate = split.rate, val.rate = val.rate, seed = seeds, iter = i,
                    hidden.nodes = hidden.nodes, activations = activations,
                    batch.no = batch.no, epochs.no = epochs.no, optimizer = optimizer, 
                    early.stopping = early.stopping, early.patience = early.patience,
                    verbose = 0, plot = F)
      mise_fold[i] = output$NNBB.mise
      predictions[[i]] = output$y_pred
      true_values[[i]] = data.y[-folds[[i]],]
      #print(paste("Fold", i, "is done!"))
    }
  }
  
  if (NN.model == 'NNSS.fda'){
    for (i in 1:nfolds){
      split.rate = c(1:nrow(data.x))[-folds[[i]]]
      output = NNSS.fda(data.x, data.y, tpts = tpts, scale.type = scale.type, 
                       basis.type=basis.type, nbasis = nbasis, norder = norder,
                       nfpc = nfpc, explained.var = explained.var,
                       split.rate = split.rate, val.rate = val.rate, seed = seeds, iter = i,
                       hidden.nodes = hidden.nodes, activations = activations,
                       batch.no = batch.no, epochs.no = epochs.no, optimizer = optimizer, 
                       early.stopping = early.stopping, early.patience = early.patience,
                       verbose = 0, plot = F)
      mise_fold[i] = output$NNSS.fda.mise
      predictions[[i]] = output$y_pred
      true_values[[i]] = data.y[-folds[[i]],]
      #print(paste("Fold", i, "is done!"))
    }
  }
  
  if (NN.model == 'NNSS.fdapace'){
    for (i in 1:nfolds){
      split.rate = c(1:nrow(data.x))[-folds[[i]]]
      output = NNSS.fdapace(data.x, data.y, tpts = tpts, scale.type = scale.type, explained.var = explained.var,
                                    split.rate = split.rate, val.rate = val.rate, seed = seeds, iter = i,
                                    hidden.nodes = hidden.nodes, activations = activations,
                                    batch.no = batch.no, epochs.no = epochs.no, optimizer = optimizer, 
                                    early.stopping = early.stopping, early.patience = early.patience,
                                    verbose = 0, plot = F)
      mise_fold[i] = output$NNSS.fdapace.mise
      predictions[[i]] = output$y_pred
      true_values[[i]] = data.y[-folds[[i]],]
      #print(paste("Fold", i, "is done!"))
    }
  }
  
  mise_mean = mean(mise_fold)
  mise_sd = sd(mise_fold)
  mise_overall = c(mise_mean, mise_sd)
  names(mise_overall) = c("Mean*", "SD")
  
  K <- backend()
  K$clear_session()
  
  return(list(mise = list(Overall_mise = mise_overall, Fold_mise = mise_fold), fold_indices = folds))
}

# 'NN.param.tune' funciton: Cross validation for NN hyperparameter
NN.param.tune <-function(tune.list, nfolds, NN.model,
                         data.x, data.y, tpts = NULL, scale.type = 1, basis.type, norder=NULL, nfpc=NULL,
                         early.stopping, penalty=NULL, penalty.rate=0){
  
  # Set up function
  tune.fct <- function(x, nfolds, NN.model,
                       data.x, data.y, tpts, scale.type, basis.type, norder, nfpc,
                       early.stopping, penalty, penalty.rate){
    
    # Clearing irrelevant information
    colnames(x) <- NULL
    rownames(x) <- NULL
    
    if (length(x) == (current.layers+7)){
      explained.var.choice = as.numeric(as.character(x[current.layers+7]))
    }
    if (length(x) == (current.layers+6)){
      explained.var.choice = NULL
    }
    
    model.results = NN.cv(nfolds = nfolds, NN.model = NN.model,
                          data.x = data.x, data.y = data.y, tpts = tpts, 
                          scale.type = scale.type,
                          basis.type = basis.type,
                          nbasis = as.numeric(as.character(x[current.layers+1])), 
                          norder = norder, 
                          nfpc = nfpc,
                          explained.var = explained.var.choice,
                          hidden.nodes = current.hidden.nodes, 
                          val.rate = as.numeric(as.character(x[current.layers+4])), 
                          activations = as.character(x[1:current.layers]), 
                          batch.no = as.numeric(as.character(x[current.layers+6])), 
                          epochs.no = as.numeric(as.character(x[current.layers+2])), 
                          optimizer = as.character(x[current.layers+3]),
                          early.stopping = early.stopping, 
                          early.patience = as.numeric(as.character(x[current.layers+5])),
                          penalty = penalty, 
                          penalty.rate = penalty.rate)
    
    list.returned = list(mise = model.results$mise,
                         hidden.layers = current.layers,
                         hidden.nodes = current.hidden.nodes, 
                         activations = as.character(x[1:current.layers]), 
                         nbasis = as.numeric(as.character(x[current.layers+1])),
                         epochs.no = as.numeric(as.character(x[current.layers+2])), 
                         optimizer = as.character(x[current.layers+3]),
                         val.rate = as.numeric(as.character(x[current.layers+4])), 
                         early.patience = as.numeric(as.character(x[current.layers+5])),
                         batch.no = as.numeric(as.character(x[current.layers+6])),
                         explained.var = explained.var.choice)
    
    # Clearing backend
    K <- backend()
    K$clear_session()
    #print(paste(as.numeric(as.character(x[current.layers+1])), "basis functions is done."))
    return(list.returned)
  }
  
  # Saving mise & grid info
  hidden.no = vector()
  grid.list.per.hidden = list()
  mise.list.per.hidden = list()
  mise.min.per.hidden = list()
  
  for (i in 1:length(tune.list$hidden.nodes)){
    
    # Details of current hidden layer(s)
    current.layers = hidden.no[i] = length(tune.list$hidden.nodes[[i]])
    current.hidden.nodes = tune.list$hidden.nodes[[i]]
    
    # Create data frame for activation fcts for current hidden layer(s)
    df.activation = expand.grid(rep(list(tune.list$activations.choice), current.layers), stringsAsFactors = F)
    
    # Getting Grid
    if (is.null(tune.list$explained.var)){
      pre.grid = expand.grid(df.activation$Var1, 
                             tune.list$nbasis, 
                             tune.list$epochs.no, 
                             tune.list$optimizer,
                             tune.list$val.rate,
                             tune.list$early.patience,
                             tune.list$batch.no)
    }else{
      pre.grid = expand.grid(df.activation$Var1, 
                             tune.list$nbasis, 
                             tune.list$epochs.no, 
                             tune.list$optimizer,
                             tune.list$val.rate,
                             tune.list$early.patience,
                             tune.list$batch.no,
                             tune.list$explained.var)
    }
    
    
    final.grid = suppressWarnings(unique(merge(df.activation, pre.grid, by = "Var1")))
    grid.list.per.hidden[[i]] = final.grid
    
    # Pass combinations to the model tune.fct
    results = pbapply(final.grid, 1, tune.fct,
                      nfolds = nfolds, 
                      NN.model = NN.model,
                      data.x = data.x, 
                      data.y = data.y,
                      tpts = tpts,
                      scale.type = scale.type,
                      basis.type = basis.type, 
                      norder = norder, 
                      nfpc = nfpc,
                      early.stopping = early.stopping, 
                      penalty = penalty, 
                      penalty.rate = penalty.rate)
    
    # Collect results
    mise_val = c()
    for (k in 1:length(results)){
      mise_val[k] = as.vector(results[[k]]$mise$Overall_mise[1])
    }
    
    mise.list.per.hidden[[i]] = results
    # The minimum error
    mise.min.per.hidden[[i]] =results[[which.min(mise_val)]]
    print(paste("Tuning complete for:", current.layers, "hidden layers with nodes", toString(current.hidden.nodes)))
    
  }
  
  mise.min = c()
  for (i in 1:length(tune.list$hidden.nodes)){
    mise.min[i] = mise.min.per.hidden[[i]]$mise$Overall_mise[1]
  }
  
  mise.min.overall = which.min(mise.min)
  
  K <- backend()
  K$clear_session()
  
  return(list(best.set = mise.min.per.hidden[[mise.min.overall]],
              all.info = mise.list.per.hidden,
              best.set.per.hideen = mise.min.per.hidden,
              grid.list = grid.list.per.hidden))
}

# 'fos.param.tune' function: Cross validation for fos hyperparameter (nbasis)
fos.param.tune <- function(tune.list, nfolds, data.x, data.y, tpts = NULL, norder=NULL){
  
  min_mise_per_basis = list()
  all_mise_per_basis = list()
  all_mise_sd_per_basis = list()
  for (k in 1:length(tune.list$basis.type.choice)){
    # Details of current basis type
    current.basis = tune.list$basis.type.choice[k]
    mise_mean_per_nbasis = c()
    mise_sd_per_nbasis = c()
    for (j in 1:length(tune.list$nbasis.choice[[k]])){
      current.nbasis = tune.list$nbasis.choice[[k]][j]
      folds = createFolds(data.x[,1], k = nfolds, list = T, returnTrain = F)
      seeds = sample(100:10000, nfolds, replace = F)
      
      mise_fold = vector("logical", length = nfolds)
      # Looping to run model
      for (i in 1:nfolds){
        split.rate = c(1:nrow(data.x))[-folds[[i]]]
        output = suppressWarnings(fos(data.x = data.x, data.y = data.y, tpts=tpts,
                                      basis.type = current.basis, nbasis = current.nbasis, norder = norder,
                                      split.rate = split.rate, seed = seeds, iter = i, plot=F))
        mise_fold[i] = output$fos.mise
        #print(paste("Fold", i, "is done!"))
      }
      mise_mean_per_nbasis[j] = mean(mise_fold)
      mise_sd_per_nbasis[j] = sd(mise_fold)
      print(paste(current.basis, "basis with", current.nbasis, "basis functions is done."))
    }
    min_mise_per_basis[[k]] = min(mise_mean_per_nbasis)
    all_mise_per_basis[[k]] = mise_mean_per_nbasis
    all_mise_sd_per_basis[[k]] = mise_sd_per_nbasis
  }
  
  names(min_mise_per_basis) = names(all_mise_per_basis) = names(all_mise_sd_per_basis) = tune.list$basis.type.choice
  
  min_mise_overall_value = min_mise_per_basis[[which.min(min_mise_per_basis)]]
  min_mise_overall_sd = all_mise_sd_per_basis[[which.min(min_mise_per_basis)]][which.min(all_mise_per_basis[[which.min(min_mise_per_basis)]])]
  min_mise_overall = c(min_mise_overall_value, min_mise_overall_sd)
  names(min_mise_overall) = c("Mean*", "SD")
  
  all_info = list(all_mise_per_basis, all_mise_sd_per_basis)
  names(all_info) = c("Mean", "Corresponding SD")
  
  basis_min_mise = tune.list$basis.type.choice[which.min(min_mise_per_basis)]
  nbasis_min_mise = tune.list$nbasis.choice[[which.min(min_mise_per_basis)]][which.min(all_mise_per_basis[[which.min(min_mise_per_basis)]])]
  
  return(list(best.set = list(min_mise_overall = min_mise_overall, 
                              basis.type = basis_min_mise, 
                              nbasis = nbasis_min_mise),
              all.info = all_info,
              best.set.per.basis.type = min_mise_per_basis))
}

# 'fam.param.tune' function: Cross validation for fam hyperparameter (spline.basis)
fam.param.tune <- function(tune.list, nfolds, data.x, data.y, tpts = NULL){
  
  for (j in 1:length(tune.list$spline.basis.choice)){
    current.spline.basis = tune.list$spline.basis.choice[j]
    folds = createFolds(data.x[,1], k = nfolds, list = T, returnTrain = F)
    seeds = sample(500:5000, nfolds, replace = F)
    
    mise_mean_per_sb = c()
    mise_sd_per_sb = c()
    mise_fold = vector("logical", length = nfolds)
    # Looping to run model
    for (i in 1:nfolds){
      split.rate = c(1:nrow(data.x))[-folds[[i]]]
      output = suppressWarnings(fam(data.x = data.x, data.y = data.y, tpts=tpts, 
                                    spline.basis = current.spline.basis, split.rate = split.rate, 
                                    seed = seeds, iter = i, plot=F))
      mise_fold[i] = output$fam.mise
      #print(paste("Fold", i, "is done!"))
    }
    mise_mean_per_sb[j] = mean(mise_fold)
    mise_sd_per_sb[j] = sd(mise_fold)
    print(paste("Spline basis =", current.spline.basis, "is done."))
  }
  
  names(mise_mean_per_sb) = names(mise_sd_per_sb) = tune.list$spline.basis.choice
  
  min_mise_overall_mean = mise_mean_per_sb[[which.min(mise_mean_per_sb)]]
  min_mise_overall_sd = mise_sd_per_sb[[which.min(mise_sd_per_sb)]]
  min_mise_overall = c(min_mise_overall_mean, min_mise_overall_sd)
  names(min_mise_overall) = c("Mean*", "SD")
  
  all_info = list(mise_mean_per_sb, mise_sd_per_sb)
  names(all_info) = c("Mean", "Corresponding SD")
  
  spline.basis_min_mise = tune.list$spline.basis.choice[which.min(mise_mean_per_sb)]
  
  return(list(best.set = list(min_mise_overall = min_mise_overall, 
                              spline.basis = spline.basis_min_mise),
              all.info = all_info))
}
