rm(list=ls())
library('ftsa')


serieslength = c(100,500,1000,2000)

for (ii in 1:4){
  for (jj in 1:100){
    
    mydf = as.matrix(read.table(paste('./trend_nonlineardata/simdata_trend_nonlinear_',serieslength[ii],'_sim',jj,'.txt',sep='')))
    
    
    trainingdf = data.frame(t(mydf[1:(serieslength[ii] - 20),]))
    
    rownames(trainingdf) = 1:51
    colnames(trainingdf) = 1:(serieslength[ii] - 20)
    
    # 
    # fts1 <- rainbow::fts(x =1:51, y = trainingdf)
    # 
    # 
    # fitmod_1 = ftsm(y = fts1)
    # pred_1= forecast(fitmod_1,h=20)$mean$y
    # pred_1 = apply(pred_1, 2, function(x){x / stats::sd(x)}  )
    # 
    # write.table(pred_1,file=paste('./trend_nonlinearsimresults/funseries_simresults_trend_nonlinear_',serieslength[ii],'_sim',jj,'.txt',sep=''),row.names = FALSE,col.names = FALSE)
    # 
    # 
    # plsrmod = fplsr(fts1)
    # pred_plsr = forecastfplsr(fts1, components=6,  h =20)$y
    # pred_plsr = apply(pred_plsr,2,function(x){x / stats::sd(x)})
    # write.table(pred_plsr,file=paste('./trend_nonlinearsimresults/fplsr_simresults_trend_nonlinear_',serieslength[ii],'_sim',jj,'.txt',sep=''),row.names = FALSE,col.names = FALSE)
    # 
    
    basispline = create.bspline.basis(c(1,51),nbasis = 13)
    datafd = as.fd(smooth.basis(1:51, as.matrix(trainingdf), basispline))
    dpca.res = fts.dpca(datafd, Ndpc = 6, freq=(-25:25/25)*pi)
    
    scores    = dpca.res$scores
    score_pred = matrix(NA, nrow = 6, ncol = 20)
    for (pc in 1:6){
      mod = Arima(scores[,pc], c(3,1,1))  
      score_pred[pc,] = predict(mod, 20)$pred
    }
    
    
    dpc_operators = dpca.res$filters$operators
    basismat = eval.basis(1:51,basispline)
    #basismat %*% dpc_operators[pc,,]
    
    pred_fun = matrix(NA, nrow = 20, ncol = 51)
    total_score = rbind(scores,t(score_pred))
    lag_dis = 20
    currenttime = serieslength[ii] - 20
    for (h in 1:20){
      basis_fun = matrix(NA, nrow = 6, ncol = 51)  
      for (pc in 1:6){
        dfpc = basismat %*% dpc_operators[pc,,]
        basis_fun[pc,] = total_score[(currenttime-lag_dis+1):(currenttime + 1),pc] %*%  t(dfpc[,(30-20):30])
      }
      pred_fun[h,] = apply(basis_fun,2,sum)
      currenttime = currenttime + 1
    }
    
    #dfpca_runningtime[jj,ii] = end_time - start_time
    write.table(pred_fun,file=paste('./trend_nonlinearsimresults/dfpca_simresults_trend_nonlinear_',serieslength[ii],'_sim',jj,'.txt',sep=''),row.names = FALSE,col.names = FALSE)
  }
}

