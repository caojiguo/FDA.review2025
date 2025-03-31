rm(list=ls())
library('ftsa')
library(fda)
library(freqdom.fda)

multidf = matrix(as.numeric(unlist(read.table('AQdata.txt',sep=' '))),nrow=2192,ncol = 72)


trainingdf = data.frame(t(multidf[1:2142,]))
rownames(trainingdf) = 1:72

colnames(trainingdf) = 1:2142
fts1 <- rainbow::fts(x =1:72, y = trainingdf)
pred_1 = matrix(0, nrow = 50, ncol = 72)
fitmod_1 = ftsm(y = fts1, 1)


pred_1 = t(forecast(fitmod_1,h=50)$mean$y)
write.table(pred_1,file='ftsapred_multi.csv',sep=',')


plsrmod = fplsr(fts1)

pred_plsr = t(forecastfplsr(object = fts1, components = 6, h =50)$y)
write.table(pred_plsr,file='fplsr_multi.csv',sep=',')



basispline = create.bspline.basis(c(1,72),nbasis = 21)
datafd = as.fd(smooth.basis(1:72, as.matrix(trainingdf), basispline))
dpca.res = fts.dpca(datafd, Ndpc = 6, freq=(-25:25/25)*pi)

scores    = dpca.res$scores
score_pred = matrix(NA, nrow = 6, ncol = 50)
for (pc in 1:6){
  mod = Arima(scores[,pc], c(3,1,1))  
  score_pred[pc,] = predict(mod, 50)$pred
}


dpc_operators = dpca.res$filters$operators
basismat = eval.basis(1:72,basispline)
#basismat %*% dpc_operators[pc,,]

pred_fun = matrix(NA, nrow = 50, ncol = 72)
total_score = rbind(scores,t(score_pred))
lag_dis = 10
currenttime = 2192 - 50
for (h in 1:50){
  basis_fun = matrix(NA, nrow = 6, ncol = 72)  
  for (pc in 1:6){
    dfpc = basismat %*% dpc_operators[pc,,]
    basis_fun[pc,] = total_score[(currenttime-lag_dis+1):(currenttime + 1),pc] %*%  t(dfpc[,(30-10):30])
  }
  pred_fun[h,] = apply(basis_fun,2,sum)
  currenttime = currenttime + 1
}

write.table(pred_fun,file='dfpca_multi.csv',sep=',')
