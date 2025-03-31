rm(list=ls())
library('ftsa')
library(fda)
library(freqdom.fda)

mydf = read.table('singlefortimeseries.txt',sep=',')

trainingdf = data.frame(t(mydf[1:1586,]))
rownames(trainingdf) = 1:24

colnames(trainingdf) = 1:1586
fts1 <- rainbow::fts(x =1:24, y = trainingdf)

pred_1 = matrix(0, nrow = 500, ncol = 24)
pred_8 = matrix(0, nrow = 500, ncol = 24)

fitmod_1 = ftsm(y = fts1)
pred_1= t(forecast(fitmod_1,h=500)$mean$y)
write.table(pred_1,file='ftsapred_8.csv',sep=',')

plsrmod = fplsr(fts1)

pred_plsr = forecastfplsr(object = fts1, components = 1, h =500)$y
write.table(pred_plsr,file='plsr.csv',sep=',')



basispline = create.bspline.basis(c(1,24),nbasis = 13)
datafd = as.fd(smooth.basis(1:24, as.matrix(trainingdf), basispline))
dpca.res = fts.dpca(datafd, Ndpc = 6, freq=(-25:25/25)*pi)

scores    = dpca.res$scores
score_pred = matrix(NA, nrow = 6, ncol = 500)
for (pc in 1:6){
  mod = Arima(scores[,pc], c(3,1,1))  
  score_pred[pc,] = predict(mod, 500)$pred
}


dpc_operators = dpca.res$filters$operators
basismat = eval.basis(1:24,basispline)
#basismat %*% dpc_operators[pc,,]

pred_fun = matrix(NA, nrow = 500, ncol = 24)
total_score = rbind(scores,t(score_pred))
lag_dis = 20
currenttime = 1586
for (h in 1:500){
  basis_fun = matrix(NA, nrow = 6, ncol = 24)  
  for (pc in 1:6){
    dfpc = basismat %*% dpc_operators[pc,,]
    basis_fun[pc,] = total_score[(currenttime-lag_dis+1):(currenttime + 1),pc] %*%  t(dfpc[,(30-20):30])
  }
  pred_fun[h,] = apply(basis_fun,2,sum)
  currenttime = currenttime + 1
}

write.table(pred_fun,file='dfpca.csv',sep=',')


