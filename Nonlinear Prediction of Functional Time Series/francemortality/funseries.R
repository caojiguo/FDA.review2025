rm(list=ls())
library('ftsa')
library(freqdom.fda)
library(fda)


mydf = load('france.rda')
mydf = log(t(fr.mort[["rate"]][["total"]]))

mydf = t(mydf[1:191,1:100])

trainingdf = data.frame(mydf[1:100,1:171])


rownames(trainingdf) = 1:100
colnames(trainingdf) = 1:171


fts1 <- rainbow::fts(x =1:100, y = trainingdf)

pred_1 = matrix(0, nrow = 20, ncol = 100)
pred_8 = matrix(0, nrow = 20, ncol = 100)

fitmod_1 = ftsm(y = fts1)
pred_1= t(forecast(fitmod_1,h=20)$mean$y)

temp = fitmod_1$coeff %*% t(fitmod_1$basis)
plot(temp[1,],type='l')
lines(mydf[1,])

plot(temp[3,],type='l')
lines(mydf[3,],col='red')



write.table(pred_1,file='linear_ftsapred.csv',sep=',')

plsrmod = fplsr(fts1)

pred_plsr = t(forecastfplsr(object = fts1,components=6,  h =20)$y)
write.table(pred_plsr,file='linear_plsr.csv',sep=',')




basispline = create.bspline.basis(c(1,100),nbasis = 8)
datafd = as.fd(smooth.basis(1:100, as.matrix(trainingdf), basispline))

dpca.res = fts.dpca(datafd, Ndpc = 3,q =50, freq=(-25:25/25)*pi)


scores    = dpca.res$scores
score_pred = matrix(NA, nrow = 3, ncol = 20)
for (pc in 1:3){
  mod = Arima(scores[,pc], c(1,1,0))  
  score_pred[pc,] = predict(mod, 20)$pred
}


dpc_operators = dpca.res$filters$operators
basismat = eval.basis(1:100,basispline)
#basismat %*% dpc_operators[pc,,]

pred_fun = matrix(NA, nrow = 20, ncol = 100)
total_score = rbind(scores,t(score_pred))
lag_dis = 10
currenttime = 171
for (h in 1:20){
  basis_fun = matrix(NA, nrow = 3, ncol = 100)  
  for (pc in 1:3){
    dfpc = basismat %*% dpc_operators[pc,,]
    basis_fun[pc,] = total_score[(currenttime-lag_dis+1):(currenttime + 1),pc] %*%  t(dfpc[,(30-lag_dis):30])
  }
  pred_fun[h,] = apply(basis_fun,2,sum)
  currenttime = currenttime + 1
}


plot(pred_fun[1,])

write.table(pred_fun,file='linear_dfpca.csv',sep=',')


