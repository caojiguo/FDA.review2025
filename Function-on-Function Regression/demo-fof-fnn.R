# set working directory to currect file location

library(fda)
library(torch)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

source("FoFNN.R")

bikedata = read.csv("../Datasets/bike_sharing/hour.csv")
unique_days = bikedata$dteday |> unique()
unique_days = unique_days[1:365]
gridobj = expand.grid(hr = 1:24, dteday = unique_days[1:365])
bikedata = data.frame(dteday = gridobj$dteday, hr = gridobj$hr) |>
  left_join(bikedata, by = c("dteday", "hr"))

hr = 1:23
cnt = sapply(unique_days, \(x) bikedata$cnt[bikedata$dteday == x])
temp = sapply(unique_days, \(x) bikedata$atemp[bikedata$dteday == x])
colnames(cnt) = colnames(temp) = unique_days

# all missing values at hr=24, remove them
cnt = cnt[1:23,]
temp = temp[1:23,]

# futher remove missing values
# cnt: NA -> no bike rental, 0
cnt[is.na(cnt)] = 0
# remove the days with few temperature records
missing_id = colMeans(is.na(temp)) > 0.1
cnt = cnt[,!missing_id]
temp = temp[,!missing_id]
unique_days = unique_days[!missing_id]
N = ncol(cnt)
M = length(unique_days)

# smoothing an re-evaluating
xbasis = create.bspline.basis(range(hr), nbasis = 7)
ybasis = create.bspline.basis(range(hr), nbasis = 11)
Y = matrix(NA, nrow = N, ncol = length(hr))
X = matrix(NA, nrow = N, ncol = length(hr))
for (i in seq_len(N)) {
  na_id = is.na(cnt[,i])
  Y[i,] = eval.fd(hr, Data2fd(hr[!na_id], cnt[!na_id,i], ybasis))
  na_id = is.na(temp[,i])
  # the temperature is divided by 50
  X[i,] = eval.fd(hr, Data2fd(hr[!na_id], temp[!na_id,i], xbasis)) * 50
}
rownames(Y) = unique_days
rownames(X) = unique_days

tidy_data = left_join(
  as.tibble(t(Y)) |>
    mutate(Hour = hr) |>
    pivot_longer(cols = unique_days,
      names_to = "Date", values_to = "Count"),
  as.tibble(t(X)) |>
    mutate(Hour = hr) |>
    pivot_longer(cols = unique_days,
      names_to = "Date", values_to = "Temperature"),
  by = c("Date", "Hour")
)

tidy_data |>
  mutate(Date = as.Date.character(Date),
    Days = as.integer(Date - min(Date))) |>
  pivot_longer(cols = c("Count", "Temperature"),
    names_to = "Variable", values_to = "Value") |>
  ggplot(aes(x = Hour, y = Value, group = Date, color = Days)) +
  geom_line(alpha = 0.2) +
  facet_wrap(~Variable, scales = "free") +
  scale_color_gradientn(colors = rainbow(10)) +
  theme_bw()

# ggsave("figures/bike.jpg", width=6, height=3.6)

fofnn = fit.fofnn(
  Y, X, xtgrid = hr, ytgrid = hr,
  xbasisobj = xbasis, ybasisobj = ybasis,
  hidden_sizes = c(), act_func = "relu", max_epoch = 1000)
Ypred = predict(fofnn, X, xtgrid = hr)

jpeg("figures/fofnn-fit.jpg", width = 640, height = 360)
par(mfrow=c(1,2))
matplot(t(Y), type="l", xlab="t", ylab="y", main="True")
matplot(t(Ypred[[1]]), type="l", xlab="t", ylab="y", main="Predicted")
par(mfrow=c(1,1))
dev.off()

# fofnn = fit.fofnn(
#   Y, X, xtgrid = hr, ytgrid = hr,
#   xbasisobj = xbasis, ybasisobj = ybasis,
#   hidden_sizes = c(), act_func = "relu", max_epoch = 1000)
# 
# Ypred = predict(fofnn, X, xtgrid = hr)
# 
# jpeg("figures/foflm-fit.jpg", width = 640, height = 360)
# par(mfrow=c(1,2))
# matplot(t(Y), type="l", xlab="t", ylab="y", main="True")
# matplot(t(Ypred[[1]]), type="l", xlab="t", ylab="y", main="Predicted")
# par(mfrow=c(1,1))
# dev.off()

sample_id = sample(1:N, 4, replace = FALSE)
matplot(t(Y[sample_id,,drop=F]), type="l", lty=1,
  xlab="t", ylab="y", main="True vs. Predicted")
matplot(t(Ypred[[1]][sample_id,,drop=F]), type="l", lty=3, add =TRUE)



