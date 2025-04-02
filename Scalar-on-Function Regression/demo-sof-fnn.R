
library(fda)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)

data(Fatspectrum, package = "fds")
data(Fatvalues, package = "fds")

tvals = Fatspectrum$x
names(tvals) = paste0("t", seq_along(tvals))
X = t(Fatspectrum$y)
y = Fatvalues

colnames(X) = paste0("t", seq_along(tvals))
tidy_data = cbind(subj=1:nrow(X), X, Fat=y) |> 
  as_tibble() |> 
  pivot_longer(
    cols=paste0("t", seq_along(tvals)),
    names_to = "tid",
    values_to = "Spectrum"
  ) |> 
  mutate(Wavelength = tvals[tid])

ggplot(tidy_data, aes(x = Wavelength, y = Spectrum)) +
  geom_line(aes(color=Fat, group=subj)) + 
  theme_bw()

sofnn = fit.sofnn(
  y, X, tgrid = tvals, lambda = 1e-7, nbasis = 11,
  hidden_sizes = c(16), act_func = "relu", max_epoch = 1000)

# jpeg("figures/sofnn-fit.jpg", width = 640, height = 360)
par(mfrow=c(1,2))
predict(sofnn, X=X, tgrid=tvals) |> 
  plot(y, main="True vs. Predicted", xlab=expression(hat(y)), ylab="y")
abline(coef=c(0,1), col=2)
plot(sofnn$func_weights[[1]], xlab="t", ylab="", main="Functional Weights")
par(mfrow=c(1,1))
# dev.off()

ggsave("figures/tecator.jpg", width = 5, height = 3.5)

sofnn = fit.sofnn(
  y, X, tgrid = tvals, lambda = 1e3, nbasis = 11,
  hidden_sizes = c(16), act_func = "relu", max_epoch = 1000)

par(mfrow=c(1,2))
predict(sofnn, X=X, tgrid=tvals) |> 
  plot(y, main="True vs. Predicted", xlab=expression(hat(y)), ylab="y")
abline(coef=c(0,1), col=2)
plot(sofnn$func_weights[[1]], xlab="t", ylab="", main="Functional Weights")
par(mfrow=c(1,1))

sofnn = fit.sofnn(
  y, X, tgrid = tvals, lambda = 1e-3, nbasis = 11,
  hidden_sizes = c(), act_func = "relu", max_epoch = 2000)

par(mfrow=c(1,2))
predict(sofnn, X=X, tgrid=tvals) |> 
  plot(y, main="True vs. Predicted", xlab=expression(hat(y)), ylab="y")
abline(coef=c(0,1), col=2)
plot(sofnn$func_weights[[1]], xlab="t", ylab="", main="Functional Weights")
par(mfrow=c(1,1))
