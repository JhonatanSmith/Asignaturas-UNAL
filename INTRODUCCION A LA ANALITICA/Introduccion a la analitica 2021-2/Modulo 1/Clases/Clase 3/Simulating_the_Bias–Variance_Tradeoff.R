#Simulating the Bias-Variance Tradeoff. complementary material for Chapter 2 ISLR
#Source: https://daviddalpiaz.github.io/r4sl/simulating-the-biasvariance-tradeoff.html
#We will illustrate Bias-Variance Decomposition, and the resulting 
#bias-variance tradeoff through simulation. Suppose we would like a train a model 
#to learn the function f(x)=x^2 

f = function(x) {
  x ^ 2
}
# More specifically,
# y= x^2 + ??, where ?? ???N(??=0,??^2=0.3^ 2.)
# We write a function which generates data accordingly.

get_sim_data = function(f, sample_size = 100) {
  x = runif(n = sample_size, min = 0, max = 1)
  y = f(x) + rnorm(n = sample_size, mean = 0, sd = 0.3)
  data.frame(x, y)
}
# To get a sense of the data, we generate one simulated dataset, 
# and fit the four models that we will be of interest.

sim_data = get_sim_data(f, sample_size = 100)

fit_1 = lm(y ~ 1, data = sim_data)
fit_2 = lm(y ~ poly(x, degree = 1), data = sim_data)
fit_3 = lm(y ~ poly(x, degree = 2), data = sim_data)
fit_4 = lm(y ~ poly(x, degree = 3), data = sim_data)

# Plotting these four trained models, we see that the zero predictor model (red) 
# does very poorly. The single predictor model (blue) is reasonable, but we can see 
# that the two (green) and three (orange) predictor models seem more appropriate. 
# Between these latter two, it is hard to see which seems more appropriate.

set.seed(430)
plot(y ~ x, data = sim_data,ylim=c(-3,2))
grid = seq(from = 0, to = 1, by = 0.01)
lines(grid, predict(fit_1, newdata = data.frame(x = grid)), 
      col = "red", lwd = 2, lty = 2)
lines(grid, predict(fit_2, newdata = data.frame(x = grid)), 
      col = "blue", lwd = 2, lty = 3)
lines(grid, predict(fit_3, newdata = data.frame(x = grid)), 
      col = "green", lwd = 2, lty = 4)
lines(grid, predict(fit_4, newdata = data.frame(x = grid)), 
      col = "orange", lwd = 2, lty = 5)
lines(grid, f(grid), col = "black", lwd = 3)
legend(x = 0.75, y = 0,
       c("y ~ 1", "y ~ poly(x, 1)", "y ~ poly(x, 2)",  "y ~ poly(x, 3)", "truth"),
       col = c("red", "blue", "green", "orange", "black"), lty = c(2, 3, 4, 5, 1), lwd = 0.2)
# We will now use simulation to estimate the bias, variance, and mean squared error 
# for the estimates for f(x) given by these models at the point x0=0.95. 
#We use simulation to complete this task, as performing the exact calculations
#are always difficult, and often impossible.

set.seed(1)
n_sims = 1000
n_models = 4
x0 = 0.95
predictions = matrix(0, nrow = n_sims, ncol = n_models)
sim_data = get_sim_data(f, sample_size = 100)
plot(y ~ x, data = sim_data, col = "white", xlim = c(0.75, 1), ylim = c(0, 1.5))

for (i in 1:n_sims) {
  
  sim_data = get_sim_data(f, sample_size = 100)
  
  fit_1 = lm(y ~ 1, data = sim_data)
  fit_2 = lm(y ~ poly(x, degree = 1), data = sim_data)
  fit_3 = lm(y ~ poly(x, degree = 2), data = sim_data)
  fit_4 = lm(y ~ poly(x, degree = 3), data = sim_data)
  
  lines(grid, predict(fit_1, newdata = data.frame(x = grid)), col = "red", lwd = 1)
  lines(grid, predict(fit_2, newdata = data.frame(x = grid)), col = "blue", lwd = 1)
  lines(grid, predict(fit_3, newdata = data.frame(x = grid)), col = "green", lwd = 1)
  lines(grid, predict(fit_4, newdata = data.frame(x = grid)), col = "orange", lwd = 1)
  
  predictions[i, ] = c(
    predict(fit_1, newdata = data.frame(x = x0)),
    predict(fit_2, newdata = data.frame(x = x0)),
    predict(fit_3, newdata = data.frame(x = x0)),
    predict(fit_4, newdata = data.frame(x = x0))
  )
}

points(x0, f(x0), col = "black", pch = "x", cex = 2)

# To evaluate the bias and variance, we simulate values for the response  
# y at x0=0.95 according to the true model.

eps = rnorm(n = n_sims, mean = 0, sd = 0.3)
y0 = f(x0) + eps
length(f(x0))
# R already has a function to calculate variance, however, 
# we add functions for bias and mean squared error.

get_bias = function(estimate, truth) {
  mean(estimate) - truth
}

get_mse = function(estimate, truth) {
  mean((estimate - truth) ^ 2)
}

#When then use the predictions obtained from the above simulation to estimate the bias,
# variance and mean squared error for estimating f(x) at x0=0.95 for the four models.

bias = apply(predictions, 2, get_bias, f(x0))
sqrBias=bias^2
variance = apply(predictions, 2, var)
mse = apply(predictions, 2, get_mse, y0)
list(bias, sqrBias, variance,mse)

degrees<-c(1,2,3,4)
plot(degrees,sqrBias,col='blue',type='l',lwd = 2, lty = 2,ylim=c(0,0.45),
     xlab='Flexibility',ylab='Bias-Variance')
lines(degrees,variance,col='orange',lwd = 2, lty = 3)
lines(degrees,mse,col='green',lwd = 2, lty = 4)

legend(x = 2.5, y = 0.4, c("Square Bias", "Variance", "MSE"),
       col = c("blue", "orange", "red"), lty = c(2, 3, 4), lwd = 0.2)

#CHECKING THE BIAS-VARIANCE EQUATION

sqrBias + variance + var(eps)
mse
