# Simulating a Gaussian Process
library(MASS)

# Setting the parameters
m = 3
theta = 1
sigma = 1

#Number of samples
n_sample = 15
x = seq(from = -3, to = 3, length = n_sample)
M = matrix(0, n_sample, n_sample)

# Filling the covariance matrix 
for (i in 1:n_sample) {
  for (j in 1:n_sample) {
    M[i,j] = sigma^2*exp(-((x[i]-x[j])^2)/2/theta)
  }
}

#Simulating the gaussian vector
y = mvrnorm(n=1, mu = m*rep(1, n_sample), Sigma = M)

#Fitting a simple kriging model
model = km(formula = ~1, design = data.frame(x = x), response = y, covtype = "gauss", coef.trend = m, coef.cov = theta, coef.var = sigma^2)

# Predicting some sample with the kriging model
n_sample2 = 8
x2 = seq(from = -3, to = 3, length = n_sample2)
y2 = predict(model, newdata = data.frame(x = x2), type = "SK")

# Fitting a universal kriging model
model2 = km(formula = ~1, design = data.frame(x = x2), response = y2$mean, covtype = "gauss")

# Let's if the model finds the good parameters in the end !
model2
