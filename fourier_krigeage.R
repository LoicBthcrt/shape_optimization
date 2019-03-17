library(lhs)
library(DiceKriging)
library(DiceOptim)
library(geometry)

# requires that the functions.R file is in the working directory 
source("./functions.R")

#setting parameters for cost functions
lambda = 10
theta = seq(from = 0, to = 2*pi, length = 200)
peri = 2*pi

cost <- function(fourrier_coeff) {
  return(-cost_func(fourrier_coeff, lambda, theta, peri))
}

# a defines the research domain : [-a, a]^d
a = 1.5

# experimental design : latin-square
d = 3
n = 50
set.seed(0)
design = optimumLHS(n, d)
design = data.frame(design)
design = (design - 0.5) * 2 * a

# fitting the initial model
response.cost = apply(design, 1, cost)
fitted.model = km(~1, design = design, response = response.cost, covtype = "matern5_2")

# setting parameter for EGO
nsteps <- 20
lower = rep(-a, d)
upper = rep(a, d)
oEGO <- EGO.nsteps(model = fitted.model, fun = cost, nsteps = nsteps, lower, upper, control = list(pop.size = 20, BFGSburnin = 2))

# getting minimum found
index = which.min(oEGO$value)
coeff = oEGO$par[index,]

points = get_points(coeff, theta)
X = unlist(points['X'])
Y = unlist(points['Y'])

plot(X, Y)

# Here, we try to see vizualize the EI function on (1, y, z)
x.grid <- y.grid <- seq(-a, a, length = n.grid <- 25)
z1.grid <- rep(1, n.grid)
design.grid = expand.grid(x.grid, y.grid)
design.grid$x = z1.grid
design.grid$y = design.grid$Var1
design.grid$z = design.grid$Var2
design.grid$Var1 <- NULL
design.grid$Var2 <- NULL

EI.grid = apply(design.grid, 1, EI, oEGO$lastmodel)
z.grid = matrix(EI.grid, n.grid, n.grid)
contour(x.grid, y.grid, z.grid, 20)
