library(lhs)
library(DiceKriging)
library(DiceOptim)
library(geometry)
library(plotly)
library(MASS)

# Choix du plan (coefficient de Fourier que l'on met constant)
#coeff = 'a0'
#coeff = 'a2'
coeff = 'b2'

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
d = 2
n = 50
set.seed(0)
design = optimumLHS(n, d)
design = data.frame(design)
design = (design - 0.5) * 2 * a


x.grid <- y.grid <- seq(-a, a, length = n.grid <- 25)
design.grid = expand.grid(x.grid, y.grid)


if (coeff == 'a0'){
  # On fixe a0 = 1
z1.grid <- rep(1, n.grid)
design.grid$x = z1.grid
design.grid$y = design.grid$Var1
design.grid$z = design.grid$Var2
} else if (coeff == 'a2'){
#On fixe a2 = 0
z1.grid <- rep(0, n.grid)
design.grid$x = design.grid$Var1
design.grid$y = z1.grid
design.grid$z = design.grid$Var2
} else{
# On fixe b2 = 0
z1.grid <- rep(0, n.grid)
design.grid$x = design.grid$Var1
design.grid$y = design.grid$Var2
design.grid$z = z1.grid
}

design.grid$Var1 <- NULL
design.grid$Var2 <- NULL

cost.grid = apply(design.grid, 1, cost)
z.grid = matrix(cost.grid, n.grid, n.grid)

# Lignes de niveau
contour(x.grid, y.grid, z.grid, 20)

# Point correspondant au minimum
xmin = design.grid[which.min(cost.grid),]

if (coeff == 'a0'){
points(xmin[ , 2], xmin[ , 3], pch = 19, col = "red")
} else if (coeff == 'a2'){
points(xmin[ , 1], xmin[ , 3], pch = 19, col = "red")} else{
points(xmin[ , 1], xmin[ , 2], pch = 19, col = "red") 
}

# Graphe en 3D
p <- plot_ly(x = x.grid, y = y.grid, z = z.grid) %>% add_surface()
