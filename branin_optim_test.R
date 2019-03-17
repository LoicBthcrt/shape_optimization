library(lhs)
library(DiceKriging)
library(DiceOptim)

# experimental design : latin-square
d = 2
n = 15
set.seed(0)
design = optimumLHS(n, d)
design = data.frame(design)
names(design) = c("x1", "x2")
plot(design$x1, design$x2)

#fitting initial model
response.branin = apply(design, 1, branin)
fitted.model1 = km(~1, design = design, response = response.branin, covtype = "gauss")
fitted.model2 = km(~1, design = design, response = response.branin, covtype = "matern5_2")

x.grid <- y.grid <- seq(0, 1, length = n.grid <- 25)
design.grid = expand.grid(x.grid, y.grid)

# EI function from first 10 points
EI.grid = apply(design.grid, 1, EI, fitted.model2)
z.grid = matrix(EI.grid, n.grid, n.grid)
contour(x.grid, y.grid, z.grid, 20)
points(design[ , 1], design[ , 2], pch = 17, col = "blue")

# EGO
nsteps <- 10
lower = rep(0, d)
upper = rep(1, d)
oEGO <- EGO.nsteps(model = fitted.model2, fun = branin, nsteps = nsteps, lower, upper, control = list(pop.size = 20, BFGSburnin = 2))

# contour of branin
response.grid = apply(design.grid, 1, branin)
z.grid = matrix(response.grid, n.grid, n.grid)
contour(x.grid, y.grid, z.grid, 40)
points(design[ , 1], design[ , 2], pch = 17, col = "blue")
points(oEGO$par, pch = 19, col = "red")
text(oEGO$par[ , 1], oEGO$par[ , 2], labels = 1:nsteps, pos = 3)

# EI grid in the end
EI.grid = apply(design.grid, 1, EI, oEGO$lastmodel)
z.grid = matrix(EI.grid, n.grid, n.grid)
contour(x.grid, y.grid, z.grid, 20)
points(design[ , 1], design[ , 2], pch = 17, col = "blue")
points(oEGO$par, pch = 19, col = "red")
