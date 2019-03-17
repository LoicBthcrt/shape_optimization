library(geometry)

# requires that the functions.R file is in the working directory 
source("./functions.R")

result = get_points(fourrier_coeff = c(1, 0, 0), theta = seq(from = 0, to = 2*pi, length = 200))
X = unlist(result['X'])
Y = unlist(result['Y'])

plot(X, Y)

test = cost_func(c(1, 0, 0), 10, seq(from = 0, to = 2*pi, length = 200), 2*pi)
