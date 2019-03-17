get_points <- function(fourrier_coeff, theta) {
  n = length(theta)
  l = (length(fourrier_coeff)-1)/2
  
  r = fourrier_coeff[1]*rep(1, n)
  
  for (i in 2:(l+1)) {
    r = r + fourrier_coeff[i]*cos((i-1)*theta) + fourrier_coeff[i+l]*sin((i-1)*theta)
  }
  
  X = r*cos(theta)
  Y = r*sin(theta)
  result = list(X = X, Y = Y)
  return(result)
}

cost_func <- function(fourrier_coeff, lambda, theta, peri) {
  n = length(theta)
  points = get_points(fourrier_coeff, theta)
  X = unlist(points['X'])
  Y = unlist(points['Y'])
  
  result = polyarea(X, Y)
  perimeter = 0
  
  for (i in 1:(n-1)) {
    perimeter = perimeter + sqrt((X[i+1] - X[i])^2 + (Y[i+1] - Y[i])^2)
  }
  
  result = result - lambda * (perimeter - peri)^2
  return(result)
}