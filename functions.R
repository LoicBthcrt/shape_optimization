get_points <- function(fourrier_coeff, theta) {
  n = length(theta)
  l = (length(fourrier_coeff)-1)/2
  
  r = fourrier_coeff[1]*rep(1, n)
  
  for (i in 2:(l+1)) {
    r = r + fourrier_coeff[i]*cos(i*theta) + fourrier_coeff[i+l]*sin(i*theta)
  }
  
  X = r*cos(theta)
  Y = r*sin(theta)
  result = list(X = X, Y = Y)
  return(result)
}

result = get_points(fourrier_coeff = c(1, 0.2, 0.5), theta = seq(from = 0, to = 2*pi, length = 200))
X = unlist(result['X'])
Y = unlist(result['Y'])

plot(X, Y)
