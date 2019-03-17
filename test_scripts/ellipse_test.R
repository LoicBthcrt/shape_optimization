library(geometry)

# length of theta
n = 200

# setting parameter and computig radius of the ellipse
theta = seq(from = 0, to = 2*pi, length = n)
a = 1.5
b = 1
r = a*b  / sqrt((b^2)*cos(theta)*cos(theta) + (a^2)*sin(theta)*sin(theta))

# geting cartesians coordinates from polar ones
X = r*cos(theta)
Y = r*sin(theta)
plot(X, Y)

# computing area with polyarea and with the real expression
area = polyarea(X, Y)
A = pi*a*b

# computing perimeter
perimeter = 0
for (i in 1:(n-1)) {
  perimeter = perimeter + sqrt((X[i+1] - X[i])^2 + (Y[i+1] - Y[i])^2)
}

# ramanujan perimeter approximation for an ellipse
p = pi*(3*(a+b) - sqrt((3*a + b)*(a + 3*b)))
