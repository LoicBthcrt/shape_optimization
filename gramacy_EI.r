library(DiceKriging)
library(DiceOptim)

# Définition de la fonction
fonction_test = function(x){
  y = 0*x
  for (i in 1:length(x)){
    y[i] =  sin(10*pi*x[i])/(2*x[i]) + (x[i]-1)^4;
  }
  y
}


# Inputs initiaux
x = seq(0.5, 2.5, by=0.01)
y = fonction_test(x);


inputs = seq(0.5, 2.5, by = 0.5)
outputs = fonction_test(inputs)

# Tests algorithme EGO "à la main"

# # Etape 2
# inputs = c(0.5,0.7435621,1,1.5,2,2.5)
# outputs = fonction_test(inputs)
# 
# # Etape 3
# inputs = c(0.5,0.7435621,1,1.5, 1.723436,2,2.5)
# outputs = fonction_test(inputs)
# 
# # Etape 4
# inputs = c(0.5,0.6995174,0.7435621,1,1.5, 1.723436,2,2.5)
# outputs = fonction_test(inputs)
# 
# # Etape 5
# inputs = c(0.5,0.6995174,0.7435621,0.8316738,1,1.5, 1.723436,2,2.5)
# outputs = fonction_test(inputs)

# et ainsi de suite..



dev.off()
plot(x, y, type="l")
points(inputs,outputs,col='red',pch=19,cex=0.6)

# Noyau de covariance Matern 5/2
model_matern <- km(~1, design = data.frame(x = inputs), response = outputs, covtype = "matern5_2")
ypredict_matern = predict(model_matern, newdata = data.frame(x = x),type="UK")

lines(x,ypredict_matern$mean,type="l",col='blue')
points(inputs,outputs,col='red',pch=19,cex=0.6)

# Calcul de l'Expected Improvement
t <- seq(from = 0.5, to = 2.5, by = 0.001)
EI_values <- apply(as.matrix(t), 1, EI, model_matern, type = "UK")
plot(t,EI_values,type='l')

# Point correspondant au maximum 
x_star <- max_EI(model_matern, lower = 0.5, upper = 2.5,parinit = 0.5,control = list(pop.size = 20, BFGSburnin = 2))

# Application de l'algorithme EGO
optim = EGO.nsteps(model_matern,fonction_test,100,lower=0.5,upper=2.5,control = list(pop.size = 20, BFGSburnin = 2))
xmin = optim$par[which.min(optim$value)]
