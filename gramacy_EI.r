library(DiceKriging)
library(DiceOptim)

fonction_test = function(x){
  y = 0*x
  for (i in 1:length(x)){
    y[i] =  sin(10*pi*x[i])/(2*x[i]) + (x[i]-1)^4;
  }
  y
}

x = seq(0.5, 2.5, by=0.01)
y = fonction_test(x);

inputs = seq(0.5, 2.5, by = 0.5)
outputs = fonction_test(inputs)
inputs = c(0.5,0.7435621,1,1.5,1.75,2,2.5)
outputs = fonction_test(inputs)

dev.off()
plot(x, y, type="l")
points(inputs,outputs,col='red',pch=19,cex=0.6)

model_exp <- km(~1, design = data.frame(x = inputs), response = outputs, covtype = "exp")
ypredict_exp = predict(model_exp, newdata = data.frame(x = x),type="UK")

model_matern <- km(~1, design = data.frame(x = inputs), response = outputs, covtype = "matern5_2")
ypredict_matern = predict(model_matern, newdata = data.frame(x = x),type="UK")

# Gauss ne fonctionne pas ! Pourquoi ? -> Normal, utiliser plutôt Matern 5/2 (régularité)
# model_gauss <- km(~1, design = data.frame(x = inputs), response = outputs, covtype = "gauss")
# ypredict_gauss = predict(model_gauss, newdata = data.frame(x = x),type="UK")

lines(x,ypredict_exp$mean,type="l",col='blue')
lines(x,ypredict_matern$mean,type="l",col='green')
#lines(x,ypredict_gauss$mean,type="l",col="orange")
points(inputs,outputs,col='red',pch=19,cex=0.6)

t <- seq(from = 0.5, to = 2.5, by = 0.001)
EI_values <- apply(as.matrix(t), 1, EI, model_matern, type = "UK")
plot(t,EI_values,type='l')

x_star <- max_EI(model_matern, lower = 0.5, upper = 2.5,parinit = 0.5,control = list(pop.size = 10, max.generations = 1000,wait.generations = 1000, BFGSburnin = 10))

