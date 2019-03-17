library(DiceKriging)
library(DiceOptim)

x = seq(0, 5, by=0.1)
y = 0 * x;

for (i in 1:length(x)){
  y[i] =  sin(1/2*pi*x[i]) + 2 * x[i]^(1/2) + (1/20) * cos(5*pi*x[i]);
}

dev.off()
plot(x, y, type="l")

# Importance du plan d'expérience pour la qualité du modèle
inputs = c(x[5],x[11],x[21],x[31],x[41],x[48])
outputs = c(y[5],y[11],y[21],y[31],y[41],y[48])

# Fonction de covariance gaussienne
model_gauss <- km(~1, design = data.frame(x = inputs), response = outputs, covtype = "gauss")
ypredict_gauss = predict(model_gauss, newdata = data.frame(x = x),type="UK")

# Avec covtype = "matern5_2" : intervalles de confiance plus larges !

# Tracé du modèle est des intervalles de confiance
lines(x,ypredict_gauss$mean,type="l",col="blue")
lines(x,ypredict_gauss$lower95,type="l",col="black",lty=2)
lines(x,ypredict_gauss$upper95,type="l",col="black",lty=2)
points(inputs, outputs, col="black", pch=19)


# Tests avec d'autres fonctions de covariance
model_exp <- km(~1, design = data.frame(x = inputs), response = outputs, covtype = "exp")
ypredict_exp = predict(model_exp, newdata = data.frame(x = x),type="UK")

model_matern <- km(~1, design = data.frame(x = inputs), response = outputs, covtype = "matern5_2")
ypredict_matern = predict(model_matern, newdata = data.frame(x = x),type="UK")

# Comparaison
dev.off()
plot(x, y, type="l")
lines(x,ypredict_exp$mean,type="l",col='blue')
lines(x,ypredict_matern$mean,type="l",col='green')
lines(x,ypredict_gauss$mean,type="l",col="orange")
points(inputs,outputs,col='black',pch=19)
legend(0.2, 5, legend=c("Exponentiel", "Matern 5/2","Gauss"),
       col=c("blue", "green","orange"),lty=1, cex=0.8)


