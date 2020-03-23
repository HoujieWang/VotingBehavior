rm(list = ls())
library(tidyverse); library(factoextra); library(cluster); library(MASS); 
library(olsrr);library(Hmisc);library(car); library(gvlma); library(leaps); 
library(glmnet); library(e1071); library(EnvStats)

############################ Kmeans ############################
load("data_final.RData")
y =  data[, 1]; X = as.matrix(data[, -1])
colnames(X) = c("A", "B", "C", "D", "E", "F")
fviz_nbclust(X, kmeans, method = "wss")
fviz_nbclust(X, kmeans, method = "silhouette")

# So we let k = 2 

clu.obj = kmeans(X, centers = 2)
y1 = y[clu.obj$cluster == 1]
X1 = X[clu.obj$cluster == 1, ]
y2 = y[clu.obj$cluster == 2]
X2 = X[clu.obj$cluster == 2, ]

fit1 = lm(y1~X1)
fit2 = lm(y2~X2)
par(mfrow = c(2,2))
plot(fit1)
par(mfrow = c(2,2))
plot(fit2)
summary(fit2)

pwer = -1.5
X1_trsfom = bcnPower(X1, lambda = rep(pwer, 6), gamma = c(1, 1, 5, 6, 6, 4))
y1_trsfom = bcnPower(y1, lambda = pwer, gamma = 0.5)
fit1 = lm(y1_trsfom~X1_trsfom)
summary(fit1)
cv_lasso = cv.glmnet(X1_trsfom, y1_trsfom)
lasso_coef = coef(cv_lasso, s = "lambda.min")
X1_trsfom = X1_trsfom[, as.vector(lasso_coef)[-1] != 0]
fit1 = lm(y1_trsfom~X1_trsfom)
summary(fit1)
par(mfrow = c(2,2))
plot(fit1)
gvlma(fit1)

