rm(list=ls())
source(file = "mainRegression.R")
set.seed(123)

ech <- 500
n <- 10
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]

c(alpha, beta) 
reg <- basic_forwardregression(x,y)
reg

basic_forwardregression(x,y)$erreur
basic_forwardregression(x[,-6],y)$erreur
basic_forwardregression(x[,-1],y)$erreur
