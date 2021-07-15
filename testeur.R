rm(list=ls())
source(file = "toolsFunctions.R")
source(file = "toolsFunctions_opti.R")
source(file = "toolsFunctions_para.R")
source(file = "toolsFunctions_c.R")
sourceCpp("toolsFunctions_c.cpp")
source(file = "mainRegression.R")
source(file = "simulations.R")


test_Q1 <- rmodellinear(1,0,1)
test_Q1.b <- rmodellinear(3,2,1)
test_Q1.c <- rmodellinear(3,2,c(1,4))
#Mettre d'autres test, garder le premier

test_Q2 <- basic_computecv(test_Q1[1], test_Q1[2])
test_Q2.b <- basic_computecv(test_Q1.c[,1], test_Q1.c[,2])


test_Q4 <- next.model(1:5,n_num = 10)
test_Q4.b <- next.model(n_num = 10)


test_Q5 <- basic_forwardregression(x,y)
test_lm <- lm(y~x)


test_Q7 <- basic_computecv.para(x,y)
truc <- do.call(c, mclapply(1:10, function(i) 
                          {a <- tab[i]**2
                           a - 2}, 
                    mc.cores = 15))


a <- basic_forwardregression(x, y)
b <- fastR_forwardregression(x, y)
c <- fastRpara_forwardregression(x, y)
d <- fastRc_forwardregression(x,y)
e <- .lm.fit(cbind(1,x),y)$coefficients
f <- lm(y~x[,c(1,3,5,6,10)])
summary(f)

fastLm_noindex(cbind(1,x),y)
fastLm(cbind(1,x), y, 3)

do.call(c, lapply(1:length(y), function(i) {fastLm(cbind(1,x[-i,]), y[-i], c(1,x[i,]), y[i])$residual}))
