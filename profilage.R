rm(list=ls())
library(profvis)
library(bench)
source(file = "mainRegression.R")
source(file = "simulations.R")



prof <- profvis({
  basic_forwardregression(x, y)
}) 
prof

mbm <- mark(taille_croiss= {vec <- c()
                            for (i in 1:10**4) { vec <- c(vec, i)}
                            vec},
            taille_const= {vec <- rep(NA, 10**4)
                           for (i in 1:10**4) {vec[i] <- i}
                           vec})
plot(mbm)

mbm2 <- mark(lm= as.numeric({lm(y ~ x)$coefficients}),
            direct= as.numeric(solve(t(cbind(1,x)) %*% cbind(1,x)) %*% (t(cbind(1,x)) %*% y)),
            interne_lm= {.lm.fit(cbind(1,x), y)$coefficients})
plot(mbm2)

mbm3 <- mark(basic= {basic_forwardregression(x, y)},
             fastR= {fastR_forwardregression(x, y)},
             iterations = 3)
plot(mbm3)

prof2 <- profvis({
  fastR_forwardregression(x, y)
}) 
prof2

mbm4 <- mark(fastR= {fastR_forwardregression(x,y)},
             fastRpara= {fastRpara_forwardregression(x,y)},
             iterations = 2,
             memory = FALSE)
plot(mbm4)

ind <- next.model(1:(round(n/2)), n)
mbm5 <-mark(basic= {meilleur_next_modele(x, y, ind)},
            opti= {meilleur_next_modele.opti(x, y, ind)},
            para= {meilleur_next_modele.para(x, y, ind)},
            memory = FALSE)
plot(mbm5)


prof3 <- profvis({
  fastRpara_forwardregression(x, y)
}) 
prof3

prof4 <- profvis({
  fastRc_forwardregression(x, y)
}) 
prof4