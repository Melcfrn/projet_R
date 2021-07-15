library(Rcpp)
library(parallel)
sourceCpp("toolsFunctions_c.cpp")

basic_computecv_dim1.c <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- do.call(c, lapply(1:length(varcible_vecnum), 
                                        function(j) 
                                        {fastLm(cbind(1,covariables_matnum[-j]), varcible_vecnum[-j], c(1,covariables_matnum[j]), varcible_vecnum[j])$residual}))
  mean(abs(erreurs_vecnum))
}

basic_computecv_dimsup1.c <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- do.call(c, lapply(1:length(varcible_vecnum), 
                                        function(j) 
                                        {fastLm(cbind(1,covariables_matnum[-j,]), varcible_vecnum[-j], c(1,covariables_matnum[j,]), varcible_vecnum[j])$residual}))
  mean(abs(erreurs_vecnum))
}

basic_computecv.c <- function(covariables_matnum, varcible_vecnum)
{
  if (length(covariables_matnum) == length(varcible_vecnum)){
    basic_computecv_dim1.c(covariables_matnum, varcible_vecnum)
  } else {
    basic_computecv_dimsup1.c(covariables_matnum, varcible_vecnum)
  }
}


basic_computecv_dim1.cpara <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- do.call(c, mclapply(1:length(varcible_vecnum), 
                                      function(j) 
                                      {fastLm(cbind(1,covariables_matnum[-j]), varcible_vecnum[-j], c(1,covariables_matnum[j]), varcible_vecnum[j])$residual}
                                      , mc.cores = 20))
  mean(abs(erreurs_vecnum))
}

basic_computecv_dimsup1.cpara <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- do.call(c, mclapply(1:length(varcible_vecnum), 
                                      function(j) 
                                      {fastLm(cbind(1,covariables_matnum[-j,]), varcible_vecnum[-j], c(1,covariables_matnum[j,]), varcible_vecnum[j])$residual}
                                      , mc.cores = 20))
  mean(abs(erreurs_vecnum))
}

basic_computecv.cpara <- function(covariables_matnum, varcible_vecnum)
{
  if (length(covariables_matnum) == length(varcible_vecnum)){
    basic_computecv_dim1.cpara(covariables_matnum, varcible_vecnum)
  } else {
    basic_computecv_dimsup1.cpara(covariables_matnum, varcible_vecnum)
  }
}



meilleur_next_modele.cpara <- function(x_matnum, y_vecnum, indices_matnum) 
{
  erreurs_vecnum <- do.call(c, mclapply(1:length(indices_matnum[1,]), 
                                        function(i)
                                        {basic_computecv.c(x_matnum[,indices_matnum[,i]], y_vecnum)},
                                        mc.cores = 20))
  indices_matnum[,which(erreurs_vecnum==min(erreurs_vecnum))]
}