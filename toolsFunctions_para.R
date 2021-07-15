library(parallel)

basic_computecv_dim1.para <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- do.call(c, mclapply(1:length(varcible_vecnum), 
                                        function(i) 
                                          {reg <- .lm.fit(cbind(1, covariables_matnum[-i]), varcible_vecnum[-i])$coefficients
                                          abs((reg[1] + covariables_matnum[i] %*% reg[2]) - varcible_vecnum[i])}
                                        , mc.cores = 20))
  mean(erreurs_vecnum)
}

basic_computecv_dimsup1.para <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- do.call(c, mclapply(1:length(varcible_vecnum), 
                                        function(i) 
                                          {reg <- .lm.fit(cbind(1, covariables_matnum[-i,]), varcible_vecnum[-i])$coefficients
                                          abs((reg[1] + covariables_matnum[i,] %*% reg[2:length(reg)]) - varcible_vecnum[i])}
                                        , mc.cores = 20))
  mean(erreurs_vecnum)
}

basic_computecv.para <- function(covariables_matnum, varcible_vecnum)
{
  if (length(covariables_matnum) == length(varcible_vecnum)){
    basic_computecv_dim1.para(covariables_matnum, varcible_vecnum)
  } else {
    basic_computecv_dimsup1.para(covariables_matnum, varcible_vecnum)
  }
}

meilleur_next_modele.para <- function(x_matnum, y_vecnum, indices_matnum) 
{
  erreurs_vecnum <- do.call(c, mclapply(1:length(indices_matnum[1,]), 
                                        function(i)
                                          {basic_computecv.opti(x_matnum[,indices_matnum[,i]], y_vecnum)},
                                        mc.cores = 20))
  indices_matnum[,which(erreurs_vecnum==min(erreurs_vecnum))]
}