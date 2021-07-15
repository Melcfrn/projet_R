basic_computecv_dim1.opti <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- rep(NA, length(varcible_vecnum))
  for (i in 1:length(varcible_vecnum)){
    reg <- .lm.fit(cbind(1, covariables_matnum[-i]), varcible_vecnum[-i])$coefficients
    erreurs_vecnum[i] <- abs((reg[1] + covariables_matnum[i] %*% reg[2]) - varcible_vecnum[i])
  }
  mean(erreurs_vecnum)
}

basic_computecv_dimsup1.opti <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- rep(NA, length(varcible_vecnum))
  for (i in 1:length(varcible_vecnum)){
    reg <- .lm.fit(cbind(1, covariables_matnum[-i,]), varcible_vecnum[-i])$coefficients
    erreurs_vecnum[i] <- abs((reg[1] + covariables_matnum[i,] %*% reg[2:length(reg)]) - varcible_vecnum[i])
  }
  mean(erreurs_vecnum)
}

basic_computecv.opti <- function(covariables_matnum, varcible_vecnum)
{
  if (length(covariables_matnum) == length(varcible_vecnum)){
    basic_computecv_dim1.opti(covariables_matnum, varcible_vecnum)
  } else {
    basic_computecv_dimsup1.opti(covariables_matnum, varcible_vecnum)
  }
}

meilleur_next_modele.opti <- function(x_matnum, y_vecnum, indices_matnum) 
{
  erreurs_vec <- rep(NA, length(indices_matnum[1,]))
  for(i in 1:length(indices_matnum[1,])){
    erreurs_vec[i] <- basic_computecv.opti(x_matnum[,indices_matnum[,i]], y_vecnum)
  }
  return(indices_matnum[,which(erreurs_vec==min(erreurs_vec))])
}

