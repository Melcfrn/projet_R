rmodellinear <- function(n_num, alpha_num, beta_vecnum)
{
  eps_vecnum <- rnorm(n_num)
  X_matnum <- matrix(rnorm(n_num*length(beta_vecnum)), nrow=n_num)
  Y_vecnum <- alpha_num + X_matnum %*% beta_vecnum + eps_vecnum
  ech_matnum <- cbind(X_matnum, Y_vecnum)
}



basic_computecv_dim1 <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- c()
  for (i in 1:length(varcible_vecnum)){
    X_train <- covariables_matnum[-i]
    Y_train <- varcible_vecnum[-i]
    reg <- lm(Y_train~X_train)$coefficients
    alpha_num <- reg[1]
    beta_num <- reg[2]
    erreur_num <- abs((alpha_num + covariables_matnum[i] %*% beta_num) - varcible_vecnum[i])
    erreurs_vecnum <- c(erreurs_vecnum, erreur_num)
  }
  mean(erreurs_vecnum)
}

basic_computecv_dimsup1 <- function(covariables_matnum, varcible_vecnum)
{
  erreurs_vecnum <- c()
  for (i in 1:length(varcible_vecnum)){
    X_train <- covariables_matnum[-i,]
    Y_train <- varcible_vecnum[-i]
    reg <- lm(Y_train~X_train)$coefficients
    alpha_num <- reg[1]
    beta_vecnum <- reg[2:length(reg)]
    erreur_num <- abs((alpha_num + covariables_matnum[i,] %*% beta_vecnum) - varcible_vecnum[i])
    erreurs_vecnum <- c(erreurs_vecnum, erreur_num)
  }
  mean(erreurs_vecnum)
}

basic_computecv <- function(covariables_matnum, varcible_vecnum)
{
  if (length(covariables_matnum) == length(varcible_vecnum)){
    basic_computecv_dim1(covariables_matnum, varcible_vecnum)
  } else {
    basic_computecv_dimsup1(covariables_matnum, varcible_vecnum)
  }
}

next.model <- function(indices_vecnum=NULL, n_num)
{
  if (is.null(indices_vecnum)){
    return(rbind(1:n_num))
  }else{
    modeles <- c()
    reste_indices <- (1:n_num)[-indices_vecnum]
    for (i in reste_indices){
      indices <- sort(c(indices_vecnum, i))
      modeles <- cbind(modeles, indices)
    }
    return(modeles)
  }
}

meilleur_next_modele <- function(x_matnum, y_vecnum, indices_matnum) 
{
  erreurs_vec <- c()
  for(i in 1:length(indices_matnum[1,])){
    erreur_num <- basic_computecv(x_matnum[,indices_matnum[,i]], y_vecnum)
    erreurs_vec <- c(erreurs_vec, erreur_num)
  }
  return(indices_matnum[,which(erreurs_vec==min(erreurs_vec))])
}


