source(file = "toolsFunctions.R")
source(file = "toolsFunctions_opti.R")
source(file = "toolsFunctions_para.R")
source(file = "toolsFunctions_c.R")
sourceCpp("toolsFunctions_c.cpp")

basic_forwardregression <- function(x_matnum, y_vecnum)
{
  if (length(x_matnum) == length(y_vecnum)){
    nvar_num <- 1
    liste_retour <- list(indices = 1, 
                         estimateurs = lm(y_vecnum~x_matnum)$coefficients, 
                         erreur = basic_computecv(x_matnum, y_vecnum))
  } else {
    nvar_num <- ncol(x_matnum)
    indices_test_matnum <- next.model(n_num =  nvar_num)
    indices_conserve_vecnum <- meilleur_next_modele(x_matnum, y_vecnum, indices_test_matnum)
    erreur_prec_num <- basic_computecv(x_matnum[,indices_conserve_vecnum],
                                       y_vecnum)
    continue <- TRUE
    while ((continue == TRUE) & (length(indices_conserve_vecnum) < nvar_num)) {
      indices_test_matnum <- next.model(indices_conserve_vecnum, nvar_num)
      indices_temp_vecnum <- meilleur_next_modele(x_matnum, y_vecnum, indices_test_matnum)
      erreur_new_num <- basic_computecv(x_matnum[,indices_temp_vecnum], y_vecnum)
      if (erreur_new_num < erreur_prec_num){
        erreur_prec_num <- erreur_new_num
        indices_conserve_vecnum <- indices_temp_vecnum
      } else {
        continue <- FALSE
      }
    }
    liste_retour <- list(indices = indices_conserve_vecnum, 
                         estimateurs = as.numeric(lm(y_vecnum~x_matnum[,indices_conserve_vecnum])$coefficients), 
                         erreur = erreur_prec_num)
  }
}

fastR_forwardregression <- function(x_matnum, y_vecnum)
{
  if (length(x_matnum) == length(y_vecnum)){
    nvar_num <- 1
    liste_retour <- list(indices = 1, 
                         estimateurs = .lm.fit(cbind(1,x_matnum), y_vecnum)$coefficients, 
                         erreur = basic_computecv.opti(x_matnum, y_vecnum))
  } else {
    nvar_num <- ncol(x_matnum)
    indices_test_matnum <- next.model(n_num =  nvar_num)
    indices_conserve_vecnum <- meilleur_next_modele.opti(x_matnum, y_vecnum, indices_test_matnum)
    erreur_prec_num <- basic_computecv.opti(x_matnum[,indices_conserve_vecnum],
                                       y_vecnum)
    continue <- TRUE
    while ((continue == TRUE) & (length(indices_conserve_vecnum) < nvar_num)) {
      indices_test_matnum <- next.model(indices_conserve_vecnum, nvar_num)
      indices_temp_vecnum <- meilleur_next_modele.opti(x_matnum, y_vecnum, indices_test_matnum)
      erreur_new_num <- basic_computecv.opti(x_matnum[,indices_temp_vecnum], y_vecnum)
      if (erreur_new_num < erreur_prec_num){
        erreur_prec_num <- erreur_new_num
        indices_conserve_vecnum <- indices_temp_vecnum
      } else {
        continue <- FALSE
      }
    }
    liste_retour <- list(indices = indices_conserve_vecnum, 
                         estimateurs = .lm.fit(cbind(1,x_matnum[,indices_conserve_vecnum]), y_vecnum)$coefficients,
                         erreur = erreur_prec_num)
  }
}

fastRpara_forwardregression <- function(x_matnum, y_vecnum)
{
  if (length(x_matnum) == length(y_vecnum)){
    nvar_num <- 1
    liste_retour <- list(indices = 1, 
                         estimateurs = .lm.fit(cbind(1,x_matnum), y_vecnum)$coefficients, 
                         erreur = basic_computecv.para(x_matnum, y_vecnum))
  } else {
    nvar_num <- ncol(x_matnum)
    indices_test_matnum <- next.model(n_num =  nvar_num)
    indices_conserve_vecnum <- meilleur_next_modele.para(x_matnum, y_vecnum, indices_test_matnum)
    erreur_prec_num <- basic_computecv.para(x_matnum[,indices_conserve_vecnum],
                                            y_vecnum)
    continue <- TRUE
    while ((continue == TRUE) & (length(indices_conserve_vecnum) < nvar_num)) {
      indices_test_matnum <- next.model(indices_conserve_vecnum, nvar_num)
      indices_temp_vecnum <- meilleur_next_modele.para(x_matnum, y_vecnum, indices_test_matnum)
      erreur_new_num <- basic_computecv.para(x_matnum[,indices_temp_vecnum], y_vecnum)
      if (erreur_new_num < erreur_prec_num){
        erreur_prec_num <- erreur_new_num
        indices_conserve_vecnum <- indices_temp_vecnum
      } else {
        continue <- FALSE
      }
    }
    liste_retour <- list(indices = indices_conserve_vecnum, 
                         estimateurs = .lm.fit(cbind(1,x_matnum[,indices_conserve_vecnum]), y_vecnum)$coefficients,
                         erreur = erreur_prec_num)
  }
}


fastRc_forwardregression <- function(x_matnum, y_vecnum)
{
  if (length(x_matnum) == length(y_vecnum)){
    nvar_num <- 1
    reg <- fastLm_noindex(cbind(1,x_matnum), y_vecnum)
    liste_retour <- list(indices = 1, 
                         estimateurs = as.numeric(reg$coefficients), 
                         erreur = basic_computecv.cpara(x_matnum, y_vecnum))
  } else {
    nvar_num <- ncol(x_matnum)
    indices_test_matnum <- next.model(n_num =  nvar_num)
    indices_conserve_vecnum <- meilleur_next_modele.cpara(x_matnum, y_vecnum, indices_test_matnum)
    erreur_prec_num <- basic_computecv.cpara(x_matnum[,indices_conserve_vecnum],
                                            y_vecnum)
    continue <- TRUE
    while ((continue == TRUE) & (length(indices_conserve_vecnum) < nvar_num)) {
      indices_test_matnum <- next.model(indices_conserve_vecnum, nvar_num)
      indices_temp_vecnum <- meilleur_next_modele.cpara(x_matnum, y_vecnum, indices_test_matnum)
      erreur_new_num <- basic_computecv.cpara(x_matnum[,indices_temp_vecnum], y_vecnum)
      if (erreur_new_num < erreur_prec_num){
        erreur_prec_num <- erreur_new_num
        indices_conserve_vecnum <- indices_temp_vecnum
      } else {
        continue <- FALSE
      }
    }
    reg <- fastLm_noindex(cbind(1,x_matnum[,indices_conserve_vecnum]), y_vecnum)
    liste_retour <- list(indices = indices_conserve_vecnum, 
                         estimateurs = as.numeric(reg$coefficients),
                         erreur = erreur_prec_num)
  }
}