---
title: "Régression linéaire"
author: "Clément François"
date: "14/05/2021"
---

# A. Organisation des fichiers
Mon code est réparti entre plusieurs fichiers :

- **toolsFunctions.R** : contient toutes les petites fonctions qui sont utilisées dans la fonction de régression basique (*basic_forwardregression*)
- **toolsFunctions_opti.R** : contient toutes les petites fonctions optimisées qui sont utilisées dans la fonction de régression optimisée sans code C (*fastR_forwardregression*)
- **toolsFunction_para.R** : contient toutes les petites fonctions parallélisées qui sont utilisées dans la fonction de régression parallélisée sans code C (*fastRpara_forwardregression*)
- **toolsFunction_c.R** : contient toutes les petites fonctions parallélisées avec du code C qui sont utilisées dans la fonction de régression parallélisée avec code C (*fastRc_forwardregression*)
- **toolsFunction_c.cpp** : contient les fonctions codées en C 
- **testeur.R** : fichier qui teste les fonctions
- **simulations.R** : fichier où je fixe la graine, simule mes variables et lance mes fonctions de régression
- **profilage.R** : analyse mes fonctions afin de connaitre ce qui peut être améliorer (allocation mémoire, temps de calcul) et de comparer des fonctions selon ces mêmes critères
- **mainRegression.R** : contient les fonctions de régression

## A.1. toolsFunction.R

*rmodellinear* permet de générer l'échantillon souhaité. Elle retourne une matrice avec les n-1 premières colonnes correspondantes aux variables x et la dernière colonne correspond au y.

*basic_computecv* retourne l'erreur d'un régression pour certaines covariables. Elle utilise *basic_computecv_dimsup1* ou *basic_computecv_dim1* en fonction du nombre de covariables.

*next.model* réalise ce qui était demandé dans la question 4.

*meilleur_next_modele* reprend les modèle retournés par *next.model* et retourne le meilleur modèle parmis eux selon nos critère.

## A.2. toolsFunctions_opti.R

*basic_computecv.opti* est une version plus rapide de *basic_computecv* grâce aux optimisation sur *basic_computecv_dimsup1* et *basic_computecv_dim1*. Elles seront argumentés par la suite. Il en est de même pour *meilleur_next_modele.opti*.

## A.3. toolsFunctions_para.R

*basic_computecv.para* est une version parallélisée de *basic_computecv.opti* utilisant *basic_computecv_dimsup1.para* et *basic_computecv_dim1.para*.

*meilleur_next_modele.para* est une version parallélisée de *meilleur_next_modele.opti* mais qui à l'intérieur de sa parallélisation reprend la fonction *basic_computecv.opti* et non *basic_computecv.para* car il faut paralléliser à la plus grande échelle possible et ne pas mélanger les parallélisations.

## A.4. toolsFunctions_c.cpp

Code C utilisant le package **RcppArmadillo** pour que certains de nos calculs soient réalisés en C plus rapidement qu'en R.

*fastLm* permet de prendre en compte un indice pour le supprimer de l'estimation. Le code ici n'est pas très propre mais je ne suis pas à l'aise en C et j'ai donc essayé de me débrouiller.
*fastLm_noindex* prend en compte l'ensemble des individus.

## A.5. toolsFunctions_c.R

Ici certaines fonctions reprennent mes fonctions C sans parallélisation et d'autres avec pour que je puisse respecter la parallélisation à la plus grande échelle.

## A.6. testeur.R

Test de certaines fonctions que j'utilise au fur et à mesure.

## A.7. simulations.R

Simulation de mes données test grâce à *rmodellinear* et fixation de la graine.

## A.8. profilage.R

Macro_profilage de chaque fonction de régression et comparaison entre différentes fonctions.

## A.9. mainRegression.R

Regroupe mes différentes régressions :

- basic_forwardregression : non optimisée
- fastR_forwardregression : version optimisée de la précédente
- fastRpara_forwardregression : version parallélisée de la précédente
- fastRc_forwardregression : version parallélisée avec code C

# B. La régression sans optimisation

Dans cette section je cherche simplement à réaliser un code qui fonctionne sans me poser de questions de rapidité d'exécution ou d'allocation mémoire. J'utilise donc la fonction *basic_forwardregression* qui n'utilise pas de sous fonctions optimisées (fichier **toolsFunctions_opti.R**).

Dans les codes je remettrai souvent la simulation de mon échantillon pour savoir à chaque fois ce qu'on analyse.

Profilage de ma fonction *basic_forwardregression* :
```{r}
library(profvis)
source(file = "mainRegression.R")

set.seed(123)
ech <- 500
n <- 10
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]

prof <- profvis({
  basic_forwardregression(x, y)
}) 
prof
```

## B.1. La fonction lm()
On remarque que ce qui est coûteux en temps de calcul et allocation mémoire est la fonction *lm()*. Elle est répétée plusieurs fois dans la fonction *basic_computecv* et cette fonction est elle même dans une boucle de la fonction *meilleur_next_modele*. Je cherche donc en priorité à changer la fonction *lm()* car on ne veut récupérer que les coefficients.

## B.2. Déclarer une variable en initialisant sa taille
Ici il est compliqué de voir ce qui est coûteux en dehors de la fonction *lm()* car elle écrase tout le reste en temps de calcul. Néanmoins je sais que déclarer la taille de ma variable à sa création est moins coûteux que de faire un object de taille croissante.


```{r}
library(bench)
mbm <- mark(taille_croiss= {vec <- c()
                            for (i in 1:10**4) { vec <- c(vec, i)}
                            vec},
            taille_const= {vec <- rep(NA, 10**4)
                           for (i in 1:10**4) {vec[i] <- i}
                           vec})
plot(mbm)
```


## B.3 Bon fonctionnement de ma fonction de régression avec sélection des variables

Une première analyse du bon fonctionnement consiste à vérifier que les estimateurs de ma régression sont proches de mes alpha et beta initialisés.

```{r}
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

summary(lm(y~x))
```

Il s'agit également de vérifier la sélection des variables. Ce point pose problème car en effet certains beta sont égaux à 0 et das le summary précédent il ne sont pas significatifs. Mais dans ma fonction je regarde juste si l'erreur diminue et donc cela diffère des significativités du summary.

```{r}

basic_forwardregression(x[,-6],y)$erreur
basic_forwardregression(x[,-1],y)$erreur
basic_forwardregression(x,y)$erreur

```
Ici quand je regarde le modèle sans la variable 1, l'erreur est plus élevée, il est donc normal de la conserver. Par contre quand j'enlève la variable 6, l'erreur ne change pas, il est donc normal de l'enlever car elle n'améliore pas le modèle selon les critère retenus.

Dans les modèles suivant je vérifie automatiquement que les résultats sont égaux puisque pour comparer les temps de calculs les résultats doivent être égaux.

# C. La régression avec optimisation et parallélisation
## C.1. Optimisation
Avant de paralléliser je souhaite optimiser mon code en reprennant les points énoncés dans la partie précédente. Je souhaite comparer plusieurs méthodes pour remplacer la fonction *lm()*.

```{r}
library(bench)
set.seed(123)
ech <- 50
n <- 5
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]
mbm2 <- mark(lm= as.numeric({lm(y ~ x)$coefficients}),
            direct= as.numeric(solve(t(cbind(1,x)) %*% cbind(1,x)) %*% (t(cbind(1,x)) %*% y)),
            interne_lm= {.lm.fit(cbind(1,x), y)$coefficients})
plot(mbm2)

ech <- 1000
n <- 20
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]
mbm2 <- mark(lm= as.numeric({lm(y ~ x)$coefficients}),
            direct= as.numeric(solve(t(cbind(1,x)) %*% cbind(1,x)) %*% (t(cbind(1,x)) %*% y)),
            interne_lm= {.lm.fit(cbind(1,x), y)$coefficients})
plot(mbm2)
```

Pour moins de variables et un plus petit échantillon, la fonction *.lm.fit()* est la meilleure mais elle est égalisée par le calcul direct pour plus de variables et un plus gros échantillon. Je fais donc le choix de conserver la fonction *.lm.fit()*.

J'ai par ailleurs initialisé la taille de certaines variables à leur création.


Comparaison de mes deux fonction de régression
```{r}
library(bench)
set.seed(123)
ech <- 500
n <- 10
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]

mbm3 <- mark(basic= {basic_forwardregression(x, y)},
             fastR= {fastR_forwardregression(x, y)},
             iterations = 1)
plot(mbm3)
```

Le temps de calcul semble avoir été divisé par 10.

Profilage de ma fonction *fastR_forwardregression* :
```{r}
library(profvis)
set.seed(123)
ech <- 500
n <- 10
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]

prof2 <- profvis({
  fastR_forwardregression(x, y)
}) 
prof2
```

La quantité de mémoire allouée a été diminué et le temps de calcul également. C'est toujours la fonction *.lm.fit()* qui prend du temps étant donné qu'elle est répétée de nombreuses fois. 

## C.2. Parallélisation
Ici je reprend mes fonctions optimisées et y ajoute la parallélisation.

Comparaison de mes régressions optimisées et parallélisées
```{r}
library(bench)
set.seed(123)
ech <- 1000
n <- 30
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]

mbm4 <- mark(fastR= {fastR_forwardregression(x,y)},
             fastRpara= {fastRpara_forwardregression(x,y)},
             iterations = 1,
             memory = FALSE)
plot(mbm4)
```

Plus l'échantillon est grand avec de nombreuses variables, plus ma régression parallélisée est meilleure. Néanmoins un un petit échantillon elle est moins rapide, cela étant dû à l'échange des données entre les cores.


Ma fonction qui prend du temps de calcul est donc *meilleur_next_modele...*. Je les compare pour voir si j'ai réussi à l'améliorer sans code C.
```{r}
rm(list=ls())
source(file = "mainRegression.R")
library(bench)
set.seed(123)
ech <- 1000
n <- 30
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]

ind <- next.model(1:(round(n/2)), n)
mbm5 <-mark(basic= {meilleur_next_modele(x, y, ind)},
            opti= {meilleur_next_modele.opti(x, y, ind)},
            para= {meilleur_next_modele.para(x, y, ind)},
            iterations = 1,
            memory = FALSE)
plot(mbm5)
```


Profilage de ma fonction *fastRpara_forwardregression* :
Je n'arrive pas à sortir le profilage en Rmarkdown mais il peut-être lancer dans le fichier profilage.R

J'ai du néanmoins faire une erreur à un moment dans la parallélisation car quand je lance plusieurs fois de suite ma fonction, au bout d'un moment je reçois une erreur que je ne comprend pas. Error in do.call(c, mclapply(1:length(indices_matnum[1, ]), function(i) { : 
  'what' must be a function or character string )
  

# D. La régression avec code C


```{r}
rm(list=ls())
source(file = "mainRegression.R")
library(profvis)
set.seed(123)
ech <- 500
n <- 10
alpha <- 4
beta <- rbinom(n,3,1/4)
tab <- rmodellinear(ech,alpha,beta)
x <- tab[,1:n]
y <- tab[,n+1]

fastR_forwardregression(x, y)$estimateurs == fastRc_forwardregression(x,y)$estimateurs
fastR_forwardregression(x, y)$estimateurs
fastRc_forwardregression(x,y)$estimateurs
```

Je ne sais pas pourquoi mes deux vecteurs sont différents (une histoire d'arrondi ?), je ne peux donc pas comparer ma régression avec du code C aux autre. Mon erreur calculée est donc également différente.
