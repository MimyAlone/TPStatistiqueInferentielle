#-----------------------------------------------------------------------------------
# TP3: TESTS D'HYPOTHESES
#-----------------------------------------------------------------------------------
# Lecture des donnees
donnees <- read.csv("C:/Users/Gribouille/Documents/TP/tp3/sinistres.csv", header=TRUE, sep=';', dec=',')
colnames(donnees)<-"Montants"

#===================================================================================
# Q1 : Stocker des echantillons dans une matrice
#===================================================================================

# Methode 1 : temps execution long
matrice <- replicate(1000, sample(donnees$Montants, size=30, replace=TRUE))
matrice<- t(matrice)

# Methode 2 : temps execution plus court !
simulation<-function(longueur_echantillon,nombre_simulation){
  vecteur<-sample(donnees$Montants, longueur_echantillon)
  n<-nombre_simulation-1
  for( i in 1:n){vecteur<-c(vecteur,sample(donnees$Montants, longueur_echantillon))}
  matrice<-matrix(vecteur,nrow = nombre_simulation,ncol=longueur_echantillon,byrow = TRUE)
  return(matrice)
}

matrice<-simulation(30,1000)

#===================================================================================
# Q2 : Test de nullite de la moyenne
#===================================================================================

# Methode 1: avec la statistique de test
test_moyenne_1 <- function(donnees, sigma, alpha){
  reponse<-1
  statistique_de_test <- mean(donnees)/(sigma/sqrt(length(donnees)))
  if (abs(statistique_de_test)>qnorm(1-alpha/2)){reponse<-0}
  return(reponse)
}

# Methode 2 : avec la p-value
test_moyenne_2 <- function(donnees, sigma, alpha){
  statistique_de_test <- mean(donnees)/(sigma/sqrt(length(donnees)))
  result <- ifelse(2*(1-pnorm(abs(statistique_de_test)))<alpha, 0, 1)
  return(result)
}

#===================================================================================
# Q3 : Application du test de la moyenne
#===================================================================================
# Valeurs par defaut :
SIGMA <- 60
ALPHA <- 0.05

# On effectue simplement le test pour chaque ligne de la matrice.
# En effet, une ligne correspond a un echantillon.

RESULTAT_1<- apply(matrice, 1, test_moyenne_1, SIGMA, ALPHA)
(NOMBRE_REJETS_1 <-length(which(RESULTAT_1==0)))


RESULTAT_2 <- apply(matrice, 1, test_moyenne_2, SIGMA, ALPHA)
(NOMBRE_REJETS_2 <-length(which(RESULTAT_2==0)))

#===================================================================================
# Q4 : P value
#===================================================================================

p_value <- function(donnees, sigma){
  statistique <- mean(donnees)/(sigma/sqrt(length(donnees)))
  result <- 2*(1-pnorm(abs(statistique)))
  return(result)
}

p_values <- apply(matrice-650, 1, p_value, SIGMA) 
(NOMBRE_REJETS_3<- length(p_values[p_values<ALPHA]))

#===================================================================================
# Q5: Puissance d'un test
#===================================================================================

PUISSANCE<-function(n,sigma,alpha,delta=0){
  z_alpha<-qnorm(1-alpha/2)
  d<- delta/sigma
  return(1-(pnorm(z_alpha-d*sqrt(n))-pnorm(-z_alpha-d*sqrt(n))))
}

PUISSANCE(30,SIGMA,ALPHA)

# Graphiques
abscisses <- seq(-50, 50, by=0.1)
ordonnees_30<-abscisses
ordonnees_50<-abscisses
ordonnees_100<-abscisses
l<-length(abscisses)
for(i in 1:l){
  ordonnees_30[i]<-PUISSANCE(30,SIGMA,ALPHA,ordonnees_30[i])
  ordonnees_50[i]<-PUISSANCE(50,SIGMA,ALPHA,ordonnees_50[i])
  ordonnees_100[i]<-PUISSANCE(100,SIGMA,ALPHA,ordonnees_100[i])
}

plot(abscisses,ordonnees_30,col="#2FAD59",type="l",xlab="delta",ylab="puissance")
lines(abscisses,ordonnees_50,col="#332FAD")
lines(abscisses,ordonnees_100,col="#D62E8A")

#===================================================================================
# Q6: Variance inconnue
#===================================================================================
# On estime la variance par s= (sum(echantillon-mean(echantillon))**2)/(n-1)
# Attention : Z suit une loi de Student a n-1 degres de libertes

#Q2
test_moyenne <- function(donnees, alpha){
  n<-30
  S<-sqrt(sum((donnees-mean(donnees))**2)/(n-1) )
  statistique_de_test <- mean(donnees)/(S/sqrt(length(donnees)))
  result <- ifelse(2*(1-pt(abs(statistique_de_test),df=n-1))<alpha, 0, 1)
  return(result)
}

#Q3
resultat <- apply(matrice-650, 1, test_moyenne,ALPHA)
(NOMBRE_REJETS<- length(resultat[resultat<ALPHA]))


#Q4

p_value_student <- function(donnees,n){
  S<-sqrt(sum((donnees-mean(donnees))**2)/(n-1))
  statistique <- mean(donnees)/(S/sqrt(length(donnees)))
  result <- 2*(1-pt(abs(statistique),df=n-1))
  return(result)
}

p_values <- apply(matrice-650, 1, p_value_student,30) 
(NOMBRE_REJETS<- length(p_values[p_values<ALPHA]))

#Q5

PUISSANCE<-function(n,alpha,delta){
  sigma<-sqrt(sum((matrice[2,]-mean(matrice[2,]))**2)/(n-1))
  z_alpha<-qt(1-(alpha/2),df=n-1)
  d<- (-delta)/sigma
  return(1-(pt(z_alpha-d*sqrt(n),df=n-1)-pt(-z_alpha-d*sqrt(n),df=n-1)))
}


abscisses <- seq(-50, 50, by=0.1)
ordonnees_30<-abscisses
l<-length(abscisses)
for(i in 1:l){
  ordonnees_30[i]<-PUISSANCE(30,ALPHA,ordonnees_30[i])
}

plot(abscisses,ordonnees_30,col="#2FAD59",type="l",xlab="delta",ylab="puissance")

# Remarque : Utilisation de t.test
p_valeur<-function(donnees,moyenne){
  return(t.test(donnees, mu=moyenne,alternative="two.sided")$p.value)
}

p_values <- apply(matrice, 1, p_valeur,650) 
(NOMBRE_REJETS<- length(p_values[p_values<ALPHA]))


# Remarque : Utilisation de power.t.test

install.packages('pwr')
library('pwr')

PUISSANCE_STUDENT<- function(donnees,taille, delta=0){
  S<-sqrt(sum((donnees-mean(donnees))**2)/(taille-1))
  result <- pwr.t.test(d = delta/S, n = taille, sig.level = 0.05, alternative = "two.sided")$power
  return(result)
}

abscisses <- seq(-50, 50, by=0.1)
ordonnees_30<-abscisses
l<-length(abscisses)
for(i in 1:l){
  ordonnees_30[i]<-PUISSANCE_STUDENT(matrice[1,],30,ordonnees_30[i])
}

plot(abscisses,ordonnees_30,col="#2FAD59",type="l",xlab="delta",ylab="puissance")

#===================================================================================
# Q7: Recherche de n
#===================================================================================
# Ceiling arrondit un nombre décimal a l'entier superieur
(ceiling(pwr.norm.test(d = 20/SIGMA, power = 0.9, sig.level = ALPHA, alternative = "two.sided")$n))