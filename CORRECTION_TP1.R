#-----------------------------------------------------------------------------------
# TP: ESTIMATEURS DU MAXIMUM DE VRAISEMBLANCE 
#-----------------------------------------------------------------------------------
# Lecture des donnees
donnees <- read.csv("C:/Users/Gribouille/Documents/TP/tp1/fiabilites.csv", header=FALSE, sep=';', dec=',')

#===================================================================================
# Q1 : Simulation d'un echantillon d'une loi discrete.
#===================================================================================

echantillon_30 <- sample(donnees$V1, 30)
hist(echantillon_30,
     main="Distribution de plusieurs temps de fonctionnement avant defaillance",
     xlab="Temps de fonctionnement avant la defaillance (en annees)",
     ylab="Effectifs",
     col="#0080FF")

echantillon_100 <- sample(donnees$V1, 100)
hist(echantillon_100,
     main="Distribution de plusieurs temps de fonctionnement avant defaillance",
     xlab="Temps de fonctionnement avant la defaillance (en annees)",
     ylab="Effectifs",
     col="#0080FF")

echantillon_500 <- sample(donnees$V1, 500)
hist(echantillon_500,
     main="Distribution de plusieurs temps de fonctionnement avant defaillance",
     xlab="Temps de fonctionnement avant la defaillance (en annees)",
     ylab="Effectifs",
     col="#0080FF")

#===================================================================================
# Q2 : Log vraisemblance
#===================================================================================

# Methode 1 : ecrire directement l'expression de la densite trouvee en cours
# N.B : R sait resoudre un probleme de minimisation mais pas de maximisation
#       On ecrit donc l'oppose de la fonction log-vraisemblance qu'on minimisera
#       avec R par la suite (ce qui revient Ã  maximiser la log-vraisemblance !)

LogVraisemblance <- function(parametres, echantillon){
  mu<-parametres[1]
  sigma<-parametres[2]
  n <- length(echantillon)
  logv <- -(n/2)*log(2*pi*(sigma^2)) - sum(log(echantillon)) - (1/(2*sigma^2))*sum((log(echantillon)-mu)^2)
  return(-logv) 
}

# Methode 2 : utiliser directement la fontion de densite predefinie sous R
# N.B log=TRUE alors le log de la fonction de densite est retourne
# N.B : R sait resoudre un probleme de minimisation mais pas de maximisation
#       On ecrit donc l'oppose de la fonction log-vraisemblance qu'on minimisera
#       avec R par la suite (ce qui revient a maximiser la log-vraisemblance !

LogVraisemblance_2 <- function(parametres, echantillon){
  mu<-parametres[1]
  sigma<-parametres[2]
  return(-sum(dlnorm(echantillon, meanlog = mu, sdlog = sigma, log = TRUE))) # signe negatif car fonction d'optimisation minimise
}

#===================================================================================
# Q3 : Optimisation
#===================================================================================

# La fonction optim prend en arguments :
# - Un vecteur avec les valeurs initiales des parametres a optimiser (mu et sigma)
# - le nom de la fonction a optimiser
# - les parametres fixes de la fonction a optimiser ( echantillon ici)

# Methode 1
(optim_30  <- optim(c(1,1), LogVraisemblance, echantillon=echantillon_30))
(optim_100 <- optim(c(1,1), LogVraisemblance, echantillon=echantillon_100))
(optim_500 <- optim(c(1,1), LogVraisemblance, echantillon=echantillon_500))

# Methode 2 
(optim_2_30  <- optim(c(1,1), LogVraisemblance_2, echantillon=echantillon_30,method="L-BFGS-B",lower = c(0.1,0.2)))
(optim_2_100 <- optim(c(1,1), LogVraisemblance_2, echantillon=echantillon_100,method="L-BFGS-B",lower = c(0.1,0.1)))
(optim_2_500 <- optim(c(1,1), LogVraisemblance_2, echantillon=echantillon_500,method="L-BFGS-B",lower = c(0.1,0.1)))

#===================================================================================
# Q4 : Methode analytique
#===================================================================================

# Formule a celle du cours pour les estimations de mu et sigma.
# Attention car les observations suivent une loi Log-Normale !
# On remplace donc les Xi par log(Xi)

# mu = (1/n) * sum(log(X))
(Mu_30<-mean(log(echantillon_30)))
(Mu_100<-mean(log(echantillon_100)))
(Mu_500<-mean(log(echantillon_500)))

# On retrouve l'ecart-type en prenant la racine de la variance estimee formule usuelle
# sigma = sqrt{ (1/n) * sum((log(X)-mu)^2) }
# equivalent a sqrt(mean(log(X)^2)-Mu^2)
# equivalent a sd(log(echantillon))*sqrt(n/n-1)

(std_30 <- sqrt((1/30)*sum((log(echantillon_30)-Mu_30)^2)))
(std_100 <- sqrt((1/100)*sum((log(echantillon_100)-Mu_100)^2)))
(std_500 <- sqrt((1/500)*sum((log(echantillon_500)-Mu_500)^2)))

#===================================================================================
# Q5 : Autres estimateurs
#===================================================================================

# En utilisant la definition de la loi log normale on a l'expression de la
# variance V[X] et l'esperance E[X]. On resoud alors le syteme pour trouver 
# mu = ln(E[X]) -  (ln{1 + (V[X]/E[X]^2)})/2 et 
# sigma mu = ln(E[X]) -  (ln{1 + (V[X]/E[X]^2)})/2

(E_X_30 <- mean(echantillon_30))
(E_X_100 <- mean(echantillon_100))
(E_X_500 <- mean(echantillon_500))

(V_X_30 <- var(echantillon_30))
(V_X_100 <- var(echantillon_100))
(V_X_500 <- var(echantillon_500))

Parametres <- function(E_X,V_X){
  var <- log(1 + (V_X/(E_X^2)))
  sigma <- sqrt(var)
  mu  <- log(E_X) - var/2
  return (c(mu,sigma))
}

(estimateurs_30 <-Parametres(E_X_30,V_X_30))
(estimateurs_100 <-Parametres(E_X_100,V_X_100))
(estimateurs_500 <-Parametres(E_X_500,V_X_500))

#===================================================================================
# Q6 : Synthèse
#===================================================================================

mu_1<-c(optim_30$par[1],optim_100$par[1],optim_500$par[1])
mu_2<-c(optim_2_30$par[1],optim_2_100$par[1],optim_2_500$par[1])
mu_3<-c(estimateurs_30[1],estimateurs_100[1],estimateurs_500[1])
mu_4<-c(Mu_30,Mu_100,Mu_500)

std_1<-c(optim_30$par[2],optim_100$par[2],optim_500$par[2])
std_2<-c(optim_2_30$par[2],optim_2_100$par[2],optim_2_500$par[2])
std_3<-c(estimateurs_30[2],estimateurs_100[2],estimateurs_500[2])
std_4<-c(std_30,std_100,std_500)

data<-data.frame(mu_1,mu_2,mu_3,mu_4,std_1,std_2,std_3,std_4)
rownames(data)<-c("echantillon_30","echantillon_100","echantillon_500")

