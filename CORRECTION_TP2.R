#-----------------------------------------------------------------------------------
# TP: INTERVALLE DE CONFIANCE
#-----------------------------------------------------------------------------------
# Lecture des donnees
donnees <- read.csv("C:/Users/Gribouille/Documents/TP/tp2/poids.csv", header=FALSE, sep=';', dec=',')

#===================================================================================
# Q1 : Moyenne empirique et variance empirique
#===================================================================================

# Moyenne empirique
moyenne<-mean(donnees$V1)

# Variance empirique
variance<-mean((donnees$V1-moyenne)**2)

#===================================================================================
# Q2 : Creation de la fonction
#===================================================================================

analyse<-function(n){
  echantillon<- sample(donnees$V1,n)
  moyenne<-mean(echantillon)
  variance<-mean((echantillon-moyenne)**2)
  ecart_type<-sqrt(variance)
  resultat<- list(echantillon,moyenne,variance,ecart_type)
  return(resultat)
}

#Exemples
(echantillon_30<-analyse(30))
(echantillon_60<-analyse(60))
(echantillon_80<-analyse(80))

#===================================================================================
# Q3 : Variance connue
#===================================================================================

IC<-function(vecteur,variance,alpha){
  moyenne<-mean(vecteur)
  s<-sqrt(variance/length(vecteur))
  q<-qnorm(1-alpha/2)
  paste("[",round(moyenne,2),"±",round(q*s,2),"]")
}

# Exemples
IC(echantillon_30[[1]],438.9**2,0.05)
IC(echantillon_60[[1]],438.9**2,0.05)
IC(echantillon_80[[1]],438.9**2,0.05)

#===================================================================================
# Q4 : Variance inconnue
#===================================================================================

IC_BIS<-function(vecteur,alpha){
  moyenne<-mean(vecteur)
  variance<-sum((vecteur-moyenne)**2)/length(vecteur)
  s<-sqrt(variance)/sqrt(length(vecteur)-1)
  q<- qt(1-alpha/2, df=length(vecteur)-1)
  paste("[",round(moyenne,2),"±",round(q*s,2),"]")
}

# Exemples
IC_BIS(echantillon_30[[1]],0.05)
IC_BIS(echantillon_60[[1]],0.05)
IC_BIS(echantillon_80[[1]],0.05)

#===================================================================================
# Q5 : Comparaison t.test
#===================================================================================
t.test(echantillon_30[[1]],mu=0) # Un test arbitraire ( mu=0 par exemple)
t.test(echantillon_60[[1]],mu=0)
t.test(echantillon_80[[1]],mu=0)

#===================================================================================
# Q6 : IC d'une proportion
#===================================================================================

# Methode 1 /  Formule du cours

IC_proportion<-function(numerateur,denominateur){
  proportion<-numerateur/denominateur
  borne<-qnorm(0.975)*sqrt(proportion*(1-proportion))/sqrt(denominateur)
  paste("[",round(proportion,3),"±",round(borne,3),"]")
}

IC_proportion(177,197)

# Methode 2 :  Binom.test
binom.test(177,197,p=0.5)
