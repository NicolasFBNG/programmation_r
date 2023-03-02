#Exercice 1 Simulation d’une population

#La taille moyenne des français est de 171cm avec un écart-type de 9 centimètres.

#1. produire les tailles d’une population simulée de 10.000.000 de français répartis suivant une loi normale
#de moyenne 171 et d’écart-type 9. Stocker ces tailles dans un vecteur “population”

set.seed(18654)
population<-rnorm(n=10000000, mean=171, sd=9)

#2. calculer la moyenne et l’écart-type de la population. Retrouvez-vous les valeurs attendues?
  
mean(population)
sd(population)

#On retrouve des valeurs très proches des valeurs attendue (moyenne de 171 et écart-type de 9)

#3. établir l’histogramme de la taille. Retrouvez-vous la forme bien connues?

hist(population, main="Distribution de la taille des français selon une loi normale", probability=TRUE)
 
#On retrouve un graphique très proche de ce à quoi l'on peut s'attendre d'une loi normale

#4. Combien de personnes ont une taille supérieure à 190cm? Combien devriez-vous en trouver théoriquement?

sum(population>=190)
(1-pnorm(q=190, mean=171, sd=9))*10000000
  
#5. Combien de personnes ont une taille inférieure à 144cm? Combien devriez-vous en trouver théoriquement?
  
sum(population<=140)
pnorm(q=144, mean=171, sd=9)*10000000


#Exercice 2: Simulation d’échantillons:

#On va essayer d’estimer la taille moyenne de la population à partir d’un échantillon.

#1. Tirez un échantillon de taille 100 dans la population initiale, à l’aide de la fonction sample. Quelle
#est la taille moyenne dans l’échantillon? Quelle est l’écart-type dans l’échantillon? Ces deux valeurs
#sont-elles proches de celles de la population?

échentillon<-sample(x=population, size=100, replace=TRUE)
mean(échentillon)
sd(échentillon)

#Ces valeurs sont proches de celles de la population

#2. à partir de l’écart-type estimé, calculez la largeur du demi-intervalle de confiance, puis les bornes
#inférieures et supérieures de l’intervalle de confiance (toujours à 95%)

demi_intervalle <-qnorm(p = 1-(0.05/2))
amplitude<- demi_intervalle*sd(échentillon)/sqrt(length(echantillion))
intervalle- <- moyenneechantillion - amplitude
intervalle+ <- moyenneechantillion + amplitude


#3. A l’aide de la fonction replicate, tirez 1000 échantillons de taille 100. Stockez dans un dataframe la
#moyenne et l’écart-type de chaque échantillon (la fonction apply peut être utile).
 
échentillons <- replicate(1000, sample(x = population, size = 100, replace = TRUE))
moyenne_échentillons <- apply(X=échentillons, MARGIN = 2, FUN = mean)
ecartype_échentillons <- apply(X=tableau, MARGIN = 2, FUN = sd)
data_frame <- rbind(moyenne_échentillons,ecartype_échentillons)

#4. tracez l’histogramme des moyennes des échantillons. Retrouve-t-on une forme connue?
 
  hist(x = data_frame[1,], main = " des moyenne des echantillions") 
  
#5. Calculez la moyenne des moyennes des échantillons, ainsi que l’écart-type des moyennes des échantillons.
#Normalement la moyenne des moyennes doit être (à peu près) égale à la moyenne de la population : on
#dit que la moyenne est un estimateur non biaisé. De même l’écart-type des moyennes des échantillons
#doit être (à peu près) égal à 0,9 c’est-à-dire σ/√
#n.
  
  moyenne_des_moyennes <- mean(data_frame[1, ])
  ecartype_des_moyenne <- sd(data_frame[1,])

  
  
  
#6. Combien d’échantillons ont une moyenne supérieure à 172,8cm? Quelle est le nombre théorique?
  
length(data_frame[1, ][data_frame[1, ]>172.8])
  
  
#7. Pour chaque échantillon, calculez la largeur du demi-intervalle de confiance en utilisant l’estimation
de l’écart-type calculée pour chaque échantillon, puis calculez les bornes inférieures et supérieures des
intervalles de confiances (variables à rajouter dans votre dataframe).





#8. Tracez les intervalles de confiance sur un graphe en utilisant la fonction plotCI du package gplots. Pour
combien d’échantillons la vraie moyenne de la population (171cm) est-elle en dehors de l’intervalle de
confiance?
  





