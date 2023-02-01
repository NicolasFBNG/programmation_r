brutToNet1 <- function(brut){
  if(is.numeric(brut)){ net <- brut - 0.22 * brut                    #Calcule directement le slaire mensuel net 
  return(net)}
  else {
    return("ERROR: type not expected")}                              #Si la valeur envoyée n'est pas numérique le code renvoie: "ERROR: type not expected"
}

brutToNet2 <- function(brut,cadre){
  if (!is.character(cadres))                                         #On vérifie si les valeurs entrées sont conformes
    {return( “ERROR : contract unknown”)
  if (!is.numeric(brut))
    {return( “ERROR : type not expected”)}
  if(cadre=="non-cadre"){net <- brut*0.78*0.925                      #On créer deux conditions pour faire des calcules différents en fonction de si on calcule pour un cadre ou non
  return(net)}
  if(cadre=="cadre"){ net <- brut*0.75*0.925
  return(net)}
}

brutToNet3 <- function(brut, cadre, taux = 7.5, temps = 100){
  if temps < 0 || temps > 100 || taux< 0 || taux > 100)              # On vérifie si toutes les valeurs entrées sont conformes
    {return("ERROR: rate and time must be in range(0,100)")}
  if (!is.character(cadres))
    {return( “ERROR : contract unknown”)
  if (!is.numeric(brut))
    {return( “ERROR : type not expected”)}
  if(cadre=="non-cadre"){ net_avant_impots <- brut*0.78}             #On calcule le salaire net avant les impots en fonctions de "cadres" ou "non-cadres"
  if(cadre=="cadre"){ net_avant_impots <- brut*0.75}
  net_après_impots<-net_avant_impots*(1-taux/100)*(temps/100)        #On calcule le salaire net après impots avec le salaires net avant les impots 
  return(list(net_avant_impots,net_après_impots))
}
