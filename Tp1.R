brutToNet1 <- function(brut){
  if(is.numeric(brut)){ net <- brut - 0.22 * brut
  return(net)}
  else {
    return("ERROR: type not expected")}
}

brutToNet2 <- function(brut,cadres){
  if(is.numeric(brut)&cadres=="non-cadres"){net <- brut*0.78*0.925
  return(net)}
  if(is.numeric(brut)&cadres=="cadres"){ net <- brut*0.75*0.925
  return(net)}
  else {return("ERROR: type not expected")}
}

brutToNet3 <- function(brut, cadres, taux = 7.5, temps = 100){
  if(!is.numeric(brut) || !is.character(cadres) || temps < 0 || temps > 100 || taux< 0 || taux > 100)
    {return("ERROR: rate and time must be in range(0,100)")}
  if(cadres=="non-cadres"){ net_avant_impots <- brut*0.78}
  if(cadres=="cadres"){ net_avant_impots <- brut*0.75}
  net_après_impots<-net_avant_impots*(1-taux/100)*(temps/100)
  return(list(net_avant_impots,net_après_impots))
}