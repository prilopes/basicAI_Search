source("Estado.R")

## Classe e métodos para o problema dos 3 Missionários e 3 Canibais
Canibais <- function(desc = NULL, pai = NULL){

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Canibais", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Canibais = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Canibais <- function(obj) {
  cat("(M C B): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Canibais <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = M + C + B
  return(sum(atual$desc))
}

geraFilhos.Canibais <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()

  desc <- obj$desc
  
  bAtual <- as.numeric(desc[3])
  
  bNovo <- as.numeric(bAtual != 1)
  
  ## gera filhos usando todos os operadores  
  if(bAtual == 1){
    
    operadores <- list(c(2,0,bAtual), c(0,2,bAtual), c(1,1,bAtual), c(1,0,bAtual), c(0,1,bAtual))
    
    filhosDesc <- lapply(operadores, function(op) desc-op)
    
  } else {
    
    operadores <- list(c(2,0,bNovo), c(0,2,bNovo), c(1,1,bNovo), c(1,0,bNovo), c(0,1,bNovo))
    
    filhosDesc <- lapply(operadores, function(op) desc+op)
  }
  
  ## verifica estados filhos incompatíveis com o problema  
  incompativeis <- sapply(1:length(filhosDesc),
                    function(i) {
                      fDesc <- filhosDesc[[i]]
                      if((fDesc['C'] > fDesc['M']) || ## Se #Canibais > #Missionários OU
                         (any(fDesc[1:2] > 3)) ||     ##    #Canibais ou #Missionários > 3 OU
                         (any(fDesc[1:2] < 0)))       ##    #Canibais ou #Missionarios < 0 então
                        i ## é incompatível: retorna índice
                      else
                        0 ## senão é compatível
                    })
  
  ## mantém no vetor apenas os que são incompatíveis
  incompativeis <- incompativeis[incompativeis != 0]
  
  ## remove estados filhos incompatíveis
  filhosDesc <- filhosDesc[-incompativeis]
  
  ## gera os objetos Canibais para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Canibais(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + 1
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}