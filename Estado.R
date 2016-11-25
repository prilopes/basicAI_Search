Estado <- function(desc=NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", NULL, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Estado = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica print
print.Estado <- function(obj){
  cat(obj$desc, "\n")
  cat(obj$g, "\n")
  cat(obj$h, "\n")
}

## Criação do método genérico "heuristica"
heuristica <- function(atual, ...) {
  UseMethod("heuristica")
}

## Função padrão para o método genérico "heuristica"
## Deve ser implementada para o problema específico
heuristica.default <- function(atual, ...) {
  print("Funcao Generica. Defina a heuristica para o seu problema!\n")
  return(NULL)
}

## Criação do método genérico "geraFilhos"
geraFilhos <- function(obj) {
  UseMethod("geraFilhos")
}

geraFilhos.default <- function(obj) {
  print("Funcao Generica. Defina a geração de filhos para o seu problema!\n")
  return(NULL)
}