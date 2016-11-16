buscaEmLargura <- function(inicial, objetivo){
  
  abertos <- list(inicial)
  fechados <- list()

  while(length(abertos) != 0){
    atual <- abertos[[1]]
    abertos <- abertos[-1]
    if(atual == objetivo){
      return(recuperaCaminho(atual))
    }else {
      filhos <- geraFilhos(atual)
      fechados <- c(fechados, atual)
      filhos <- removeRepetidos(filhos, c(abertos, fechados))
      abertos <- c(abertos, filhos) ## filhos têm prioridade menor! 
    }
  }
  return("Busca falhou! :'-(")
}

buscaEmProfundidade <- function(inicial, objetivo){
  
  abertos <- list(inicial)
  fechados <- list()
  
  while(length(abertos) != 0){
    atual <- abertos[[1]]
    abertos <- abertos[-1]
    if(atual == objetivo){
      return(recuperaCaminho(atual))
    }else {
      filhos <- geraFilhos(atual)
      fechados <- c(fechados, atual)
      filhos <- removeRepetidos(filhos, c(abertos, fechados))
      abertos <- c(filhos, abertos) ## filhos têm prioridade maior!
    }
  }
  return("Busca falhou! :'-(")
}

buscaCustoUniforme <- function(inicial, objetivo){
  
  abertos <- list(inicial)
  fechados <- list()
  
  while(length(abertos) != 0){
    atual <- abertos[[1]]
    abertos <- abertos[-1]
    if(atual == objetivo){
      return(recuperaCaminho(atual))
    }else {
      filhos <- geraFilhos(atual)
      fechados <- c(fechados, atual)
      filhosNovos <- removeRepetidos(filhos, c(abertos, fechados)) 
      ## se gera filhos que aparecem em abertos, atualiza o valor de custo
      ## abertos devem estar ordenados por menor custo
      abertos <- ordenaPorCusto(c(atualizaAbertos(abertos, filhos),filhosNovos))
    }
  }
  return("Busca falhou! :'-(")
}

recuperaCaminho <- function(atual){
  
  caminho <- list()
  
  while(!is.null(atual)){
    caminho <- c(caminho, atual)
    atual <- atual$pai
  }
  
  return(rev(caminho))
}

removeRepetidos <- function(filhos, gerados){
  
  ## pega descrições dos estados já gerados
  gDesc <- lapply(gerados, function(gerado) gerado$desc)
  
  ## verifica se filhos não aparece em estados gerados
  fNovos <- sapply(filhos, function(filho) !is.element(list(filho$desc), gDesc))
  
  filhos <- filhos[fNovos]
  
  return(filhos)
}

atualizaAbertos <- function(abertos, filhos){
  
  ## pega descrições dos estados na lista de abertos
  aDesc <- lapply(abertos, function(aberto) aberto$desc)
  
  ## verifica se filhos aparece em estados da lista de abertos
  fEmAbertos <- sapply(filhos, function(filho) is.element(list(filho$desc), aDesc))
  
  if(any(fEmAbertos)){ ## se existe ao menos um filho que já aparece na lista de abertos
    
    ## recupera apenas os filhos que aparecem na lista de abertos
    filhosAbertos <- filhos[fEmAbertos]
    
    fADesc <- lapply(filhosAbertos, function(filhoAberto) filhoAberto$desc)
    
    selecionaAbertos <- sapply(abertos, function(aberto) is.element(list(aberto$desc),fADesc))
    
    atualizaAbertos <- abertos[selecionaAbertos]
    
    abertos <- abertos[!selecionaAbertos]
    
    atualizados <- list()
    
    for(filho in filhosAbertos){
      for(atualiza in atualizaAbertos)
        if((filho == atualiza) &&
           (filho$g < atualiza$g)){
          atualizados <- c(atualizados, list(filho))
        }else{
          atualizados <- c(atualizados, list(atualiza))
        }
        break
    }
    
    abertos <- c(atualizados, abertos)
  }
  
  
  return(abertos)
}

ordenaPorCusto <- function(abertos){
  
  aCusto <- t(as.data.frame(lapply(abertos, function(aberto) aberto$g)))
  
  rownames(aCusto) <- NULL
  
  ordemCusto <- order(aCusto)

  abertos <- lapply(ordemCusto, function(ordIndice) abertos[[ordIndice]])
  
  return(abertos)
}