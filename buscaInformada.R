buscaBestFirst <- function(inicial, objetivo, abordagem = "AEstrela"){
  
  abertos <- atualizaAvaliacao(list(inicial), abordagem)
  fechados <- list()
  
  while(length(abertos) != 0){
    atual <- abertos[[1]]
    abertos <- abertos[-1]
    if(atual == objetivo){
      return(recuperaCaminho(atual))
    }else {
      filhos <- geraFilhos(atual)
      filhos <- atualizaAvaliacao(filhos, abordagem)
      filhosNovos <- novosGerados(filhos, c(abertos, fechados)) ## gerados pela primeira vez
      filhosAbertos <- geradosAntes(filhos, abertos) ## filhos que já aparecem na lista de abertos
      abertos <- c(filhosNovos, filhosAbertos[[1]])
      if(length(fechados) > 0){
        filhosFechados <- geradosAntes(filhos, fechados, lista = "fechados") ## filhos que já aparecem na lista de fechados
        fechados <- filhosFechados[[1]]
        ## atualização da lista abertos: 
        ##      filhos gerados pela 1a vez + 
        ##      já gerados e atualizados na lista de abertos +
        ##      já gerados e fechados, mas reabertos
        abertos <- c(abertos, filhosFechados[[2]])
      }
      abertos <- ordenaPorAvaliacao(abertos)
      ## inclui estado atual na lista de fechados
      fechados <- c(fechados, atual)
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

atualizaAvaliacao <- function(estados, abordagem = "AEstrela"){

  switch(abordagem,
          Gulosa = estados <- lapply(estados,
                                     function(estado) {
                                       estado$f <- estado$h
                                       estado
                                      }),
          
         AEstrela = estados <- lapply(estados,
                                      function(estado) {
                                        estado$f <- estado$g + estado$h
                                        estado
                                      })
  )
  
  return(estados)
}

novosGerados <- function(filhos, gerados){
  
  ## pega descrições dos estados já gerados
  gDesc <- lapply(gerados, function(gerado) gerado$desc)
  
  ## verifica se filhos não aparece em estados gerados
  fNovos <- sapply(filhos, function(filho) !is.element(list(filho$desc), gDesc))
  
  filhos <- filhos[fNovos]
  
  return(filhos)
}

geradosAntes <- function(filhos, gerados, lista = "abertos"){
  
  ## pega descrições dos estados na lista de gerados
  gDesc <- lapply(gerados, function(gerado) gerado$desc)
  
  ## verifica filhos que aparecem na lista de gerados
  fGerados <- sapply(filhos, function(filho) is.element(list(filho$desc), gDesc))
  
  res <- list(gerados, list())
  
  if(any(fGerados)){ ## se existe ao menos um filho que já aparece na lista de abertos
    
    ## recupera apenas os filhos que aparecem na lista de gerados
    filhosGerados <- filhos[fGerados]
    
    ## pega descrição de filhos gerados
    fGDesc <- lapply(filhosGerados, function(filhoGerado) filhoGerado$desc)
    
    ## pega na lista de gerados os estados correspondentes aos filhos
    novamenteG <- sapply(gerados, function(gerado) is.element(list(gerado$desc),fGDesc))
    
    ## lista com estados gerados novamente - precisa atualizar com relação à f(n)
    atualizaGerados <- gerados[novamenteG]
    
    ## lista com estados que não foram gerados novamente - são mantidos como estão
    gerados <- gerados[!novamenteG]
    
    atualizados <- list()
    
    reabertos <- list()
    
    for(filho in filhosGerados){
      for(atualiza in atualizaGerados)
        if((filho == atualiza) &&
           (filho$f < atualiza$f)){ ## se a f(filho) < f(atualiza)
          if(lista == "abertos") ## se é a lista de abertos sendo verificada, então
                                 ## trocamos filho pro atualiza 
            atualizados <- c(atualizados, list(filho))
          else{ ## senão (é a lista de fechados sendo verificada) o filho deve ir
                ## para a lista de abertos (reabertos)
            reabertos <- c(reabertos, list(filho))
          }
        }else{ 
          atualizados <- c(atualizados, list(atualiza))
        }
        break
    }
    
    gerados <- c(gerados,atualizados)
    res <- list(gerados, reabertos)
  }
  
  return(res)
}

ordenaPorAvaliacao <- function(abertos){
  
  aAvaliacao <- t(as.data.frame(lapply(abertos, function(aberto) aberto$f)))
  
  rownames(aAvaliacao) <- NULL
  
  ordemCusto <- order(aAvaliacao)

  abertos <- lapply(ordemCusto, function(ordIndice) abertos[[ordIndice]])
  
  return(abertos)
}