debugSource("Canibais.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

inicial <- Canibais(desc = c(M = 3, C = 3, B = 1))

objetivo <- Canibais()
objetivo$desc <- c(M = 0, C = 0, B = 0)

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))
 
cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))