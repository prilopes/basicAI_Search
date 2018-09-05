# basicAI_Search
Basic AI search algorithms implemented in R.

## Contents

- `Estado.R` defines a general representation of a state in a AI search problem;
- `buscaDesinformada.R` implements Uninformed Search algorithms: breadth-first, depth-first and uniform cost)
- `buscaInformada.R` implements Informed Search algorithms: greedy and A*.

### Missionaries and Canibals

Script in `Canibais.R` implements `Estado.R` for the problem of the [Missionaries and Canibals](https://en.wikipedia.org/wiki/Missionaries_and_cannibals_problem), defining the generation of new states given a set of operators and the heuristica evaluation for this particular problem.

Script `exemploCanibais.R` instantiates a version of the Missionaries and Canibals (3 missionaries, 3 canibals and 1 boat, all starting on the left margin of the river) and runs all the available searches.

#### Initialization

Defining initial and objective nodes:

```{r}
initial <- Canibais(desc = c(M = 3, C = 3, B = 1))

objective <- Canibais()
objective$desc <- c(M = 0, C = 0, B = 0)

```

#### Uninformed Search

Executing a breadth-first search is as simple as calling the `buscaEmLargura(initial, objective)` function, which returns the path obtained from the initial node to the objective node as a list object:

```{r}
buscaEmLargura(initial, objective)
```

The other available searches work similarly.

#### Informed Search

For the informed search, best-first is the executed algorithm. Besides the initial and objective nodes, the function expects a parameter indicating the approach to be used in the search: "Greedy" must be passed for the greedy search and "AEstrela" for the A* search. If no value is passed, the function uses the defined default of "AEstrela".

```{r}
buscaBestFirst(inicial, objetivo, "Greedy")
```
***

**Obs**: It is worth noting that the problem of 3 Missionaries and 3 Canibals has a small and simple search tree, so the path returned for all serach algorithms is the same.
