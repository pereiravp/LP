# Laboratórios de Informática / Programação I

## Interpretador

Para abrir o interpretador do Haskell (GHCi) com o projeto carregado, utilize o comando `repl` do Cabal

```bash
cabal repl
```

## Testes e Feedback

A plataforma de feedback associada ao projecto fornece também a funcionalidade de correr testes unitários sobre as várias tarefas.
Para isso, o projeto contém vários executáveis, `t<N>-feedback`, em que `N` é o número da tarefa. Por exemplo, para a Tarefa 1 o executável é `t1-feedback`.

Pode correr cada executável, por exemplo para a Tarefa 1, pode utilizar o comando:

```bash
cabal run t1-feedback
```

## Coverage dos Testes

Ao executar os testes para uma tarefa, pode também medir a cobertura dos mesmos, ou seja, quanto do vosso código da tarefa é utilizado na resolução dos testes.
Para isso, devem seguir os seguintes passos, por exemplo para a Tarefa 1:

```bash
cabal clean
cabal run --enable-coverage t1-feedback
./runhpc.sh t1-feedback
```

Alternativamente, pode utilizar o comando seguinte script (usando a Tarefa 1 como exemplo):

```bash
./runcoverage.sh t1
```

## Qualidade do Código

Um dos eixos que devem ter em consideração no desenvolvimento do vosso projeto é a qualidade do código, que se prende com questões como a estrutura, modularidade, tamanho e documentação das funções, etc. Uma ferramenta que apoia na análise da qualidade do vosso código é o homplexity, que podem instalar com o seguinte comando:

```
cabal install homplexity --flags="html"
```

Para correr a ferramenta para o vosso projeto e gerar uma página web com um relatório podem executar o comando:

```
homplexity-cli --format=HTML lib/ > homplexity.html
```

## Documentação

A documentação do projeto pode ser gerada recorrendo ao [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock-project
```
