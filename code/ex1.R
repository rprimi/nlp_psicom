#'----------------------------------------------------------------------------------------------
#' PROJETO: Aula NLP - Ex1
#'
#' OBJETIVO: 
#'
#' PRODUTO: 
#'
#' AUTOR: Araê Cainã
#'
#' DATA DE CRIACAO: 21/02/2022
#'
#' DATA DE MODIFICACAO: 
#'
#' MODIFICACOES: 
#'----------------------------------------------------------------------------------------------


# Pacotes -----------------------------------------------------------------

library(readxl) # lê arquivos em excel
library(tidyverse) # habilita uma série de pacotes que serão utilizados
library(corrr) # pacote com opção para correlação
library(psych) # análises psicométricas
library(irlba) # faz o svd

# Abrindo base de dados ---------------------------------------------------

df <- readxl::read_excel("data/resp_footprintxlsx.xlsx")

# Análise fatorial e de componentes principais ----------------------------

## Análise de correlação ----

### Método 1 ----
# Faz a correlação somente com as colunas dos sites visitados
r <- cor(df[,2:7])

# Imprime no console os resultados com duas casas decimais
round(r, digits = 2)

# Cria o mapa de calor da correlação
heatmap(r)

### Método 2 ----

# Calcula a correlação
r2 <- corrr::correlate(df[,2:7])

# Imprime a tabela de correlação
corrr::fashion(r2)

# Cria um scatterplot
corrr::rplot(r2)

# Gera um gráfico de redes da correlação
corrr::network_plot(r2)

## PCA ----

# roda as Análises de Componentes Principais (PCA)
pc <- psych::principal(
  df[,2:7],     # base de dados
  nfactors = 3, #  cria 3 fatores
  scores = TRUE # gera os scores
)

# Imprime os resultados
psych::print.psych(pc, digits = 2, cut = 0.3)

# Cria um plot com os autovalores
plot(pc$values)

## PZC ----

# Matrix com autovalores
eigens <- eigen(r)

# multiplicação dos autovetores pelos quadrados da diagonais dos autovalores
loadings <- eigens$vectors %*% sqrt(diag(eigens$values))

# reproduz a matrix dos loadings da PCA
loadings %*% t(loadings)

## SVD (Singular Value Decomposition) ----

# Roda a SVD
r_svd <- svd(as.matrix(df[,2:7]), nv = 3)

# Variáveis
u <- r_svd$u

# Pessoas
v <- r_svd$v

# Valores
s <- diag(r_svd$d)

# Gera o gráfico com os autovalores
plot(r_svd$d)

# reproduz a matriz
u[,1:3] %*%  # escores fatoriais dos sujeitos nos três primeiros componentes
  s[1:3, 1:3] %*% # autovalores
  t(v) # cargas fatoriais das variáveis nos componentes


