#'----------------------------------------------------------------------------------------------
#' PROJETO: Aula NLP - Ex2
#'
#' OBJETIVO: Fundamentos de processamento de texto
#'
#' PRODUTO: 
#'
#' AUTOR: Araê Cainã
#'
#' DATA DE CRIACAO: 11/04/2022
#'
#' DATA DE MODIFICACAO: 
#'
#' MODIFICACOES: 
#'----------------------------------------------------------------------------------------------


# Pacotes -----------------------------------------------------------------

library(readxl)
library(sjmisc)
library(tidyverse)
library(quanteda)


# Base de dados -----------------------------------------------------------

##' Lê a base de dados
usos <- readxl::read_excel(path = 'data/usos.xlsx')



# Explorando dados --------------------------------------------------------

##' Você pode usar o comando `names()` para saber quais colunas existem em um dataframe
names(usos)

##' Para criar uma tabela descritiva de uma coluna, use a função `freq()` do pacote sjmisc
sjmisc::frq(usos$resp_num)

##' Para descobrir quantos sujeitos existem, basta descobrir
##' quantos códigos únicos existem na base de dados. Depois,
##' use a função length para saber o tamanho do vetor

length(unique(usos$ID))

##' Para saber a distribuição de uma variável numérica, você pode
##' criar um histograma, com a função hist()
hist(usos$resp_num)

##' Você também pode usar o pacote `ggplot2` para criar
##' um histograma mais bonito

ggplot2::ggplot(
  usos,
  ggplot2::aes(
    x = resp_num # qual informação vai no eixo x
  )
) + 
  ggplot2::geom_histogram(
    color = 'white', # mudança de cor no histograma
    binwidth = 2     # agrupamento de valores no eixo x
  )


# Criando uma tabela somente com as ideais e a avaliação ------------------

##' O uso do pipe ` %>% ` é útil no R. Você pode realizar várias 
##' análises em ordem, sem ter que criar novos objetos
usos %>% 
  dplyr::filter(avaliacao > 3 ) %>%      # filtra pela coluna `avaliacao`
  dplyr::select(resposta, avaliacao) %>% # seleciona somente as colunas `resposta` e `avaliacao`
  dplyr::arrange(dplyr::desc(resposta))  # organiza por ordem descendente


# Transformação dos dados -------------------------------------------------


## Tokenização ----
###' Tokenizaar é o processo de separar todas as palavras em observação únicas.
###' Na prática, uma frase como "Meu nome é Araê" viraria um vetor com 4 elementos:
###' "Meu", "nome", "é", "Araê".
###' Com isso é possível criar uma document term matrix, que é uma matriz
###' cujas colunas são as palavras possíveis e as linhas os indivíduos. Cada
###' casela será preenchida com a quantidade de vezes que aparece naquele indivíduo

###' Use a função abaixo para tokenizar a coluna de respostas
###' 
tokenized_corpus <- quanteda::tokens(
  usos$resposta,      # Coluna com as respostas
  remove_punct = TRUE # Remove as pontuações
)

###'  OBS:
###' Para retirar a lista de palavras basta carregar a lista de palavras
###' e pedir para remover com a função `tokens_remove`
stopwords <- read_csv(
  'http://www.labape.com.br/rprimi/ds/stopwords.txt',
  col_names = FALSE
)
names(stopwords) <- 'word'

tokenized_corpus <-  quanteda::tokens_remove(
    tokenized_corpus,  # Corpus tokenizado
    stopwords$word     # Palavras a remover
  )

###' Para criar uma dfm, onde cada coluna é uma palavra e cada linha é um texto,
###' use a função abaixo

data_matrix <- quanteda::dfm(tokenized_corpus)

###' Para retirar as palavras que aparecem poucas vezes, use a função `dfm_trim`
data_matrix <- quanteda::dfm_trim(
  data_matrix,
  min_termfreq = 2,
  min_docfreq = 2
)


###' Para saber a frequência de cada termo, use a função `featfreq()` do quanteda
quanteda::featfreq(data_matrix)

###' Para saber os termos mais frequentes, use a função `topfeatures()`
quanteda::topfeatures(data_matrix, 23)

###' Para criar o wordcloud, use a função `textplot_wordcloud`
quanteda.textplots::textplot_wordcloud(data_matrix)



