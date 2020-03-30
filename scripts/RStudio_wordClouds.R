# 
# https://gomesfellipe.github.io/post/2017-12-17-string/string/
# 
# Instalação dos pacotes
# 
install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm")
# 
# Carregando os pacotes
# 
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
# 
# Lendo o arquivo texto
# 
# O arquivo foi gravado a partir do arquivo de Movimentações Patrimoniais do projeto do TCC
# 
# write.table(mpi$descricaoBem, file = "./data/ml.txt", sep = "\t", row.names = FALSE, col.names = FALSE)
# 
# Lendo a partir de uma seleção do usuário
# 
# text <- readLines(file.choose())
# 
text <- readLines("C:/Users/Marcio/Dropbox (Pessoal)/TCC_PUCMinas/PUC_Minas/data/ml.txt")
# 
# Carregando os dados como o tipo de dados "corpus"
# 
# O pacote "corpus" não define um objeto "corpus" especial, mas define um novo tipo de dados, 
# corpus_text, para armazenar uma coleção de textos.
# 
docs <- Corpus(VectorSource(text))
#
# Definição do processo (inspect)
# 
# Inspecionar interativamente um modelo de ajuste sazonal. Inspecione interativamente um objeto. 
# O objetivo da inspeção é resumir todas as opções, gráficos e estatísticas relevantes que geralmente
#  devem ser consideradas.
# 
inspect(docs)
# 
# A transformação é realizada usando a função tm_map () para substituir, por exemplo, caracteres especiais do texto.
# 
# a função tm_map () é usada para remover espaços em branco desnecessários, converter o texto em letras minúsculas 
# e remover palavras-chave comuns como "o", "as" etc.
# 
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
# 
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, toSpace, "º")
# 
# Converte o texto para caixa baixa (minúsculas)
# 
docs <- tm_map(docs, content_transformer(tolower))
# 
# Remove os numeros
# 
docs <- tm_map(docs, removeNumbers)
# 
# Remove os "stopwords"
# 
docs <- tm_map(docs, removeWords, stopwords('portuguese'))
# 
# Remove os "stopwords" - Vetor especial
#
docs <- tm_map(docs, removeWords, c("trabalho", "angular", "seletiva", "courvin", "conjunto",
                                    "lugares", "microsoft", "paineis", "volante", "giratória",
                                    "braços", "vídeo", "video", "lcd", "espaldar", "aparelho",
                                    "giratória", "capacidae", "fixa", "intel", "tecido", "capacidade",
                                    "coleta", "tipo", "porta", "aço", "alto", "esmaltada", "médio",
                                    "lenovo", "inox", "média", "objeto", "suspensa", "giratoria",
                                    "laser", "couro", "windows", "tiny", "thinkcentre", "pro",
                                    "preto", "led", "axis", "pasta", "aprox", "medio", "gbps",
                                    "auditório", "modelo", "rba", "regulável", "regulavel",
                                    "reclinável", "apoio", "marca", "sfp", "madeira", "pés", "usb")) 
# 
# Remove pontuação
# 
docs <- tm_map(docs, removePunctuation)
#
# Elimina espaços em branco extras
# 
docs <- tm_map(docs, stripWhitespace)
#
# Cria uma matris de termos (documento de termos)
#
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# 
# head(d, 10)
# 
#                            word freq
# microcomputador microcomputador 4410
# poltrona               poltrona 3410
# notebook               notebook 3144
# mesa                       mesa 3011
# monitor                 monitor 2806
# trabalho               trabalho 2801
# portatil               portatil 2569
# elitebook             elitebook 2555
# vídeo                     vídeo 2492
# lixeira                 lixeira 2276
# 
# Gera a Nuvem de Palavras
# 
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
# 
# 
barplot(d[1:30,]$freq, las = 2, names.arg = d[1:30,]$word,
        col ="lightblue", main ="Palavras mais Frequentes",
        ylab = "Frequência das Palavras")
