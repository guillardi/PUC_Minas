
write_excel_csv2(mpi, "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT.csv", na = "NA", append = FALSE, delim = ";", quote_escape = "double")

#
# https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/
# 

#
# TreeMap (quadrados com totais por ano)
#
install.packages("treemap")

# library
library(treemap)

# Build Dataset
group <- c(rep("group-1",4),rep("group-2",2),rep("group-3",3))
subgroup <- paste("subgroup" , c(1,2,3,4,1,2,1,2,3), sep="-")
value <- c(13,5,22,12,11,7,3,1,23)
data <- data.frame(group,subgroup,value)
# 
# treemap
# 
treemap(data,
        index=c("group","subgroup"),
        vSize="value",
        type="index"
) 
# 
# treemap
#
mpiSumarizadoGestores <- mpi %>% 
  group_by(responsavelCadastro, ano = year(dataMovimentacao), mes = month(dataMovimentacao, label = TRUE)) %>% 
  tally(name = "movimentacoes")
# 
treemap(mpiSumarizadoGestores,
        index = c("responsavelCadastro","ano", "mes"),
        vSize = "movimentacoes",
        type  = "index"
) 




x <- c("apple", "banana", "pear")
str_view(x, "(an)+")
str_view(x, ".a.")

# And this tells R to look for an explicit .
str_view(c("abc", "a.c", "bef"), "a\\.c")

x <- c("apple", "banana", "pear")
str_view(x, "^a")
str_view(x, "a$")


x <- c("apple pie", "apple", "apple cake")
str_view(x, "apple")

str_view(x, "^apple$")

str_view(c("abc", "a.c", "a*c", "a c"), "a[.]c")

str_view(c("abc", "a.c", "a*c", "a c"), ".[*]c")

str_view(c("abc", "a.c", "a*c", "b*c", "a c"), ".[*]c")

str_view(c("abc", "a.c", "a*c", "a c"), "a[ ]")

str_view(c("grey", "gray"), "gr(e|a)y")

x <- "1888 is the longest year in Roman numerals: MDCCCLXXXVIII"
str_view(x, "CC?")

str_view(x, "CC+")

str_view(x, 'C[LXV]+')

str_view(fruit, "(..)\\1", match = TRUE)

str_view(fruit, "(.)(.)\\2\\1", match = TRUE)

# How many common words start with t?
sum(str_detect(words, "^t"))
#> [1] 65

# What proportion of common words end with a vowel?
mean(str_detect(words, "[aeiou]$"))
#> [1] 0.277


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#
# http://www.datasciencemadesimple.com/groupby-mean-in-r-2/
# Groupby mean of single column
# 
# http://www.sthda.com/english/wiki/amazing-interactive-3d-scatter-plots-r-software-and-data-visualization
# GGPLOT
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
aggregate(df1$Sales, by=list(df1$State), FUN=mean)




#
# Percentual das movimentações de bens (contagem de bens movimentados)
#
paste(format(100*mean(str_detect(mpi$responsavelCadastro, "(Guillardi)")), digits=2, nsmall=2), "%", sep = "")
#
sprintf("Percentual de bens movimentados: %3.2f%%", 100 * mean(str_detect(mpi$responsavelCadastro, "(Guillardi)")))
#
sprintf("Percentual de bens movimentados: %3.2f%%", 100 * mean(str_detect(mpi[which(year(mpi$dataMovimentacao) > 2016),]$responsavelCadastro, "(Guillardi)")))


mpi[which(year(mpi$dataMovimentacao) == 2017),]$responsavelCadastro
#
# [1] 1540
#

mpi_teste <- mpi %>% group_by(responsavelCadastro, ano = as.factor(year(dataMovimentacao))) %>%
  tally(name = "bensMovimentados") %>% 
  mutate(n = mean(str_detect(mpi[which(year(mpi$dataMovimentacao) == ano),]$responsavelCadastro, responsavelCadastro)))

rowsum(mpi_teste$bensMovimentados, mpi_teste$ano)[[1]]

rowsum(mpi_teste$bensMovimentados, mpi_teste$ano)

match(mpi_teste$ano,c("2017",'2018','2019','2020'))

match("2019",c("2017",'2018','2019','2020'))



#
# Data Frame
# 
aggregate(bensMovimentados~ano, mpi_teste, sum)[[1]][3]
aggregate(bensMovimentados~ano, mpi_teste, sum)[[1]]

x[x$ano == '2017']

dimnames(x)


dimnames(rowsum(mpi_teste$bensMovimentados, mpi_teste$ano))[[1]][1]

mpi_teste[which(mpi_teste$ano == "2017"),]




sum(mpi_teste$bensMovimentados)

str(mpi_teste)

mpi[which(mpi$nivel3 == TRUE),]$nivel2 <- NA_character_

xsum <- rowsum(mpi_teste$bensMovimentados, mpi_teste$ano)

dimnames(xsum)[[1]][2]


mpi_teste <- mutate(mpi_teste, mpiFunction(responsavelCadastro, ano) )



mpiFunction <- function(mpiResp, mpiAno) {
  #
  nTotal <- mpi %>% filter(year(mpi$dataMovimentacao) == mpiAno) %>% tally()
  #
  nResp <- mpi %>% filter(year(mpi$dataMovimentacao) == mpiAno, mpi$responsavelCadastro == mpiResp) %>% tally()
  #
  return(nResp/nTotal*100)
}
  
addPercent <- function(x){
    percent <- round(x * 100, digits = 1)
    result <- paste(percent, "%", sep = "")
 return(result)
}

#

stuff <- c("bread", "cookies")

price <- c(2.1, 4)

sprintf("%s costed $ %3.2f ", stuff, price)[1]

#
df %>% 
  mutate(
    vowels = str_count(word, "[aeiou]"),
    consonants = str_count(word, "[^aeiou]"))
#
#

fruit <- c("apple", "banana", "pear", "pinapple")
str_detect(fruit, "a")

#

data(package='tseries')
help(USAccDeaths)
USAccDeaths  

# Agregação Temporal
# 
# Pode-se mudar a frequencia de uma serie temporal com a função aggregate. Por exemplo, podemos agregar dados mensais em somas trimestrais ou médias anuais.
# 
# x=USAccDeaths
# 
# aggregate(x,nfrequency=4,FUN=sum) # somas trimestrais
# 
# Qtr1  Qtr2  Qtr3  Qtr4
# 1973 26041 29980 31774 28026
# 1974 22769 26648 28686 26519
# 1975 23592 26813 27998 24660
# 1976 22945 25493 27294 25009
# 1977 22475 26295 28241 25911
# 1978 22519 26741 29421 26943
# 
# aggregate(x,nfreq=1,FUN=mean) # medias anuais
# 
# Time Series:
#   Start = 1973 
# End = 1978 
# Frequency = 1 
# [1] 9651.750 8718.500 8588.583 8395.083 8576.833 8802.000





library(xlsx)
library(tseries)
library(gdata)


z = read.xlsx(".\\data-raw\\atmosfera.xls",1)
dim(z)

z[1:10,]


x = as.ts(z[,2:3])

x = as.ts(USAccDeaths)


# what type of plot should be drawn. Possible types are
# 
# "p" for points,
# "l" for lines,
# "b" for both,
# "c" for the lines part alone of "b",
# "o" for both ‘overplotted’,
# "h" for ‘histogram’ like (or ‘high-density’) vertical lines,
# "s" for stair steps,
# "S" for other steps, see ‘Details’ below,
# "n" for no plotting.

# 
# Jan   Feb   Mar   Apr   May   Jun   Jul   Aug   Sep   Oct   Nov   Dec
# 1960  6550  8728 12026 14395 14587 13791  9498  8251  7049  9545  9364  8456
# 1961  7237  9374 11837 13784 15926 13821 11143  7975  7610 10015 12759  8816
# 1962 10677 10947 15200 17010 20900 16205 12143  8997  5568 11474 12256 10583
# 1963 10862 10965 14405 20379 20128 17816 12268  8642  7962 13932 15936 12628
# 1964 12267 12470 18944 21259 22015 18581 15175 10306 10792 14752 13754 11738
# 1965 12181 12965 19990 23125 23541 21247 15189 14767 10895 17130 17697 16611
# 1966 12674 12760 20249 22135 20677 19933 15388 15113 13401 16135 17562 14720
# 1967 12225 11608 20985 19692 24081 22114 14220 13434 13598 17187 16119 13713
# 1968 13210 14251 20139 21725 26099 21084 18024 16722 14385 21342 17180 14577
# 
# 

# 
# Alguns códigos... testes... etc
#
sprintf_ <- sprintf("%s teste de concatenação de %s coisas", "Um", "muitas")
# 
x <- "ED. CNC, 08º ANDAR"

str_view(x, "^(ED\\. CNC\\, )")

gsub("^(ED\\. CNC\\, )", "CNC, \\2", x)
# 
# [1] "CNC, 08º ANDAR"
# 
fruit <- c("banana", "coconut", "cucumber", "jujube", "papaya", "sala berry")

str_view(fruit, "(..)\\1", match = TRUE)

responsavel = "Marcio Guillardi da Silva" 

str_view(responsavel, "([:upper:]?)\\1", match = TRUE)

str_view(responsavel, "([[:upper:]]?)([[:lower:]])\\1" , match = TRUE)

# 
x <- "SALA 604B1 RECEPÇÃO DA TI"
gsub("(^SALA\\s{1})([^\\Nº])","SL. Nº \\2", x)
# 
# [1] "SL. Nº 604B1 RECEPÇÃO DA TI"
# 
x <- "SALA Nº 801"
gsub("(^SALA\\s{1})([\\Nº])","SL. \\2", x)
# 
# [1] "SL. Nº 801"
# 
x <- "SL. 904A"
gsub("^(SL\\.)([[:space:]])([^\\Nº])","SL. Nº\\2\\3", x)
# 
# [1] "SL. Nº 904A"
# 
# 
mpi[1,]                                       # Retorna a linha correspondente (da sequencia)
mpi[[4]][mpi$id == "31892"]                   # Retorna todos os registros encontrados no dataframe (df[[coluna]][id="busca"])
mpi[match("31892",mpi$id),]                   # Retorna o primeiro registro encontrado correspondente ao id informado
mpi$tombamento[mpi$id == "31892"]             # Retorna uma lista com o número do tombamento dos registros correspondentes
mpi$id == "31892"                             # Retorna uma lista de TRUE ou FALSE para o registro encontrado ou não
which(mpi$id == "31892")                      # Retorna o número da linha que contém o registro encontrado / id Movimentação = 31892
#
str_extract("SL.904A", "SL\\.[:digit:]{1}")   # [1] "SL.9"  : Apenas com o primeiro dígito numérico
str_extract("SL.904A", "SL\\.[:digit:]+")     # [1] "SL.904": Todos os dígitos (núméricos)
str_extract("SL.904A", "SL\\.[:alnum:]+")     # [1] "SL.904": Todos os dígitos (núméricos) e alfabéticos (letras)
str_extract("SL.904A", "SL\\.[0-9]+")
str_extract("SL.904A", "^(SL.)([^[:space:]])([[:alnum:]]+)")
# 
str_locate("SL.904A", "SL\\.[0-9]+")
#      start end
# [1,]     1   6
#
# 
# Utilizar essas funções para substituição de caracteres
# 
x = "PleaseAddSpacesBetweenTheseWords"
gsub("([a-z])([A-Z])", "\\1 \\2", x)
# 
# [1] "Please Add Spaces Between These Words"
# 
# 
x <- "<dd>Found on January 1, 2007</dd>"
gsub("<dd>[F|f]ound on |</dd>", "", x)
# 
# [1] "January 1, 2007"
# 
# 
hw <- "Hadley Wickham"
# 
str_sub(hw, 1, 6)
str_sub(hw, end = 6)
str_sub(hw, 8, 14)
str_sub(hw, 8)
str_sub(hw, c(1, 8), c(6, 14))
# 
gregexpr(pattern = ",", "ED. CNC, ANDAR TERREO, PORTARIA CNC", fixed = TRUE)[[1]]
#
# [1]  8 22
# attr(,"match.length")
# [1] 1 1
# attr(,"index.type")
# [1] "chars"
# attr(,"useBytes")
# [1] TRUE
#
# 
gregexpr(pattern = ",", "ED. CNC, ANDAR TERREO, PORTARIA CNC", fixed = TRUE)[[1]][2] 
# 
# [1] 22
# 
# 
str_sub("ED. CNC, 06º ANDAR, COPA 06", gregexpr(pattern = ",", "ED. CNC, 06º ANDAR, COPA 06", fixed = TRUE)[[1]][2]+2, str_length("ED. CNC, 06º ANDAR, COPA 06"))
#
# [1] "COPA 06"
# 
# 
x <-  gregexpr(pattern ='2',"thequickbrownfoxeswe2retired")
# 
if_else(x[[1]][1] > 0, "Encontrado", "Não Encontrado")
# 
# 
# 
nIdades <- data.frame(idades = c(20,25,30,35,40,60,50,70))
#
sIdades <- cut(nIdades$idades, c(0,30,50,100), labels = c("Jovem", "Adulto", "Idoso"))# 
#
# Exemplo com imdf (filmes)
# 
imdb %>%
  mutate(
    lucro = receita - orcamento,
    lucro = ifelse(lucro <= 0, "Não", "Sim")
  ) %>% 
  filter(!is.na(lucro)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = orcamento, y = receita, color = lucro))
# 
# 
# ****************************************************************************************************************************
# 
# Geoms%>%
# 
# Os geoms definem qual forma geométrica será utilizada para a visualização das observações. Como já vimos, a função geom_point() gera gráficos de dispersão transformando pares (x,y) em pontos. Veja a seguir outros geoms bastante utilizados:
# 
# geom_line - para linhas definidas por pares (x,y).
# geom_abline - para retas definidas por um intercepto e uma inclinação.
# geom_hline - para retas horizontais.
# geom_bar - para barras.
# geom_histogram - para histogramas.
# geom_boxplot - para boxplots.
# geom_density - para densidades.
# geom_area - para áreas.
# 
# ****************************************************************************************************************************
# 
# Um padrão para os gráficos
# 
# Você deve ter percebido que, para fazer um gráfico usando `ggplot2` e a gramática dos gráficos, existe um padrão:
#   
#   ggplot(data = DATA) + GEOM_FUNCTION(mapping = aes(MAPPINGS))
# 
# Para fazer um gráfico, basta substituir DATA por um banco de dados, GEOM_FUNCTION por uma função geométrica e MAPPINGS 
# por uma coleção de mapas estéticos. Isso será muito útil quando você for fazer o seu próprio gráfico.
# 
# ****************************************************************************************************************************
# 
# Há várias funções auxiliares que você pode usar em select():
# 
# starts_with("abc"): corresponde aos nomes que começam com "abc".
# 
# ends_with("xyz"): corresponde aos nomes que terminam com "xyz".
# 
# contains("ijk"): corresponde aos nomes que contêm "ijk".
# 
# matches("(.)\\1"): seleciona variáveis ??que correspondem a uma expressão regular. Este corresponde a qualquer variável que 
# contenha caracteres repetidos. Você aprenderá mais sobre expressões regulares em strings .
#
# num_range("x", 1:3): Corresponde x1, x2e x3
# 
# 
# forcats::fct_explicit_na(religion))
# detach(package:tidyr)
# install.packages(c("nycflights13", "gapminder", "Lahman", "lubridate"))

# 
# Comandos GIT para gravar os dados no Github
# 
# git add -A
# git commit -m "A commit from my local computer"
# git push
# 
# https://help.github.com/en/github/using-git/renaming-a-remote
#
# 
# Regular Expression Syntax:
#   
# Syntax  - Description
# 
# \\d       Digit, 0,1,2 ... 9
# \\D       Not Digit
# \\s       Space
# \\S       Not Space
# \\w       Word
# \\W       Not Word
# \\t       Tab
# \\n       New line
# ^         Beginning of the string
# $         End of the string
# \         Escape special characters, e.g. \\ is "\", \+ is "+"
# |         Alternation match. e.g. /(e|d)n/ matches "en" and "dn"
# •         Any character, except \n or line terminator
# [ab]      a or b
# [^ab]     Any character except a and b
# [0-9]     All Digit
# [A-Z]     All uppercase A to Z letters
# [a-z]     All lowercase a to z letters
# [A-z]     All Uppercase and lowercase a to z letters
# i+        i at least one time
# i*        i zero or more times
# i?        i zero or 1 time
# i{n}      i occurs n times in sequence
# i{n1,n2}  i occurs n1 - n2 times in sequence
# i{n1,n2}? non greedy match, see above example
# i{n,}     i occures >= n times
# [:alnum:] Alphanumeric characters: [:alpha:] and [:digit:]
# [:alpha:] Alphabetic characters: [:lower:] and [:upper:]
# [:blank:] Blank characters: e.g. space, tab
# [:cntrl:] Control characters
# [:digit:] Digits: 0 1 2 3 4 5 6 7 8 9
# [:graph:] Graphical characters: [:alnum:] and [:punct:]
# [:lower:] Lower-case letters in the current locale
# [:print:] Printable characters: [:alnum:], [:punct:] and space
# [:punct:] Punctuation character: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~
# [:space:] Space characters: tab, newline, vertical tab, form feed, carriage return, space
# [:upper:] Upper-case letters in the current locale
# [:xdigit:]  Hexadecimal digits: 0 1 2 3 4 5 6 7 8 9 A B C D E F a b c d e f
# 
# 
# For completeness, write_csv() from the readr package is faster and never writes row names
# 
# install.packages('readr', dependencies = TRUE)
# library(readr)
# write_csv(t, "t.csv")
# 
# If you need to write big data out, use fwrite() from the data.table package. It's much faster than both write.csv and write_csv
# 
# install.packages('data.table')
# library(data.table)
# fwrite(t, "t.csv")
# 
# Below is a benchmark that Edouard published on his site
# 
# microbenchmark(write.csv(data, "baseR_file.csv", row.names = F),
#                write_csv(data, "readr_file.csv"),
#                fwrite(data, "datatable_file.csv"),
#                times = 10, unit = "s")
# 
# by(dataFrame, 1:nrow(dataFrame), function(row) dostuff)
# 
# library("gtools")
# asc("°")
# asc("º")
# chr(186)
# 
# ************** MAIS SOBRE R **********************
#   
# https://bookdown.org/rdpeng/rprogdatascience/history-and-overview-of-r.html#back-to-r
# 
# https://www.cloudera.com/tutorials/advanced-analytics-with-sparkr-in-rstudio.html
# 
# http://www.datasciencemadesimple.com/
# 
# Séries Temporais
# 
# https://www.tutorialspoint.com/r/r_time_series_analysis.htm
# 
# # https://bookdown.org/rdpeng/rprogdatascience/r-nuts-and-bolts.html#data-frames
# 
# http://www.endmemo.com/program/R/gregexpr.php
# 
# https://shiny.rstudio.com/
# 
# SOBRE LISTAS VETORES ETC (CURSO BOM)
# https://bookdown.org/wevsena/curso_r_tce/curso_r_tce.html#o-que-e-uma-lista
# 
# ************** MAIS SOBRE R **********************
#
# Replacing values with NA
# 
# https://cran.r-project.org/web/packages/naniar/vignettes/replace-with-na.html
#
# http://rstudio-pubs-static.s3.amazonaws.com/261838_71b13475011340ab94e9c51d8e462080.html
#
# is.na(NA_character_)
# [1] TRUE
# 
# is.na(NA_integer_)
# [1] TRUE
# 
# is.na(NA_real_)
# [1] TRUE
# 
# is.na(NA_complex_)
# [1] TRUE
# 
# class(NA_integer_)
# [1] "integer"
# 
# typeof(NA_integer_) 
# [1] "integer"

