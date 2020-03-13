# 
# PUC Minas - Ciência de Dados
# Projeto Final - TCC 
# Aluno: Marcio Guillardi da Silva
# 
# Limpa a lista de objetos
# 
rm(list = ls())
# 
# Carregamento das Bibliotecas (library)
#
library(tidyverse)
library(tidyr)
#
# Define a pasta/diretório de trabalho
#
setwd("C:/Users/Marcio/Dropbox (Pessoal)/TCC_PUCMinas/PUC_Minas")
#
# Lê o arquivo novo completo (novas colunas - último chamado - Campos como FACTOR)
#
patrimonio_novo <- read.csv(".\\data-raw\\movimentacoesPGT_2.csv", sep = ";", encoding = "UTF-8")
#
# Mudando a ordem das colunas e alterando os nomes de algumas delas
#
colnames(patrimonio_novo) <- c("id", "data", "interna", "retorno", "cedente", "responsavel", "nivelSuperior", "sala", "tombamento", "descricao",
                              "grupo", "inventario", "responsavelCadastro", "responsavelCiencia", "dataConfirmacaoRecebimento", "dataBaixa", 
                              "dataEncerramentoGarantia", "valorEntrada")
# 
# Alterando a ordem das colunas & Agrupando por Responsável e Cedente
# 
patrimonio_novo <- patrimonio_novo %>% select(id, responsavel, cedente, sala, nivelSuperior, tombamento, descricao, grupo, everything()) %>%
  arrange(responsavel, cedente)
# 
# Cria um dataset sumarizado por Responsável e Cedente (somatório das movimentações)
# Arquivo com a contagem de movimentações por Responsável (quem recebeu o bem) e pelo cedente (quem cedeu o bem)
# 
patrimonio_sumarizado <- patrimonio_novo %>% group_by(responsavel, cedente) %>% tally(name = "movimentacoes")
# 
# Cria um dataset sumarizado por Responsável (somatório das movimentações)
# Arquivo com a contagem de movimentações por Responsável (quem recebeu o bem) e pelo cedente (quem cedeu o bem)
# 
view(patrimonio_sumarizado_responsavel) <- patrimonio_novo %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# Gravando arquivo sumarizado para Análise etc (CSV)
# 
write.csv(patrimonio_sumarizado, file = ".\\data\\movimentacoesPGT_2_transformada_sumarizado.csv", fileEncoding = "UTF-8")
# 
# Dividindo a coluna sala em duas: Sala e a descrição do setor/gabinete
# 
patrimonio_novo <- patrimonio_novo %>% separate(sala, c("sala", "nivel1","nivel2", "nivel3"), sep = ",")
#



str_locate("SL.904A", "\w{2}/./d")



str(patrimonio_sumarizado)

patrimonio_novo_backup <- patrimonio_novo

view(patrimonio_sumarizado)
view(patrimonio_lixo)
View(patrimonio_select)
view(patrimonio_teste)
view(patrimonio_novo)


# LENDO ARQUIVOS CSV
patrimonio <- read.csv("C:\\Users\\marcio.silva.MPT\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\TODAS_MOVIMENTAÇÕES.csv", sep = ";", encoding = "UTF-8")
patrimonio_novo <- read.csv("C:\\Users\\Marcio\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\movimentacoesPGT_2.csv", sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
patrimonio_novo <- read.csv("C:\\Users\\Marcio\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\movimentacoesPGT_2.csv", sep = ";", encoding = "UTF-8", as.is = TRUE)
patrimonio_novo <- read.csv("C:\\Users\\Marcio\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\movimentacoesPGT_2_transformada.csv",sep = ",", encoding = "UTF-8")
imdb <- readr::read_rds("C:\\Users\\marcio.silva.MPT\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\imdb.rds")

# GRAVANDO ARQUIVOS CSV
write.csv(patrimonio, file = "C:\\Users\\marcio.silva.MPT\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\Patrimonio_Planilha.csv")
write.csv(patrimonio_novo, file = "C:\\Users\\marcio.silva.MPT\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT_2_transformada.csv")



patrimonio_novo <- select(patrimonio_novo, id:valor.entrada)


# FILTRANDO O DATASET

patrimonio_select <- patrimonio_novo %>% filter(patrimonio_novo, grepl("BENS NÃO LOCALIZADOS NA SALA", sala, fixed = TRUE))

patrimonio_select <- filter(patrimonio_novo, sala == "ED. CNC" & is.na(nivel2))

patrimonio_select <- filter(patrimonio_novo, is.na(responsavel))

patrimonio_select <- filter(patrimonio_novo, is.na(responsavel))

patrimonio_select <- filter(patrimonio_novo, grepl("ROLL", sala, fixed = TRUE))

patrimonio_select <- filter(patrimonio_novo, is.na(caracter))

patrimonio_select <- filter(patrimonio_novo, grepl("º ANDAR", nivelSuperior, fixed = TRUE))

patrimonio_select <- filter(patrimonio_novo, is.na(nivel2))

view(patrimonio_select)

patrimonio_teste <- patrimonio_select %>% mutate(sala = if_else(sala == "ED. CNC" & is.na(nivel2) & str_length(nivelSuperior) > 3, 
                                                                str_sub(local, gregexpr(pattern = ",", local, fixed = TRUE)[[1]][2]+2, str_length(local)),
                                                                str_trim(sala), 
                                                                missing = NULL),
                                                 nivel2 = if_else(is.na(nivel2) & str_length(nivelSuperior) >3,
                                                                str_sub(nivelSuperior, gregexpr(pattern = ",", nivelSuperior, fixed = TRUE)[[1]][1]+2, str_length(nivelSuperior)),
                                                                str_trim(nivel2), 
                                                                missing = NULL),
                                                 caracter = gregexpr(pattern = ",", local, fixed = TRUE)[[1]][2], 
                                                 tamanho  = str_length(local))

view(patrimonio_teste)

patrimonio_novo <- patrimonio_novo %>% mutate(sala = if_else(nivel2 == "ROLL", 
                                                               str_trim(nivel2),
                                                               str_trim(sala), 
                                                               missing = NULL),
                                              nivel2 = if_else(grepl("ROLL", local, fixed = TRUE),
                                                               str_sub(nivelSuperior, gregexpr(pattern = ",", nivelSuperior, fixed = TRUE)[[1]][1]+2, str_length(nivelSuperior)),
                                                               str_trim(nivel2), 
                                                               missing = NULL))

patrimonio_novo <- patrimonio_novo %>% mutate(sala = if_else(is.na(sala), 
                                                             str_sub(local, gregexpr(pattern = ",", local, fixed = TRUE)[[1]][2]+2, str_length(local)),
                                                             str_trim(sala), 
                                                             missing = NULL))

view(patrimonio_novo)

patrimonio_teste <- patrimonio_teste %>% mutate(nivelSuperior = sub("ED. CNC, ", "CNC, ", nivelSuperior))

patrimonio_novo$sala[patrimonio_novo$sala == "0602"] <- "602"

patrimonio_novo <- patrimonio_novo %>% mutate(nivel2 = if_else(nivel1 == "ED. CNC" & grepl("ANDAR", nivel2, fixed = TRUE),
                                                        str_trim(nivel4), 
                                                        nivel2), 
                                              nivel1 = if_else(nivel1 == "ED. CNC" & grepl("SALA", nivel3, fixed = TRUE), 
                                                        str_trim(nivel3), 
                                                        nivel1))


patrimonio_teste <- patrimonio_novo %>% mutate(sala = if_else(sala == "ED. CNC" & is.na(nivel2), 
                                                              str_trim(local),
                                                              "NÃO", 
                                                              missing = NULL))

patrimonio_teste <- patrimonio_novo %>% mutate(sala = if_else(sala == "ED. CNC", 
                                                              "SIM",
                                                              "NÃO", 
                                                              missing = NULL))


patrimonio_select <- patrimonio_select%>% mutate(local = if_else(local == "ED. CNC, ANDAR TERREO, RECEPÇAO", 
                                                                 "ED. CNC, ANDAR TÉRREO, RECEPÇÃO", 
                                                                 str_trim(local)))

patrimonio_teste <- tibble(x1 = patrimonio_novo$sala[patrimonio_novo$sala == "ED. CNC" & is.na(patrimonio_novo$nivel2)], 
                           x2 = patrimonio_novo$local[patrimonio_novo$sala == "ED. CNC" & is.na(patrimonio_novo$nivel2)])

patrimonio_lixo <- filter(patrimonio_novo, patrimonio_novo$sala == "1506A")
patrimonio_lixo <- filter(patrimonio_novo, patrimonio_novo$sala == "SIM")

gregexpr(pattern = ",", "ED. CNC, ANDAR TERREO, PORTARIA CNC", fixed = TRUE)[[1]][2] 

gregexpr(pattern = ",", "ED. CNC, 07° ANDAR, BANHEIRO FEMININO", fixed = TRUE)[[1]][2]+2

str_sub("ED. CNC, 06° ANDAR, COPA 06", gregexpr(pattern = ",", "ED. CNC, 06° ANDAR, COPA 06", fixed = TRUE)[[1]][2]+2, str_length("ED. CNC, 06° ANDAR, COPA 06"))


# -------------------------------------------------------------------------------------------------------

varStrSub <- "ED. CNC, 07° ANDAR, BANHEIRO FEMININO"

str_sub(varStrSub, gregexpr(pattern = ",", varStrSub)[[1]][2]+2, str_length(varStrSub))

# str_sub(local, gregexpr(pattern = ",", local)[[1]][2]+2, str_length(local))

# -------------------------------------------------------------------------------------------------------


patrimonio_patrimnovo_baixados <- patrimonio_novo %>% filter(month(dataBaixa) == 1)
patrimonio_novo_baixados <- patrimonio_novo %>% filter(month(dataBaixa) %in% c(1,2,3))

patrimonio_novo <- patrimonio_novo %>% mutate(str_split(sala,","))
patrimonio_novo <- patrimonio_novo %>% mutate(str_split(sala,","))


patrimonio_select <- filter(patrimonio_novo, responsavel == "Alex Mendonca Feitosa")


patrimonio_novo <-  patrimonio_novo %>% mutate(nivel2 = if_else(nivel1 == "ED. CNC" & grepl("ANDAR", nivel2, fixed = TRUE), 
                                                                str_trim(nivel4), nivel2), 
                                               nivel1 = if_else(nivel1 == "ED. CNC" & grepl("SALA", nivel3, fixed = TRUE), 
                                                                str_trim(nivel3), nivel1))


patrimonio_novo <-  patrimonio_novo %>% mutate(nivel1 = if_else(nivel1 == "BENS NÃO LOCALIZADOS NA" & nivel2 == "SALA Nº 501C", "BENS NÃO LOCALIZADOS NA SALA Nº 501C", nivel1), nivel2 = if_else(nivel1 == "BENS NÃO LOCALIZADOS NA SALA Nº 501C" & nivel2 == "SALA Nº 501C", "COORDENAÇÃO DE SUPORTE AO USUÁRIO	", nivel2))



patrimonio_novo <- mutate(separate(data = patrimonio_novo, col = sala, into = c("nivel1", "nivel2", "nivel3", "nivel4", "nivel5"), sep = "\\,"))


patrimonio_novo <- patrimonio_novo %>% mutate(nivel2 = if_else(is.na(nivel2), nivel1, nivel2, missing = 'NA'))



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
# ************** MAIS SOBRE R **********************
  
ggplot(data = sumarizado) + 
  geom_point(mapping = aes(x = Responsável, y = n))


sumarizado_sala <- patrimonio %>% subset(Sala != "NA") %>% group_by(Sala) %>% tally()

sumarizado_sala %>% ggplot(mapping = aes(x = Sala, y = n)) + 
  geom_point()

ggplot(data = mpg, aes(x = displ, y = hwy, color = class)) + 
  +     geom_point() + labs(x = "Cilindradas", y = "Milhas/galão")



# ARRANGE com alteração do NOME DAS COLUNAS

selecao <- flights %>% arrange(year, month, day) %>% select(year, month, day)

selecao <- flights %>% arrange(year,month,day) %>% select(Ano = year, Mes = month, Dia = day)


imdb %>%
  mutate(
    lucro = receita - orcamento,
    lucro = ifelse(lucro <= 0, "Não", "Sim")
  ) %>% 
  filter(!is.na(lucro)) %>% 
  ggplot() + 
  geom_point(mapping = aes(x = orcamento, y = receita, color = lucro))


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
  
patrimonio <- patrimonio %>% select(Id:Data, Data.de.Confirmação.de.Recebimento, Local.de.Destino)

patrimonio_novo_sumarizado <- patrimonio_novo %>% subset(responsavel != "NA") %>% group_by(responsavel, cedente) %>% tally(name = "movimentacoes")
patrimonio_novo_sumarizado <- patrimonio_novo %>% subset(!is.na(responsavel)) %>% group_by(responsavel, cedente) %>% tally(name = "movimentacoes")

patrimonio <- patrimonio %>% mutate(Sala = Local.de.Destino)
patrimonio <- patrimonio %>% mutate(Sala = str_replace_all(Sala,"Nº ",""))
patrimonio <- patrimonio %>% mutate(Sala = str_replace_all(Sala,"SL. ",""))
patrimonio <- patrimonio %>% mutate(Sala = str_replace_all(Sala,"SL.","")) 

patrimonio <- patrimonio %>% subset(Id != "63261")
patrimonio <- patrimonio %>% subset(Id != "76760")

patrimonio$Data.Prevista.para.Devolução[patrimonio$Data.Prevista.para.Devolução ==""] <- NA
patrimonio$Data.de.Confirmação.de.Recebimento[patrimonio$Data.de.Confirmação.de.Recebimento ==""] <- NA
patrimonio$Fornecedor[patrimonio$Fornecedor ==""] <- NA
patrimonio$Responsável[patrimonio$Responsável ==""] <- NA

patrimonio_novo <- patrimonio_novo %>% mutate(nivel1 = str_replace_all(nivel1,"SALA Nº ",""))
patrimonio_novo <- patrimonio_novo %>% mutate(sala = str_replace_all(sala,"CNC, 14ºAND.","CNC, 14º ANDAR"))
patrimonio_novo <- patrimonio_novo %>% mutate(nivel2 = str_replace_all(nivel2,"14ºAND.","14º ANDAR"))
patrimonio_novo <- patrimonio_novo %>% mutate(local = str_replace_all(local,"ED. CNC, ANDAR TÉRREO PORTARIA CNC","ED. CNC, ANDAR TÉRREO, PORTARIA"))
patrimonio_novo <- patrimonio_novo %>% mutate(nivelSuperior = str_replace_all(nivelSuperior,"SANITÁRIOS, VESTIÁRIOS E COPA","SANITÁRIOS E COPA"))

patrimonio_novo <- patrimonio_novo %>% mutate(nivelSuperior = sub("ºAND.", "º ANDAR", nivelSuperior))
patrimonio_novo <- patrimonio_novo %>% mutate(nivelSuperior = sub("ED. CNC, ", "CNC, ", nivelSuperior))

# *** APAGA OS REGISTROS DE MOVIMENTAÇÕES TEMPORÁRIAS **** 
  
patrimonio <- patrimonio %>% subset(Responsável != "NA") %>% group_by(Responsável, Cedente)


# **** DESAGRUPA UM DATA FRAME ANTERIORMENTE AGRUPADO ==== O SELECT() CONSIDERA AS COLUNAS DE AGRUPAMENTO

patrimonio %>% ungroup() %>% select(Id)

patrimonio %>% ungroup() %>% select(Sala)

teste_patrimonio <- patrimonio %>% mutate(Sala = str_sub(Sala, 1,10))

patrimonio %>% ungroup() %>% select(Sala) %>%mutate(Sala = str_sub(Sala, 1,10))

teste_patrimonio <- patrimonio %>% mutate(Sala = ifelse(gregexpr(pattern = ",", Sala)[[1]][1]>0,str_sub(Sala, 1,gregexpr(pattern = ",", Sala)[[1]][1]-1), Sala))

# (* https://gomesfellipe.github.io/post/2017-12-17-string/string/)

teste_patrimonio <- patrimonio %>% mutate(Sala = if_else(grepl(",", Sala, fixed = TRUE), str_sub(Sala, 1,10), Sala))


resultado <-  gregexpr(pattern ='2',"thequickbrownfoxeswe2retired")

if_else(resultado[[1]][1]>0, "Encontrado", "Não Encontrado")


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

patrimonio_select <- filter(patrimonio_novo, grepl("BENS NÃO LOCALIZADOS NA SALA", sala, fixed = TRUE))
patrimonio_select <- filter(patrimonio_novo, grepl("^0", sala))

patrimonio_novo$sala[patrimonio_novo$sala == "0602"] <- "602"
patrimonio_novo$sala[patrimonio_novo$sala == "0101C"] <- "101C"
patrimonio_novo$sala[patrimonio_novo$sala == "0603B"] <- "603B"
patrimonio_novo$local[patrimonio_novo$id == "41571"] <- "ED. CNC, ANDAR TÉRREO, PROTOCOLO"
patrimonio_novo$nivel2[patrimonio_novo$id == "41571"] <- "ANDAR TÉRREO"
patrimonio_novo$nivelSuperior[patrimonio_novo$id == "41571"] <- "CNC, ANDAR TÉRREO"

patrimonio_select <- filter(patrimonio_novo, grepl("^0", sala))
patrimonio_select <- filter(patrimonio_novo, grepl("^0", sala))


# forcats::fct_explicit_na(religion))
# detach(package:tidyr)
# install.packages(c("nycflights13", "gapminder", "Lahman", "lubridate"))

# 
# git add -A
# git commit -m "A commit from my local computer"
# git push
# 
# https://help.github.com/en/github/using-git/renaming-a-remote
# 
# patrimonio_novo_sumarizado %>% filter(movimentacoes > 1000) %>% ggplot() +
#   geom_bar(mapping = aes(x = movimentacoes))
# 
# library(devtools)
# library(dplyr)
# library(stringr)
# library(lubridate)
# library(nycflights13)
# library(lubridate)
# 