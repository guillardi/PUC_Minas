# 
# PUC Minas - Ciência de Dados
# Projeto Final - TCC 
# Aluno: Marcio Guillardi da Silva
# 
# Limpar a lista de objetos/dataframe para o Power BI/Tableau
# 
rm(list = ls())
# 
# Carregamento das Bibliotecas (library)
#
library(tidyverse)
library(tidyr)
library(lubridate)
#
# Define a pasta/diretório de trabalho
#
setwd("C:/Users/Marcio/Dropbox (Pessoal)/TCC_PUCMinas/PUC_Minas")
#
# Lê o arquivo novo completo (novas colunas - último chamado - Campos como FACTOR)
#
patrimonio_novo <- read.csv(".\\data-raw\\movimentacoesPGT_2.csv", sep = ";", encoding = "UTF-8")
# 
# patrimonio_sumarizado_responsavel <- patrimonio_novo %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# patrimonio_sumarizado_responsavel %>% filter(movimentacoes > 100) %>% 
#   ggplot(mapping = aes(x = responsavel, y = movimentacoes, colour=responsavel)) + 
#   geom_point() + theme(axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
#   labs(y = "Qtde de Movimentaçõs (> 100)", x = "Responsável (Destino das Movimentações)") +
#   ggtitle("Contagem de Movimentações")
# 
# "inventario" == !NA: movimentação automática de grande quantidade de bens para ajuste de localidade de inventário/levantamento patrimonial
# 
patrimonio_novo <- patrimonio_novo %>% filter(is.na(inventario))
# 
# patrimonio_sumarizado_responsavel <- patrimonio_novo %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# patrimonio_sumarizado_responsavel %>% filter(movimentacoes > 100) %>% 
#   ggplot(mapping = aes(x = responsavel, y = movimentacoes, colour=responsavel)) + 
#   geom_point() + theme(axis.text.x=element_blank(), plot.title = element_text(hjust = 0.5)) +
#   labs(y = "Qtde de Movimentaçõs (> 100)", x = "Responsável (Destino das Movimentações)") +
#   ggtitle("Contagem de Movimentações")
#
# Transformando em caracteres os atributos (colunas) "RESPONSAVEL" e "CEDENTE":
# 
# Quando o cedente e o responsável são  as mesmas pessoas indica (normalmente) que houve uma movimentação de ajuste de localidade ou 
# uma mudança em grande escala. Neste caso as movimentações não serão computadas para fins estatísticos, pois permanece sob a responsabilidade
# da mesma pessoa.
# 
# Erro para operação/comparação com um campo "factor": 'Error in Ops.factor(responsavel, cedente): level sets of factors are different'
# 
patrimonio_novo <- mutate_at(patrimonio_novo, vars("responsavel", "cedente"), as.character)
patrimonio_novo <- patrimonio_novo %>% filter(!(responsavel == cedente))
patrimonio_novo <- mutate_at(patrimonio_novo, vars("tombamento", "responsavel", "cedente"), as.factor)
patrimonio_novo$data = dmy(patrimonio_novo$data)
patrimonio_novo$dataConfirmacaoRecebimento = dmy_hm(patrimonio_novo$dataConfirmacaoRecebimento)
# 
# Mudando a ordem das colunas e alterando os nomes de algumas delas
#
colnames(patrimonio_novo) <- c("id", "dataMovimentacao", "interna", "retorno", "cedente", "responsavel", "nivelSuperior", "sala", "tombamento",
                               "inventario", "responsavelCadastro", "dataConfirmacaoRecebimento")
# 
# Alterando a ordem das colunas & Agrupando por Responsável e Cedente
# 
patrimonio_novo <- patrimonio_novo %>% select(id, responsavel, cedente, sala, nivelSuperior, tombamento,everything()) %>%
  arrange(responsavel, cedente)
# 
# Padronização para a sala caso seja: "sala" == "PROTOCOLO GERAL" | "sala" == "SEÇÃO DE ARQUIVO"
# 
patrimonio_novo <- patrimonio_novo %>% mutate(sala = if_else(sala == "PROTOCOLO GERAL" | sala == "SEÇÃO DE ARQUIVO",
                                                             paste(nivelSuperior, sala, sep = ", "),
                                                             str_trim(sala)),
                                              nivelSuperior = if_else(nivelSuperior == "SL. 803, DEP. DOCUMENTAÇÃO E GESTÃO DA INFORMAÇÃO",
                                                                      "ED. CNC. 08º ANDAR",
                                                                      str_trim(nivelSuperior)))
# 
# Dividindo a coluna "SALA" em três níveis de detalhamento: nº da sala + 3 níveis (formato chr / caracter)
# 
# Descrição da SALA antes da alteração (exemplo/ID 14320): "ED. CNC, 15° ANDAR, SALA Nº 1507A, COORDENADOR DA ASSESSORIA DE PLANEJAMENTO E GESTÃO"
# 
# Após execução da operação abaixo: sala <- "ED. CNC"; nível1 <- "15° ANDAR"; nível2 <-  "SALA Nº 1507A"
#                                                      nivel3 <- "COORDENADOR DA ASSESSORIA DE PLANEJAMENTO E GESTÃO"
# 
patrimonio_novo <- patrimonio_novo %>% separate(sala, c("sala", "nivel1","nivel2", "nivel3"), sep = "\\,\\s")
patrimonio_novo <- mutate_at(patrimonio_novo, vars("sala", "nivel1", "nivel2", "nivel3"), as.factor)
# 
# patrimonio_novo <- separate(data = patrimonio_novo, col = sala, into = c("sala", "nivel1", "nivel2", "nivel3"), sep = "\\,\\s")
# 
# 
# Padronização da sala e dos níveis de detalhamento da sala (nivel1 <- nivel3)
# 
patrimonio_novo <- patrimonio_novo %>% mutate(nivel1 = if_else(sala == "ED. CNC" & !is.na(nivel3), 
                                                                str_trim(nivel3),
                                                                str_trim(nivel1)))
# 
# se sala == "ED. CNC" 'e' nivel2 'não igual a' NA 'logo' sala <- nivel2 ~ transformando("SALA Nº" -> "SL. Nº")
# 
# Observe que nível2 pode conter "SALA Nº 1507A" 'quando' sala 'igual' "ED. CNC"
# 
patrimonio_novo <- patrimonio_novo %>% mutate(sala = if_else(sala == "ED. CNC" & !is.na(nivel2),
                                                             str_replace_all(nivel2, "SALA Nº", "SL. Nº"),
                                                             if_else(sala != "ED. CNC",
                                                                     str_trim(sala),
                                                                     if_else(!is.na(nivel2), str_trim(nivel2), str_trim(sala)),
                                                                     missing = NULL)))
# 
# 
# 
# Após essa transformação, o campo "sala" também indica: 
# 
# "BENS NÃO LOCALIZADOS...": bens não loalizados em diligências de inventários ou mudanças físicas de grande escala (NÃO serão considerados!)
# 
patrimonio_novo <- patrimonio_novo %>% filter(!grepl("BENS NÃO LOCALIZADOS", sala, fixed = TRUE))
# 
# ... o campo "nivelSuperior" indica: 
#
# "DESFAZIMENTO..."- movimentação de grande quantidade de bens para doação (NÃO serão considerados!)
# 
patrimonio_novo <- patrimonio_novo %>% filter(!grepl("DESFAZIMENTO", nivelSuperior, fixed = TRUE))
# 
# ... o campo "sala" indica: 
#
# "UL VIRTUAL" e "UL GERAL"- movimentação de grande quantidade de bens para doação ou incorporação para posterior distribuição
# 
patrimonio_novo <- patrimonio_novo %>% filter(!grepl("UL VIRTUAL", sala, fixed = TRUE) &
                                                !grepl("UL GERAL", sala, fixed = TRUE))
#
# Apagando movimentações temporárias de grande quantidade de bens (responsáveis temporários para conferência/incorporação de bens)
#
for (mpi in c(74883,74795,7500,75041,75092,75130,75841,75842,75933,76054,76101,76278,76282,76321,76366,76384,76388,76389,76390,82778)){
  patrimonio_novo <- patrimonio_novo[which(!patrimonio_novo$id == mpi),]
}
"Levy Carlos Caixeta de Sa"
"Daniela Heitor de Moura"
"SL. 402SS"
"SL. 0603B2"
"SL. 0603B2"
"SL. 0602"
#
# patrimonio_novo[37451,]                                                # Retorna a linha correspondente (na sequencia)
# patrimonio_novo[[9]][patrimonio_novo$X.U.FEFF.id == "82778"]           # Retorna todos os registros encontrados no dataframe (df[[coluna]][id="busca"])
# patrimonio_novo[match("76388",patrimonio_novo$X.U.FEFF.id),]           # Retorna o primeiro registro encontrado correspondente ao id informado
# patrimonio_novo$tombamento[patrimonio_novo$X.U.FEFF.id == "76388"]     # Retorna uma lista com o número do tombamento dos registros correspondentes
# patrimonio_novo$X.U.FEFF.id == "76388"                                 # Retorna uma lista de TRUE ou FALSE correspondente ao registro encontrado ou não
# which(patrimonio_novo$X.U.FEFF.id == "76282")                          # Retorna o número da linha que contém o registro encontrado / id Movimentação = 76388
#
# 
# Cria um dataset sumarizado por Responsável e Cedente (somatório das movimentações)
# Arquivo com a contagem de movimentações por Responsável (quem recebeu o bem) e pelo cedente (quem cedeu o bem)
# 
patrimonio_sumarizado <- patrimonio_novo %>% group_by(responsavel, cedente) %>% tally(name = "movimentacoes")
# 
# Cria um dataset sumarizado por Responsável (somatório das movimentações)
# Arquivo com a contagem de movimentações por Responsável (quem recebeu o bem) e pelo cedente (quem cedeu o bem)
# 
patrimonio_sumarizado_responsavel <- patrimonio_novo %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# Gravando arquivo sumarizado para Análise etc (CSV) para o Power BI / Tableau
# 
write.csv(patrimonio_sumarizado, file = ".\\data\\movimentacoesPGT_2_transformada_sumarizado.csv", fileEncoding = "UTF-8")
write.csv(patrimonio_novo, file = "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT_2_transformada.csv")


ggplot(data = patrimonio_sumarizado_responsavel) + 
  geom_point(mapping = aes(x = responsavel, y = movimentacoes)) + theme(axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank()) +
  labs(y = "Quantidade de Movimentaçõs", x = "Responsável (Destino das Movimentações)")

patrimonio_sumarizado_responsavel %>% filter(movimentacoes > 100) %>% 
  ggplot(mapping = aes(x = responsavel, y = movimentacoes, colour=responsavel)) + 
    geom_point() + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
      labs(y = "Qtde de Movimentaçõs", x = "Responsável (Destino das Movimentações)") +
        ggtitle("Responsável x Quantidade de Movimentações", subtitle = "(* Mais de 100 Movimentações)")

# 
# Substituição de caracteres no campo "SALA"
# 
# sala == "SL.904A" -> "SL. Nº 904A"
# 
patrimonio_novo <- patrimonio_novo %>% mutate(sala = gsub("^(SL.)([^[:space:]])","\\1 Nº \\2", sala))
# 
# sala == "SALA 604B1 RECEPÇÃO DA TI" -> "SL. Nº 604B1 RECEPÇÃO DA TI"
# 
patrimonio_novo <- patrimonio_novo %>% mutate(sala = gsub("(^SALA\\s{1})([\\Nº])","SL. \\2", sala))
# 
# sala == "SALA Nº 604B1 RECEPÇÃO DA TI" -> "SL. Nº 604B1 RECEPÇÃO DA TI"
# 
patrimonio_novo <- patrimonio_novo %>% mutate(sala = gsub("^(SL.)([^[:space:]])","\\1 Nº \\2", sala))
# 
# sala == "SL. 904A" -> "SL. Nº 904A"
# 
patrimonio_novo <- patrimonio_novo %>% mutate(sala = gsub("^(SL.)([[:space:]])([^\\Nº])","SL. Nº\\2\\3", sala))


x <- "SALA 604B1 RECEPÇÃO DA TI"
gsub("(^SALA\\s{1})([^\\Nº])","SL. Nº \\2", x)

x <- "SALA N° 801"
gsub("(^SALA\\s{1})([\\Nº])","SL. \\2", x)

x <- "SALA Nº 604B1 RECEPÇÃO DA TI"
gsub("(^SALA\\s{1})([\\Nº])","SL. \\2", x)

x <- "SL. Nº 904A"
gsub("^(SL.)\\s+","\\1 Nº ", x)

x <- "SL. 904A"
gsub("^(SL.)([[:space:]])([^\\Nº])","SL. Nº\\2\\3", x)


patrimonio_select <- filter(patrimonio_novo, !grepl("^(SL.)([[:space:]])", sala))
patrimonio_select <- filter(patrimonio_novo, grepl(("^PROTOCOLO "), sala))
patrimonio_select <- filter(patrimonio_novo, grepl(("^BENS NÃO LOCALIZADOS"), sala))
patrimonio_select <- filter(patrimonio_novo, grepl("Daniela Heitor de Moura", responsavel))
patrimonio_select <- filter(patrimonio_novo, grepl("Guillardi", responsavel))
patrimonio_select <- filter(patrimonio_novo, grepl("Patricia Rambo", responsavel))
patrimonio_select <- filter(patrimonio_novo, grepl("PROTOCOLO GERAL",sala))
patrimonio_select <- filter(patrimonio_novo, grepl("PROTOCOLO GERAL",nivel3))
patrimonio_select <- filter(patrimonio_novo, grepl("SEÇÃO DE ARQUIVO",nivel3))
patrimonio_select <- filter(patrimonio_novo, grepl("ED. CNC",sala))
patrimonio_select <- filter(patrimonio_novo, grepl("76388",X.U.FEFF.id))
.

patrimonio_novo_backup <- patrimonio_novo

patrimonio_novo <- patrimonio_novo_backup

txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo", txt)))
  cat("'foo' appears at least once in\n\t", txt, "\n")
i # 2 and 4
txt[i]


# BENS NÃO LOCALIZADOS
# UL VIRTUAL
# cedente == responsavel



patrimonio_novo_ <- patrimonio_novo_ %>% mutate(nivel2 = str_replace_all(nivel2,"14ºAND.","14º ANDAR"))


str_extract("SL.904A", "SL\\.[:digit:]{1}")  # [1] "SL.9"  : Apenas com o primeiro dígito numérico
str_extract("SL.904A", "SL\\.[:digit:]+")    # [1] "SL.904": Todos os dígitos (núméricos)
str_extract("SL.904A", "SL\\.[:alnum:]+")    # [1] "SL.904": Todos os dígitos (núméricos) e alfabéticos (letras)
str_extract("SL.904A", "SL\\.[0-9]+")
str_extract("SL.904A", "^(SL.)([^[:space:]])([[:alnum:]]+)")
!is.na(str_extract("SL.904A TESTE", "SL\\.[:digit:]+"))
!is.na(str_extract("SL. 904A TESTE", "^(SL.)"))
str_locate("SL.904A", "SL\\.[0-9]+")



# Utilizar essas funções para substituição de caracteres

x <- "SL. 904A"
x <- gsub("^(SL.)\\s+","\\1 Nº ", x)

x <- "SL.904A"
gsub("^(SL.)\\S","\\1 Nº ", x)
gsub("^(SL.)([^[:space:]])","\\1 Nº \\2", x)


# Exemplos

s = "PleaseAddSpacesBetweenTheseWords"
gsub("([a-z])([A-Z])", "\\1 \\2", s)

x <- "<dd>Found on January 1, 2007</dd>"
gsub("<dd>[F|f]ound on |</dd>", "", x)

c(1:10,"D")

hw <- "Hadley Wickham"
str_sub(hw, 1, 6)
str_sub(hw, end = 6)
str_sub(hw, 8, 14)
str_sub(hw, 8)
str_sub(hw, c(1, 8), c(6, 14))
.

# LENDO ARQUIVOS CSV
patrimonio_novo <- read.csv("C:\\Users\\Marcio\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\movimentacoesPGT_2.csv", sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
patrimonio_novo_as_is <- read.csv("C:\\Users\\Marcio.silva.mpt\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\movimentacoesPGT_2.csv", sep = ";", encoding = "UTF-8", as.is = TRUE)
patrimonio_novo <- read.csv("C:\\Users\\Marcio\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\movimentacoesPGT_2_transformada.csv",sep = ",", encoding = "UTF-8")
imdb <- readr::read_rds("C:\\Users\\marcio.silva.MPT\\Dropbox (Pessoal)\\PGT COSMOS PLANILHA\\imdb.rds")


# GRAVANDO ARQUIVOS CSV
write.csv(patrimonio_novo, file = "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT_2_transformada.csv")
#


patrimonio_novo_teste <- select(patrimonio_novo, id:valor.entrada)


# FILTRANDO O DATASET

patrimonio_select <- patrimonio_novo %>% filter(patrimonio_novo, grepl("BENS NÃO LOCALIZADOS NA SALA", sala, fixed = TRUE))
patrimonio_select <- filter(patrimonio_novo, sala == "ED. CNC" & !is.na(nivel2))
patrimonio_select <- filter(patrimonio_novo, is.na(responsavel))
patrimonio_select <- filter(patrimonio_novo, grepl("ROLL", sala, fixed = TRUE))
patrimonio_select <- filter(patrimonio_novo, grepl("º ANDAR", nivelSuperior, fixed = TRUE))
patrimonio_select <- filter(patrimonio_novo, is.na(nivel2))

view(patrimonio_select)

patrimonio_teste <- patrimonio_select %>% mutate(sala = if_else(sala == "ED. CNC" & !is.na(nivel2), 
                                                                str_sub(local, gregexpr(pattern = ",", local, fixed = TRUE)[[1]][2]+2, str_length(local)),
                                                                str_trim(sala), 
                                                                missing = NULL),
                                                 nivel1 = if_else(is.na(nivel1) & str_length(nivelSuperior) >3,
                                                                  str_sub(nivelSuperior, gregexpr(pattern = ",", nivelSuperior, fixed = TRUE)[[1]][1]+2, str_length(nivelSuperior)),
                                                                  str_trim(nivel1), 
                                                                  missing = NULL),
                                                 caracter = gregexpr(pattern = ",", local, fixed = TRUE)[[1]][2], 
                                                 tamanho  = str_length(local))


patrimonio_teste <- patrimonio_select %>% mutate(sala = if_else(sala == "ED. CNC" & !is.na(nivel1) & str_length(nivelSuperior) > 3, 
                                                                str_sub(local, gregexpr(pattern = ",", local, fixed = TRUE)[[1]][2]+2, str_length(local)),
                                                                str_trim(sala), 
                                                                missing = NULL),
                                                 nivel1 = if_else(is.na(nivel1) & str_length(nivelSuperior) >3,
                                                                str_sub(nivelSuperior, gregexpr(pattern = ",", nivelSuperior, fixed = TRUE)[[1]][1]+2, str_length(nivelSuperior)),
                                                                str_trim(nivel1), 
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


str_sub("ED. CNC, 06° ANDAR, COPA 06", gregexpr(pattern = ",", "ED. CNC, 06° ANDAR, COPA 06", fixed = TRUE)[[1]][2]+2, str_length("ED. CNC, 06° ANDAR, COPA 06"))


# -------------------------------------------------------------------------------------------------------

varStrSub <- "ED. CNC, 07° ANDAR, BANHEIRO FEMININO"

str_sub(varStrSub, gregexpr(pattern = ",", varStrSub)[[1]][2]+2, str_length(varStrSub))
# 
# Para aplicar em: 
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
  
ggplot(data = patrimonio_sumarizado_responsavel) + 
  geom_point(mapping = aes(x = responsavel, y = movimentacoes)) + theme(axis.text.x=element_blank(),
                                                                        axis.ticks.x=element_blank())

patrimonio_sumarizado_responsavel %>% filter(movimentacoes > 50) %>% ggplot(mapping = aes(x = responsavel, y = movimentacoes)) + 
  geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))


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

patrimonio_select <- patrimonio_novo$dataConfirmacaoRecebimento[patrimonio_novo$X.U.FEFF.id == 17478]
patrimonio_select <- filter(patrimonio_novo, grepl("BENS NÃO LOCALIZADOS NA SALA", sala, fixed = TRUE))
patrimonio_select <- filter(patrimonio_novo, grepl("LOTE", sala, fixed = TRUE))
patrimonio_select <- filter(patrimonio_novo, grepl("DESFAZIMENTO", nivelSuperior, fixed = TRUE))
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
# Comandos GIT para gravar os dados no Github
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
