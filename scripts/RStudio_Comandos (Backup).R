# 
# PUC Minas - Ciência de Dados
# Projeto Final - TCC 
# Aluno: Marcio Guillardi da Silva
# 
rm(list = ls())
# 
# Carregamento das Bibliotecas (library)
#
library(tidyverse)
library(tidyr)
library(lubridate)
library(readr)
library(gtools)
#
# Define a pasta/diretório de trabalho
#
setwd("C:/Users/Marcio/Dropbox (Pessoal)/TCC_PUCMinas/PUC_Minas")
#
# Lê o arquivo com todas as movimentações/completo (campos como STRINGS/CARACTERES)
# 
# mpi = Movimentação Patrimonial Interna
#
mpi <- read.csv(".\\data-raw\\movimentacoesPatrimoniaisInternas.csv", sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
#
# (41.311 Registros)
#
#
# mpi_sumarizado <- mpi %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# (365 observações)
#
# mpi_sumarizado %>% filter(movimentacoes > 100) %>%
#   ggplot(mapping = aes(x = responsavel, y = movimentacoes, colour=responsavel)) +
#   geom_point() + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
#   labs(y = "Qtde de Movimentaçõs", x = "Responsável (Destino das Movimentações)") +
#   ggtitle("Responsável x Quantidade de Movimentações", subtitle = "(* Mais de 100 Movimentações)")
# 
# 
# "inventario" == !NA: movimentação automática de grande quantidade de bens para ajuste de localidade de inventário/levantamento patrimonial
# 
mpi <- mpi %>% filter(is.na(inventario))
# 
# (30.961 Registros) 
# (10.350 Registros ignorados)
#
# 
# Quando o cedente e o responsável são  as mesmas pessoas indica (normalmente) que houve uma movimentação de ajuste de localidade ou 
# uma mudança em grande escala. Neste caso as movimentações não serão computadas para fins estatísticos, pois permanece sob a responsabilidade
# da mesma pessoa.
# 
mpi <- mpi %>% filter(!(responsavel == cedente))
#
# (21.005 Registros)
# (9.956 Registros ignorados) 
#
# 
# Mudando a ordem das colunas e alterando os nomes de algumas delas
#
colnames(mpi) <- c("id", "dataMovimentacao", "interna", "retorno", "cedente", "responsavel", "nivelSuperior", "sala", "tombamento",
                   "inventario", "responsavelCadastro", "dataConfirmacaoRecebimento")
# 
# Alterando a ordem das colunas & Agrupando por Responsável e Cedente
# 
mpi <- mpi %>% select(id, responsavel, cedente, sala, nivelSuperior, tombamento,everything()) %>%
  arrange(responsavel, cedente)
# 
# Operação para padronização/ajuste da sala, caso: "sala" == "PROTOCOLO GERAL" ou "sala" == "SEÇÃO DE ARQUIVO"
# (* Houve um erro na classificação/descrição da árvore de dependência entre as salas e seu nível superior) 
#
# mpi_teste <- mpi %>% filter(sala == "PROTOCOLO GERAL" | sala == "SEÇÃO DE ARQUIVO")
# 
# (186 Registros)
# 
# sala          = "PROTOCOLO GERAL" 'OU' sala = "SEÇÃO DE ARQUIVO"
# nivelSuperior = "SL. 803, DEP. DOCUMENTAÇÃO E GESTÃO DA INFORMAÇÃO"
# 
# sala <- nivelSuperior + " ," + sala
# nivelSuperior <- "ED. CNC, 08º ANDAR"
# 
mpi <- mpi %>% mutate(sala = if_else(sala == "PROTOCOLO GERAL" | sala == "SEÇÃO DE ARQUIVO",
                                     paste(nivelSuperior, sala, sep = ", "),
                                     str_trim(sala)),
                      nivelSuperior = if_else(nivelSuperior == "SL. 803, DEP. DOCUMENTAÇÃO E GESTÃO DA INFORMAÇÃO",
                                              "ED. CNC, 08º ANDAR",
                                              str_trim(nivelSuperior)))
#
# 
# Dividindo a coluna "SALA" em três níveis de detalhamento: nº da sala + 3 níveis (formato chr / caracter)
# 
# Descrição da SALA antes da alteração (exemplo/ID 14320): "ED. CNC, 15º ANDAR, SALA Nº 1507A, COORDENADOR DA ASSESSORIA DE PLANEJAMENTO E GESTÃO"
# 
# Após execução da operação abaixo: sala   <- "ED. CNC"
#                                   nível1 <- "15º ANDAR"; 
#                                   nível2 <-  "SALA Nº 1507A" 
#                                   nivel3 <- "COORDENADOR DA ASSESSORIA DE PLANEJAMENTO E GESTÃO"
# nivel1 com  2.770 registros = NA 
# nivel2 com 16.395 registros = NA 
# nivel3 com 18.079 registros = NA 
# 
mpi <- mpi %>% separate(sala, c("sala", "nivel1","nivel2", "nivel3"), sep = "\\,\\s")
# 
# mpi <- separate(data = mpi, col = sala, into = c("sala", "nivel1", "nivel2", "nivel3"), sep = "\\,\\s")
# 
# 
# Padronização da "sala" e dos níveis de detalhamento da sala
# 
# 'SE' sala == "ED. CNC" ~ (nivel1 <- nivel3)
# 
# Exemplo:
# 
# sala    = "ED. CNC"
# nivel1  = "12º ANDAR"
# nivel2  = "SALA Nº 1206"
# nivel3 != "NA" / "ASS GAB DR LUIS ANTONIO C DE MELO - 1º OFICIO"
# 
# mpi_teste <- mpi %>% filter(grepl("ED\\. CNC", sala))
# 
mpi <- mpi %>% mutate(nivel1 = if_else(sala == "ED. CNC" & !is.na(nivel3),
                                       str_trim(nivel3),
                                       str_trim(nivel1)))  # IMUTÁVEL / Permanece com o seu conteúdo atual
#
# 
# 'SE' sala == "ED. CNC" 'E' nivel2 'DIFERENTE DE' 'NA' ~ sala <- nivel2 (transformando("SALA Nº" -> "SL. Nº"))
# 
# Exemplo:
# 
# nível2 = "SALA Nº 1507A" 'QUANDO' sala = "ED. CNC"
# 
mpi <- mpi %>% mutate(sala = if_else(sala == "ED. CNC" & !is.na(nivel2),
                                     str_replace_all(nivel2, "SALA Nº", "SL. Nº"),
                                     if_else(sala != "ED. CNC",
                                             str_trim(sala),
                                             if_else(!is.na(nivel2), 
                                                     str_trim(nivel2), 
                                                     str_trim(sala)),
                                             missing = NULL)))
# 
# 
# Após essa transformação, o campo "sala" também indica: 
# 
# "BENS NÃO LOCALIZADOS...": bens não loalizados em diligências de inventários ou mudanças físicas de grande escala (NÃO serão considerados!)
#
# (1.503 Registros = "BENS NÃO LOCALIZADOS")
# 
# mpi_teste <- mpi %>% filter(grepl("BENS NÃO LOCALIZADOS", sala))
# 
mpi <- mpi%>% filter(!grepl("BENS NÃO LOCALIZADOS", sala, fixed = TRUE))
#
# (19.502 Registros)
#
# 
# ... o campo "nivelSuperior" indica: 
# "DESFAZIMENTO..."- movimentação de grande quantidade de bens para doação (NÃO serão considerados!)
# 
mpi <- mpi %>% filter(!grepl("DESFAZIMENTO", nivelSuperior, fixed = TRUE))
# 
# (631 Registros = DESFAZIMENTO - nivelSuperior)
# 
mpi <- mpi %>% filter(!grepl("DESFAZIMENTO", nivel1))
#
# (45 Registros = DESFAZIMENTO - nivel1 / 539 Registros com nivelSuperior = DESFAZIMENTO )
# 
# (18.826 Registros)
#
# 
# ... o campo "sala" indica: 
#
# "UL VIRTUAL" e "UL GERAL"- movimentação de grande quantidade de bens para doação ou incorporação para posterior distribuição
# 
mpi <- mpi %>% filter(!grepl("UL VIRTUAL", sala, fixed = TRUE) &
                        !grepl("UL GERAL", sala, fixed = TRUE))
#
# (16.676 Registros / 2.150 Registros ignorados)
#
# 
# Apagando movimentações temporárias de grande quantidade de bens (responsáveis temporários para conferência/incorporação de bens)
#
# Inicialmente foi feito um levantamento das MPI (Movimentações Patrimoniais Internas) que movimentaram os bens para conferência/incorporação
# ... e foram apagados a partir do número de ID da MPI... (Notebooks adquiridos para Procuradores e Diretores)
# 
# for (mpiId in c(74883,74795,7500,75041,75092,75130,75841,75842,75933,76054,76101,76278,76282,76321,76366,76384,76388,76389,76390,82778)){
#   mpi <- mpi[which(!mpi$id == mpiId),]
# }
#
# ... foi identificado que o código acima (id da MPI) não seria uma forma efetiva para eliminar aquelas MPIs; poderia haver 
# movimentações não identificadas (manualmente). Uma análise do padrão dessas movimentações foi feita e, então, 
# considerado que os responsáveis seriam sempre os mesmos e os patrimônios (tombamentos) estariam dentro de 
# uma determinada faixa...
# 
mpi <- filter(mpi, !(tombamento >= 37827 &        # Dentro da faixa... 32827 ~ 38623
                       tombamento <= 38623 &
                       (responsavel == "Daniela Heitor de Moura" | responsavel == "Levy Carlos Caixeta de Sa" | responsavel == "Marcio Guillardi da Silva")))
#
# (15.139 Registros / 1.537 Registros ignorados)
#
# mpi_teste <- mpi[which(!(mpi$tombamento >= 37827 &
#                             mpi$tombamento <= 38623 &
#                             (mpi$responsavel == "Levy Carlos Caixeta de Sa" | mpi$responsavel == "Daniela Heitor de Moura" | mpi$responsavel == "Marcio Guillardi da Silva"))
#                         ),]
#
#
# Também houve aquisição e incorporação de painéis divisórios individuais (baias) para todas as salas...
# ... esses bens já existiam e foram incorporados aos bens patrimoniais da PGT.
# 
mpi <- filter(mpi, !(tombamento >= 36843 &        # Dentro da faixa... 36843 ~ 37817
                       tombamento <= 37817 &
                       cedente == "Levy Carlos Caixeta de Sa"))
#
# (14247 Registros / 892 Registros ignorados )
# 
# mpi_sumarizado <- mpi %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# (351 Observações)
# 
# ggplot(data = mpi_sumarizado) + 
#   geom_point(mapping = aes(x = responsavel, y = movimentacoes)) + theme(axis.text.x=element_blank(),
#                                                                         axis.ticks.x=element_blank()) +
#   labs(y = "Quantidade de Movimentaçõs", x = "Responsável (Destino das Movimentações)")
# 
# mpi_sumarizado %>% filter(movimentacoes > 100) %>% 
#   ggplot(mapping = aes(x = responsavel, y = movimentacoes, colour=responsavel)) + 
#     geom_point() + theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5)) +
#       labs(y = "Qtde de Movimentaçõs", x = "Responsável (Destino das Movimentações)") +
#        ggtitle("Responsável x Quantidade de Movimentações", subtitle = "(* Mais de 100 Movimentações)")
# 
# 
# Substituição de caracteres no campo "SALA" - Padronizando os nomes
# 
# mpi <- mutate_at(mpi, vars("sala"), as.factor)
# str(mpi)
# mpi <- mutate_at(mpi, vars("sala"), as.character)
# 
# $ sala: Factor w/ 368 levels
# 
# sala == "SL.904A" -> "SL. Nº 904A"
#
mpi <- mpi %>% mutate(sala = gsub("^(SL\\.)([^[:space:]])","\\1 Nº \\2", sala))
# 
# sala == "SALA 604B1 RECEPÇÃO DA TI" -> "SL. Nº 604B1 RECEPÇÃO DA TI"
# 
mpi <- mpi %>% mutate(sala = gsub("(^SALA\\s{1})([\\Nº])","SL. \\2", sala))
# 
# sala = "SALA Nº 604B1 RECEPÇÃO DA TI" -> "SL. Nº 604B1 RECEPÇÃO DA TI"
# 
mpi <- mpi %>% mutate(sala = gsub("^(SL\\.)([^[:space:]])","\\1 Nº \\2", sala))
# 
# sala == "SL. 904A" -> "SL. Nº 904A"
# 
mpi <- mpi %>% mutate(sala = gsub("^(SL\\.)([[:space:]])([^\\Nº])","SL. Nº\\2\\3", sala))
# 
# Inserir espaços antes e após o caracter "-" (travessão)
# 
# x <- "GAB DR OTAVIO BRITO LOPES-7º OFICIO"
# x <- "GAB DR MAURICIO CORREIA DE MELLO-35º OFICIO"
# x <- "GAB DRA CRISTINA S. ALMEIDA NOBRE-33º OFICIO"
# 
# gsub("([[A-z]]*)(\\S)([-+])([0-9])","\\1\\2 - \\4", x)
# grepl("([[A-z]]*)([-+])([0-9])",x)
# 
# mpi_teste <- mpi %>% filter(grepl("OTAVIO BRITO", nivel1))
# mpi_teste <- mpi %>% filter(grepl("([[A-z]]*)(\\s)([-+])\\s([0-9])",nivel1))
# mpi_teste <- mpi %>% filter(grepl("([[A-z]]*)([-+])([0-9])",nivel3))
#
mpi <- mpi %>% mutate(nivel1 = gsub("([[A-z]]*)(\\S)([-+])([0-9])","\\1\\2 - \\4", nivel1))
# 
# nivel1 = "ANDAR TERREO" ~ "ANDAR TÉRREO"
# 
mpi <- mpi %>% mutate(nivel1 = if_else(nivel1 == "ANDAR TERREO",
                                       "ANDAR TÉRREO",
                                       nivel1))
# 
# Padronizando o campo nivelSuperior
# 
# nivelSuperior = CNC - TERREO. // sala = SL. Nº 001 - PROTOCOLO
# 
mpi <- mpi %>% mutate(sala = if_else(grepl("CNC - TERREO.", nivelSuperior),
                                     "SL. Nº 001",
                                     sala),
                      nivel1 = if_else(grepl("CNC - TERREO.", nivelSuperior),
                                       "PROTOCOLO GERAL ATENDIMENTO AO PUBLICO - DA",
                                       nivel1),
                      nivelSuperior = if_else(grepl("CNC - TERREO.", nivelSuperior),
                                              "CNC, ANDAR TÉRREO",
                                              nivelSuperior)) # %>% filter(grepl("PROTOCOLO GERAL ATENDIMENTO AO PUBLICO - DA", nivel1))
# 
# "ED. CNC, 12º ANDAR" ~ "CNC, 12º ANDAR"
#
mpi <- mpi %>% mutate(nivelSuperior = gsub("^(ED. CNC. )", "CNC, \\2", nivelSuperior)) # %>% filter(grepl("ED. CNC,",nivelSuperior))
#
# 
# "CNC, 03ºAND." ~ "CNC, 03º ANDAR"
#
# mpi_teste <- filter(mpi, grepl(("ºAND."), nivelSuperior))
# mpi_teste <- mpi %>% filter(grepl("([º])(AND)(\\.)+", nivelSuperior))
#
mpi <- mpi %>% mutate(nivelSuperior = gsub("([º])(AND)(\\.)+","\\1 \\2AR", nivelSuperior))
# 
# 'SE' nivelSuperior = "PGT"
# 
# 'E' salas = 
# "SL. Nº 801"
# "SL. Nº 807"
# "SL. Nº 1005"
# "SL. Nº 1007"
# "SL. Nº 1007A"
# "SL. Nº 1107"
# "SL. Nº 1107A"
# "SL. Nº 1108"
# "SL. Nº 1108A"
# "SL. Nº 1301"
# "SL. Nº 1301A"
# "SL. Nº 1506"
# "SL. Nº 0002"
# "SL. Nº 00002"
# "SL. Nº 00004"
# 
varSalas <- c("SL. Nº 401C", "SL. Nº 801", "SL. Nº 807", "SL. Nº 1005", "SL. Nº 1007", "SL. Nº 1007A", "SL. Nº 1107", "SL. Nº 1107A", "SL. Nº 1108",
              "SL. Nº 1108A", "SL. Nº 1301", "SL. Nº 1301A", "SL. Nº 1506", "SL. Nº 0002", "SL. Nº 00002", "SL. Nº 00004", "SL. Nº 1501A", "ED. CNC")
# 
andares <- c("CNC, 04º ANDAR", "CNC, 08º ANDAR", "CNC, 08º ANDAR", "CNC, 10º ANDAR", "CNC, 10º ANDAR", "CNC, 10º ANDAR", "CNC, 11º ANDAR", 
             "CNC, 11º ANDAR", "CNC, 11º ANDAR", "CNC, 11º ANDAR", "CNC, 13º ANDAR", "CNC, 13º ANDAR", "CNC, 15º ANDAR", "CNC, ANDAR TÉRREO",
             "CNC, ANDAR TÉRREO", "CNC, ANDAR TÉRREO", "CNC, 15º ANDAR", "CNC, ANDAR TÉRREO")
# 
mpi <- mpi %>% mutate(nivelSuperior = if_else(nivelSuperior == "PGT",
                                              if_else(sala %in% varSalas,
                                                      andares[varSalas %>% match(x = sala)],
                                                      nivelSuperior),
                                              nivelSuperior))
# 
# match_ <- "SL. Nº 401C"
# 
# andares[varSalas %>% match(x = match_)]  
# 
# match_ %in% varSalas          # Retorna TRUE
# 
# 
# nivel1 = "ROLL" / sala = 17º ANDAR / nivelSuperior = "CNC, 17º ANDAR" / Trocando sala x nivel1
# 
mpi_row <- row.names(mpi[which(mpi$nivel1 == "ROLL"),])
# 
# mpi_row <- rownames(subset(mpi, mpi$nivel1 == "ROLL"))
# 
for (rowNames in mpi_row) {
  mpi[rowNames,"nivel2"] = mpi[rowNames,"sala"]
  mpi[rowNames,"sala"] = mpi[rowNames,"nivel1"]
  mpi[rowNames,"nivel1"] = mpi[rowNames,"nivel2"]
} 
# 
# Outra forma de realizar o mesmo procedimento acima: 
# 
#            sala = "17º ANDAR"
#          nivel1 = "ROLL"
# nivelSuperirior = "CNC, 17º ANDAR"
# 
# (CNC,)(17º ANDAR)
# 
# gregexpr(pattern = ",", nivelSuperior, fixed = TRUE)[[1]][1]+2, str_length(nivelSuperior)
# 
# 'gregexpr' retorna uma lista com o mesmo tamanho que o texto, 
# cada elemento da mesma forma que o valor de retorno para regexpr, 
# exceto que as posições iniciais de cada correspondência (disjunta)
# são fornecidas.
#  
#  x <- 'CNC, 17º ANDAR'
# 
# str_sub(x, gregexpr(pattern = ",", x, fixed = TRUE)[[1]][1]+2,  str_length(x))
# [1] "17º ANDAR"
# 
# mpi_teste <- mpi %>% filter(grepl("ROLL",nivel1))
# 
# mpi_teste <- mpi %>% mutate(sala = if_else(nivel1 == "ROLL",
#                                            str_trim(nivel1),
#                                            str_trim(sala), 
#                                            missing = NULL),
#                             nivel1 = if_else(grepl("ROLL", sala, fixed = TRUE),
#                                              str_sub(nivelSuperior, gregexpr(pattern = ",", nivelSuperior, fixed = TRUE)[[1]][1]+2, str_length(nivelSuperior)),
#                                              str_trim(nivel2), 
#                                              missing = NULL)) %>% filter(grepl("ROLL",sala))
# 
# mpi_teste <- mpi %>% filter(grepl("ROLL",sala))  # Verificando o resultado
# 
# 
# sala = "COPA 16" <- sala   = "SANITÁRIOS E COPA"
# nivel1 = "16º ANDAR"  <-  nivelSuperior = "CNC, 16º ANDAR"
# x <- "18º ANDAR"
# gsub("^([0-9]*)(\\º)(\\s)([A-Z]*)", "\\1", x)
# [1] "18"
#
mpi$sala[mpi$sala == "COPA 1"] <- "COPA 13"
# 
# sala = "SANITÁRIOS E COPA"
# nivelSuperior = "CNC, 12º ANDAR"
# 
# nivel1 <- "12º ANDAR"
# sala <- COPA + 12
# 
mpi <- mpi %>% mutate(nivel1 = if_else(sala == "SANITÁRIOS E COPA",
                                       gsub("^(CNC, )", "\\2", nivelSuperior),
                                       nivel1),
                      sala = if_else(sala == "SANITÁRIOS E COPA",
                                     paste("COPA", gsub("^([0-9]*)(\\º)(\\s)([A-Z]*)", "\\1", nivel1), sep = " "),
                                     sala)) # %>% filter(grepl("COPA", sala))
# 
# mpi_teste <- mpi %>% filter(grepl("COPA", sala))
#
# x <- "COPA 18"
# gsub("^([A-Z]*)(\\s)([0-9]*)", "\\3", x)
# [1] "18"
# 
# 
# 'SE' sala = "COPA 03"
# 'SE' nivelSuperior = "SANITÁRIOS E COPA"
# 
# nivel1        <- "03º ANDAR"
# nivelSuperior <- "CNC, 3º ANDAR"
# 
mpi <- mpi %>% mutate(nivel1 = if_else(nivelSuperior == "SANITÁRIOS E COPA",
                                       paste(gsub("^([A-Z]*)(\\s)([0-9]*)", "\\3", sala), "º ANDAR", sep = ""),
                                       nivel1),
                      nivelSuperior = if_else(nivelSuperior == "SANITÁRIOS E COPA",
                                              paste("CNC, ",gsub("^([A-Z]*)(\\s)([0-9]*)", "\\3", sala), "º ANDAR"),
                                              nivelSuperior)) # %>% filter(grepl("COPA", sala))
# 
# mpi_teste <- mpi %>% filter(grepl("COPA", sala))
#
# 
# Transformações Pontuais (pequenas quantidade de dados identificados)
# 
mpi$nivel1[mpi$sala == "SL. Nº 101A - CENTRAL DE TELEFONIA"] <- "CENTRAL DE TELEFONIA"
mpi$sala[mpi$sala == "SL. Nº 101A - CENTRAL DE TELEFONIA"] <- "SL. Nº 101A"
# 
mpi$nivel1[mpi$sala == "SL. Nº 101SS - SALA DE MONITORAMENTO"] <- "SALA DE MONITORAMENTO"
mpi$sala[mpi$sala == "SL. Nº 101SS - SALA DE MONITORAMENTO"] <- "SL. Nº 101SS"
# 
mpi$nivel1[mpi$sala == "SL. Nº 707 - DA - DL - SEÇÃO DE PATRIMÔNIO"] <- "SEÇÃO DE PATRIMÔNIO - DL"
mpi$sala[mpi$sala == "SL. Nº 707 - DA - DL - SEÇÃO DE PATRIMÔNIO"] <- "SL. Nº 707"
# 
mpi$nivel1[mpi$sala == "SL. Nº 1108A,GAB DRA IVANA AUXILIADORA MENDONÇA SANTOS - 12º OFICIO"] <- "GABINETE DRA IVANA AUXILIADORA MENDONÇA SANTOS - 12º OFICIO"
mpi$sala[mpi$sala == "SL. Nº 1108A,GAB DRA IVANA AUXILIADORA MENDONÇA SANTOS - 12º OFICIO"] <- "SL. Nº 1108A"
# 
mpi$nivel1[mpi$sala == "SL. Nº 1208,ASS GAB DRA HELOÍSA MARIA MORAES RÊGO PIRES - 4º OFICIO"] <- "ASSESSORIA DRA HELOÍSA MARIA MORAES RÊGO PIRES - 4º OFICIO"
mpi$sala[mpi$sala == "SL. Nº 1208,ASS GAB DRA HELOÍSA MARIA MORAES RÊGO PIRES - 4º OFICIO"] <- "SL. Nº 1208"
# 
mpi$nivel1[mpi$sala == "SL. Nº 302M CAIS/AREA ARMAZENAMENTO - DRH"] <- "CAIS/AREA ARMAZENAMENTO - DRH"
mpi$sala[mpi$sala == "SL. Nº 302M CAIS/AREA ARMAZENAMENTO - DRH"] <- "SL. Nº 302M"
# 
mpi$nivel1[mpi$sala == "SL. Nº 1704,MEMORIAL DO MPT - CDI"] <- "MEMORIAL DO MPT - CDI"
mpi$sala[mpi$sala == "SL. Nº 1704,MEMORIAL DO MPT - CDI"] <- "SL. Nº 1704"
# 
mpi$nivel1[mpi$sala == "SL. Nº 504A CODEP/DRH"] <- "CODEP - DRH"
mpi$sala[mpi$sala == "SL. Nº 504A CODEP/DRH"] <- "SL. Nº 504A"
# 
mpi$nivel1[mpi$sala == "DEPÓS. - SUBSOLO"] <- "DEPÓSITOS DO SETOR DE PATRIMÔNIO"
mpi$sala[mpi$sala == "DEPÓS. - SUBSOLO"] <- "DEPÓSITOS DO SETOR DE PATRIMÔNIO"
# 
#
# sala = "MESAS PARA IMPRESSORAS DA SIMPRESS" / Padronização da "sala" e dos níveis
#
mpi_row <- rownames(subset(mpi, mpi$sala == "MESAS PARA IMPRESSORAS DA SIMPRESS"))
# 
for (rowNames in mpi_row) {
  mpi[rowNames,"sala"] = "SL. Nº 604B"
  mpi[rowNames,"nivel1"] = "ASS. DO DEPART. DE TECNOLOGIA DA INFORMAÇÃO"
  mpi[rowNames,"nivel2"] = "MESAS PARA IMPRESSORAS DA SIMPRESS"
  mpi[rowNames,"nivelSuperior"] = "CNC, 06º ANDAR"
}
# 
# mpi_teste <- mpi %>% filter(grepl("SL. 0604B, ASS. DTI - DP. TEC. DA INFORMAÇÃO", nivelSuperior))
# mpi_teste <- mpi %>% filter(grepl("MESAS PARA IMPRESSORAS DA SIMPRESS", nivel2))
#
# Outra forma de realizar o mesmo procedimento acima:
#  
# mpi <- mpi %>% mutate(sala = if_else(nivelSuperior == "SL. 0604B, ASS. DTI - DP. TEC. DA INFORMAÇÃO",
#                                      "SL. Nº 604B",
#                                      str_trim(sala)),
#                       nivel1 = if_else(nivelSuperior == "SL. 0604B, ASS. DTI - DP. TEC. DA INFORMAÇÃO",
#                                        "ASS. DO DEPART. DE TECNOLOGIA DA INFORMAÇÃO",
#                                        str_trim(nivel1)),
#                       nivel2 = if_else(nivelSuperior == "SL. 0604B, ASS. DTI - DP. TEC. DA INFORMAÇÃO",
#                                        "MESAS PARA IMPRESSORAS DA SIMPRESS",
#                                        str_trim(nivel2)),
#                       nivelSuperior = if_else(nivelSuperior == "SL. 0604B, ASS. DTI - DP. TEC. DA INFORMAÇÃO",
#                                               "CNC, 06º ANDAR",
#                                               str_trim(nivelSuperior)))
# 
# mpi_teste <- filter(mpi, grepl("SALA 604B1 RECEPÇÃO DA TI", nivel2))
# 
mpi <- mpi %>% mutate(sala = if_else(grepl("SALA 604B1 RECEPÇÃO DA TI", nivel2),
                                     "SL. Nº 604B1",
                                     sala),
                      nivel1 = if_else(grepl("SALA 604B1 RECEPÇÃO DA TI", nivel2),
                                       "RECEPÇÃO DO DEPART. DE TECNOLOGIA DA INFORMAÇÃO",
                                       nivel1))
# 
# mpi_teste <- mpi %>% filter(nivel2 == "SALA 604B1 RECEPÇÃO DA TI")
# 
mpi <- mpi %>% mutate(nivel1 = if_else(sala == "SL. Nº 802 DEOF-DEP.EXEC. ORÇAMENTÁRIA E FINANCEIRA",
                                       "DEP. EXEC. ORÇAMENTÁRIA E FINANCEIRA",
                                       str_trim(nivel1)),
                      sala = if_else(sala == "SL. Nº 802 DEOF-DEP.EXEC. ORÇAMENTÁRIA E FINANCEIRA",
                                     "SL. Nº 802",
                                     str_trim(sala)))
# 
# mpi_teste <- mpi %>% filter(sala == "SL. Nº 802")
# 
# 
# mpi$sala[mpi$sala == "0602"]  <- "602"      # De forma "manual", um por um...
# mpi$sala[mpi$sala == "0101C"] <- "101C"
# mpi$sala[mpi$sala == "0603B"] <- "603B"
# 
# x <- "SL. Nº 0603"
# gsub("^(SL\\.\\s)(Nº\\s)(0)([^0])","\\1\\2\\4", x)
#                                     SL. Nº 603
# 
# mpi_teste <- mpi %>% filter(grepl("^(SL\\.\\s)(Nº\\s)(0)([^0])", sala))
# 
mpi <- mpi %>% mutate(sala = if_else(grepl("^(SL\\.\\s)(Nº\\s)(0)([^0])", sala),
                                     gsub("^(SL\\.\\s)(Nº\\s)(0)([^0])","\\1\\2\\4", sala),
                                     sala)) # %>% filter(grepl("^(SL\\.\\s)(Nº\\s)(603)", sala))
# 
# 
# Padronizando a Descrição dos Gabinetes dos Procuradores (Assessorias e Gabinetes)
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("ASS DR", nivel1),
                                       str_replace_all(nivel1, "ASS DR", "ASSESSORIA DR"),
                                       nivel1))
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("ASSESSORIA GAB\\. DRA\\.", nivel1),
                                       str_replace_all(nivel1, "ASSESSORIA GAB\\. DRA\\.", "ASSESSORIA DRA"),
                                       nivel1)) 
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("ASSESSORIA GAB\\. DR\\.", nivel1),
                                       str_replace_all(nivel1, "ASSESSORIA GAB\\. DR\\.", "ASSESSORIA DR"),
                                       nivel1)) 
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("ASS. GAB DR", nivel1),
                                       str_replace_all(nivel1, "ASS\\. GAB DR", "ASSESSORIA DR"),
                                       nivel1)) 
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("ASS GAB DR\\.", nivel1),
                                       str_replace_all(nivel1, "ASS GAB DR\\.", "ASSESSORIA DR"),
                                       nivel1)) 
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("ASS GAB DR", nivel1),
                                       str_replace_all(nivel1, "ASS GAB DR", "ASSESSORIA DR"),
                                       nivel1)) 
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("GAB DR", nivel1),
                                       str_replace_all(nivel1, "GAB DR", "GABINETE DR"),
                                       nivel1)) 
#
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("GAB\\. DR", nivel1),
                                       str_replace_all(nivel1, "GAB\\. DR", "GABINETE DR"),
                                       nivel1)) 
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("GABINETE DR\\.", nivel1),
                                       str_replace_all(nivel1, "GABINETE DR\\.", "GABINETE DR"),
                                       nivel1)) 
# 
mpi <- mpi %>% mutate(nivel1 = if_else(grepl("GABINETE DRA\\.", nivel1),
                                       str_replace_all(nivel1, "GABINETE DRA\\.", "GABINETE DR"),
                                       nivel1)) 
# 
# mpi_teste <- mpi %>% filter(grepl("JOSE NETO", nivel1))
# 
# 
# Leitura do arquivo geral de bens (patrimonios) para realizar a junção (left_join) dos arquivos (Recuperar a Descrição) 
# 
bensPatrimoniais <- read.csv(".\\data-raw\\BensPorGrupo.csv", sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
# 
bensPatrimoniais <- mutate_at(bensPatrimoniais, vars("tombamento"), as.integer)
# 
# left_join(table_1, table_2, by = c("ID_1" = "ID_2"))
# 
# mpi_select <- mpi %>% left_join(bensPatrimoniais, by = c("tombamento"))
#
# Inserindo a descrição do bem patrimonial a partir do arquivo geral de bens
# 
mpi <- mpi%>% mutate(descricaoBem = bensPatrimoniais$descricao[match(tombamento, bensPatrimoniais$tombamento)])
#
# 
# mpi <- mutate_at(mpi, vars("sala"), as.factor); str(mpi); mpi <- mutate_at(mpi, vars("sala"), as.character)
# 
# $ sala: Factor w/ 368 levels
# 
# mpi_sumarizado <- mpi %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# Gravando arquivo sumarizado para Análise etc (CSV) para o Power BI / Tableau
# 
# write.csv(mpi_sumarizado, file = ".\\data\\mpi_Sumarizado.csv", fileEncoding = "UTF-8")
# write.csv(mpi, file = "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT.csv", fileEncoding = "UTF-8")
# 
# Pacote library(readr)
# 
# write_excel_csv2(mpi, "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT.csv", na = "NA", append = FALSE, delim = ";", quote_escape = "double")
# 
# write_csv(mpi, "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT.csv")
# 
# ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK 
# ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK 
# 


# 
# FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO 
# FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO FAZER ESSA MUTAÇAO 

# OK OK OK



mpi_teste <- mpi %>% filter(grepl("([SL. Nº])(AND)(\\.)+", nivelSuperior))



x <- "ED. CNC. 08º ANDAR 48"
gsub("^(ED. CNC, )", "CNC, \\2", x)


x <- "SALA 604B1 RECEPÇÃO DA TI"
gsub("(^SALA\\s{1})([^\\Nº])","SL. Nº \\2", x)

x <- "SALA Nº 801"
gsub("(^SALA\\s{1})([\\Nº])","SL. \\2", x)

x <- "SALA Nº 604B1 RECEPÇÃO DA TI"
gsub("(^SALA\\s{1})([\\Nº])","SL. \\2", x)

x <- "SL. Nº 904A"
gsub("^(SL\\.)\\s+","\\1 Nº ", x)

x <- "SL. 904A"
gsub("^(SL\\.)([[:space:]])([^\\Nº])","SL. Nº\\2\\3", x)

# FILTRANDO O DATASET

names(mpi)  # Nomes das colunas

mpi_teste <- filter(mpi, grepl("^(SL\\.)([^[:space:]])", sala))

mpi_teste <- filter(mpi, grepl(("ºAND\\."), nivelSuperior))
mpi_teste <- filter(mpi, grepl(("^BENS NÃO LOCALIZADOS"), sala))
mpi_teste <- filter(mpi, grepl("Guillardi", responsavel))
mpi_teste <- filter(mpi, grepl("Patricia Rambo", responsavel))
mpi_teste <- filter(mpi, grepl("PROTOCOLO GERAL",sala)|grepl("SEÇÃO DE ARQUIVO",sala))
mpi_teste <- filter(mpi, grepl("PROTOCOLO GERAL",nivel3))
mpi_teste <- filter(mpi, grepl("SEÇÃO DE ARQUIVO",nivel3))
mpi_teste <- filter(mpi, grepl("ED\\. CNC",nivelSuperior))
mpi_teste <- filter(mpi, grepl("76388",X.U.FEFF.id))
mpi_teste <- filter(mpi, grepl("BENS NÃO LOCALIZADOS NA SALA", sala, fixed = TRUE))
mpi_teste <- filter(mpi, grepl("ROLL", sala, fixed = TRUE))
mpi_teste <- filter(mpi, grepl("º ANDAR", nivelSuperior, fixed = TRUE))
mpi_teste <- filter(mpi, grepl("BENS NÃO LOCALIZADOS NA SALA", sala, fixed = TRUE))
mpi_teste <- filter(mpi, grepl("LOTE", sala, fixed = TRUE))
mpi_teste <- filter(mpi, grepl("DESFAZIMENTO", nivelSuperior, fixed = TRUE))
mpi_teste <- filter(mpi, grepl("^0", sala))

bens_select <- filter(bensPatrimoniais, tombamento >= 37827 & tombamento <= 38623 & dataBaixa == "")

mpi_select <- filter(mpi_select, (responsavel == "Daniela Heitor de Moura" | responsavel == "Levy Carlos Caixeta de Sa" | responsavel == "Marcio Guillardi da Silva"))

mpi_select <- filter(mpi_select, tombamento >= 37827 & tombamento <= 38623 &
                       ((cedente == "Levy Carlos Caixeta de Sa" & mpi$responsavel == "Daniela Heitor de Moura") |
                          (cedente == "Daniela Heitor de Moura" & responsavel == "Levy Carlos Caixeta de Sa"))
)

mpi_select <- filter(mpi_temp, tombamento >= 37800 & tombamento <= 38650)
bens_select <- filter(bensPorGrupo, id >= 37800 & id <= 38650)


patrimonio_select <- filter(mpi, sala == "PROTOCOLO GERAL" | sala == "SEÇÃO DE ARQUIVO")
patrimonio_select <- filter(mpi, is.na(nivel2))
patrimonio_select <- filter(mpi_teste, local == "ED. CNC" & !is.na(nivel1))
patrimonio_select <- filter(mpi, is.na(responsavel))
patrimonio_select <- mutate_at(patrimonio_novo, vars("tombamento"), as.integer)
patrimonio_select <- filter(patrimonio_select, (tombamento < 38624 & tombamento > 37827))

patrimonio_select <- patrimonio_novo$dataConfirmacaoRecebimento[patrimonio_novo$X.U.FEFF.id == 17478]

patrimonio_select <- mpi[which(mpi$id == "14320"),]

patrimonio_novo$local[patrimonio_novo$id == "41571"] <- "ED. CNC, ANDAR TÉRREO, PROTOCOLO"
patrimonio_novo$nivel2[patrimonio_novo$id == "41571"] <- "ANDAR TÉRREO"
patrimonio_novo$nivelSuperior[patrimonio_novo$id == "41571"] <- "CNC, ANDAR TÉRREO"

patrimonio_select <- filter(patrimonio_novo, grepl("^0", sala))
patrimonio_select <- filter(patrimonio_novo, grepl("^0", sala))


# MUDANÇA DE TIPO DE COLUNA (FACTOR, DATA ETC)

patrimonio_novo <- mutate_at(patrimonio_novo, vars("responsavel", "cedente"), as.character)
patrimonio_novo <- mutate_at(patrimonio_novo, vars("responsavel", "cedente"), as.factor)
patrimonio_novo <- mutate_at(patrimonio_novo, vars("sala", "nivel1", "nivel2", "nivel3"), as.factor)
mpi = dmy(mpi_novo$data)
mpi$dataConfirmacaoRecebimento = dmy_hm(patrimonio_novo$dataConfirmacaoRecebimento)
# 
patrimonio_novo[37451,]                                                # Retorna a linha correspondente (na sequencia)
patrimonio_novo[[9]][patrimonio_novo$X.U.FEFF.id == "82778"]           # Retorna todos os registros encontrados no dataframe (df[[coluna]][id="busca"])
patrimonio_novo[match("76388",patrimonio_novo$X.U.FEFF.id),]           # Retorna o primeiro registro encontrado correspondente ao id informado
patrimonio_novo$tombamento[patrimonio_novo$X.U.FEFF.id == "76388"]     # Retorna uma lista com o número do tombamento dos registros correspondentes
patrimonio_novo$X.U.FEFF.id == "76388"                                 # Retorna uma lista de TRUE ou FALSE correspondente ao registro encontrado ou não
which(patrimonio_novo$X.U.FEFF.id == "76282")                          # Retorna o número da linha que contém o registro encontrado / id Movimentação = 76388
#
txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo", txt)))
  cat("'foo' appears at least once in\n\t", txt, "\n")
i # 2 and 4
txt[i]

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
gsub("^(SL.)\\s+","\\1 Nº ", x)

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

mpi <- mpi %>% mutate(sala = if_else(sala == "ED. CNC" & !is.na(nivel2),
                                     str_replace_all(nivel2, "SALA Nº", "SL. Nº"),
                                     if_else(sala != "ED. CNC",
                                             str_trim(sala),
                                             if_else(!is.na(nivel2), 
                                                     str_trim(nivel2), 
                                                     str_trim(sala)),
                                             missing = NULL)))

# 
# gregexpr(pattern = ",", local, fixed = TRUE) = 
# 
# [[1]]
# [1] 13 51
# attr(,"match.length")
# [1] 1 1
# 
local <- "SL. Nº 1804C, ASSESSORIA DA VICE PROCURADORA GERAL, 18º ANDAR"
# 
str_sub(local, gregexpr(pattern = ",", local, fixed = TRUE)[[1]][2]+2, str_length(local))

patrimonio_teste <- mpi %>% mutate(sala = if_else(sala == "ED. CNC" & !is.na(nivel2), 
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

mpi_select <- mpi %>% mutate(sala = if_else(nivel2 == "ROLL",
                                            str_trim(nivel2),
                                            str_trim(sala), 
                                            missing = NULL),
                             nivel2 = if_else(grepl("ROLL", sala, fixed = TRUE),
                                              str_sub(nivelSuperior, gregexpr(pattern = ",", nivelSuperior, fixed = TRUE)[[1]][1]+2, str_length(nivelSuperior)),
                                              str_trim(nivel2), 
                                              missing = NULL))

patrimonio_novo <- patrimonio_novo %>% mutate(sala = if_else(is.na(sala), 
                                                             str_sub(local, gregexpr(pattern = ",", local, fixed = TRUE)[[1]][2]+2, str_length(local)),
                                                             str_trim(sala), 
                                                             missing = NULL))

view(patrimonio_novo)

mpi <- mpi %>% mutate(nivelSuperior = sub("ED. CNC, ", "CNC, ", nivelSuperior))

mpi_backup <- mpi

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


str_sub("ED. CNC, 06º ANDAR, COPA 06", gregexpr(pattern = ",", "ED. CNC, 06º ANDAR, COPA 06", fixed = TRUE)[[1]][2]+2, str_length("ED. CNC, 06º ANDAR, COPA 06"))


# -------------------------------------------------------------------------------------------------------

varStrSub <- "ED. CNC, 07º ANDAR, BANHEIRO FEMININO"

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

# 
# For completeness, write_csv() from the readr package is faster and never writes row names
# 
# install.packages('readr', dependencies = TRUE)
library(readr)
write_csv(t, "t.csv")
# 
# If you need to write big data out, use fwrite() from the data.table package. It's much faster than both write.csv and write_csv
# 
# install.packages('data.table')
library(data.table)
fwrite(t, "t.csv")
# 
# Below is a benchmark that Edouard published on his site
# 
microbenchmark(write.csv(data, "baseR_file.csv", row.names = F),
               write_csv(data, "readr_file.csv"),
               fwrite(data, "datatable_file.csv"),
               times = 10, unit = "s")

by(dataFrame, 1:nrow(dataFrame), function(row) dostuff)

library("gtools")
asc("°")
asc("º")
chr(186)

