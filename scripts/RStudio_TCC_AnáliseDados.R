#
#
#
mpiSumResponsavelCadastro <- mpi %>% group_by(responsavelCadastro) %>%
  tally(name = "bensMovimentados") # ,sort = TRUE
#
mpiSumResponsavelCadastro <- mpi %>% group_by(responsavelCadastro, year(dataMovimentacao), month(dataMovimentacao, label = TRUE)) %>%
  tally(name = "bensMovimentados") # ,sort = TRUE
#
#
# 
mpiSumSalas <- mpi %>% group_by(sala, ano = as.factor(year(dataMovimentacao))) %>%
  tally(name = "bensMovimentados") # ,sort = TRUE

mpiSumSala <- mpi %>% group_by(sala) %>%
  tally(name = "bensMovimentados") # ,sort = TRUE
#
#
#
mpiSum <- mpi %>% group_by(ano = as.factor(year(dataMovimentacao)), mes = as.factor(month(dataMovimentacao, label = TRUE))) %>%
  tally(name = "bensMovimentados") # %>% as.ts()

mpiSum <- mpi %>% filter(year(dataMovimentacao) == 2019) %>% group_by(mes = month(dataMovimentacao, label = TRUE)) %>%
  tally(name = "bensMovimentados") # %>% as.ts()

###################################################################################################################################
#
# Contanto as movimentações patrimoniais (total de bens movimentados por MPI por ANO e MÊS)
#
mpiSum <- mpi %>% group_by(ano = year(dataMovimentacao),  mes = month(dataMovimentacao, label = TRUE), id_mpi = id) %>%
  tally(name = "bensMovimentados")
#
# 4.033 movimentações
#
#
# Contando as movimentações patrimoniais (Totalizando por Mês)
#
mpiSum <- mpi %>% group_by(ano = year(dataMovimentacao),  mes = month(dataMovimentacao, label = TRUE), id) %>%
  tally() %>%  tally(name = "totalMPI")
#
# Mesma operação acima com summarise 
#
mpiSum <- mpi %>% group_by(ano = year(dataMovimentacao),  mes = month(dataMovimentacao, label = TRUE), id) %>%
  summarise(totalMPI = n_distinct(id)) %>% tally(name = "totalMPI")
#
# filter(mpiSum, ano == "2017", mes == "dez")
# 
mpiSum2017 <- mpi %>% filter(year(dataMovimentacao) == 2017) %>% group_by(mes = month(dataMovimentacao, label = TRUE), id) %>%
  tally() %>%  tally(name = "totalMPI")
#
#
###################################################################################################################################




#
# prop.table()
#
m <- matrix(1:4, 2)
m
prop.table(m,1) # linhas
prop.table(m,2) # colunas

#
# Prop SALA
#
# Table cria uma tabela das contagens de cada fator
# (table uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.)
#
prop <- as.data.frame(prop.table(table(mpi$sala)))

propTableMPI <- as.data.frame(table(mpi$sala))

prop_teste <- left_join(prop, propTableMPI, by = "Var1")

prop_teste <- mutate(prop_teste, Freq.x = Freq.x*100)

sum(prop_teste$Freq.y)
# 
# 14.282 Ocorrências
#
#
# Prop Responsável Cadastro
#
#
# Total de bens movimentados (proporção de bens movimentados)
#
prop <- as.data.frame(prop.table(table(mpi$responsavelCadastro)))
propTable <- as.data.frame(table(mpi$responsavelCadastro))
#
prop_teste <- left_join(prop, propTable, by = "Var1")
prop_teste <- mutate(prop_teste, Freq.x = Freq.x*100)
#
sum(prop_teste$Freq.y)
#
prop_teste %>% ggplot(aes(x = Var1, y = Freq.y)) +
  geom_col(aes(fill = paste(Var1, "(", Freq.y, ")"))) +
  geom_label(label = paste(format(prop_teste$Freq.x, digits=2, nsmall=2), "%", sep = ""), nudge_x = 0.25, nudge_y = 0.25) +
  labs(y = "Quantidade de Bens Movimentados", x = "Responsável pelo Cadastro", 
       title = "Responsável x Quantidade de Bens Movimentados",
       subtitle =  paste(gsub("(?!^)(?=(?:\\d{3})+$)", ".", 
                              sum(prop_teste$Freq.y, na.rm=TRUE), perl=T),
                         " MPIs Cadastradas"), 
       fill = "Responsável Cadastro")
#
# Total de Movimentações Internas cadastradas (proporção de MPIs cadastradas)
# 
propMPIs <- mpi %>% group_by(responsavelCadastro, id) %>% tally() # %>% tally(name = "Freq")
prop <- as.data.frame(prop.table(table(propMPIs$responsavelCadastro)))
propTable <- as.data.frame(table(propMPIs$responsavelCadastro))
#
prop_teste <- left_join(prop, propTable, by = "Var1")
#
prop_teste <- mutate(prop_teste, Freq.x = Freq.x*100)
#
plot(prop_teste$Var1, prop_teste$Freq.y, type = "o")
#
prop_teste %>% ggplot(aes(x = Var1, y = Freq.y)) +
  geom_col(aes(fill = paste(Var1, "(", Freq.y, ")"))) +
  geom_label(label = paste(format(prop_teste$Freq.x, digits=1, nsmall=2), "%", sep = ""), nudge_x = 0.25, nudge_y = 0.25) +
  labs(y = "Movimentações Cadastradas", x = "Responsável pelo Cadastro", 
       title = "Responsável x Quantidade de MPIs Cadastradas",
       subtitle =  paste(gsub("(?!^)(?=(?:\\d{3})+$)", ".", 
                              sum(prop_teste$Freq.y, na.rm=TRUE), perl=T),
                         " MPIs Cadastradas"), 
       fill = "Responsável Cadastro")
#
# Prop Tombamento
#
prop <- as.data.frame(prop.table(table(mpi$tombamento)))
#
propTableMPI <- as.data.frame(table(mpi$tombamento))
#
prop_teste <- left_join(prop, propTableMPI, by = "Var1")
#
prop_teste <- mutate(prop_teste, Freq.x = Freq.x*100)
#
prop_teste <- prop_teste[order(prop_teste$Freq.y),]
#
# Filtrando os bens com mais de 6 movimentações
#
# prop_teste <- prop_teste[prop_teste$Freq.y > 6,]
#
# Lendo as últimas 10 linhas do data frame
#
prop_teste <- tail(prop_teste, 10)
# 
# Recuperando a descrição do bem patrimonial do arquivo geral de bens
#
prop_teste <- prop_teste %>% mutate(descricaoBem = bensPatrimoniais$descricao[match(Var1, bensPatrimoniais$tombamento)])
#
sum(prop_teste$Freq.x)
#
prop_teste %>% ggplot(aes(x = Var1, y = Freq.y)) +
  geom_col(aes(fill = paste(descricaoBem, "(", Freq.y, ")"))) +
  geom_label(label = paste(format(prop_teste$Freq.x, digits=1, nsmall=2), "%", sep = ""), nudge_x = 0.25, nudge_y = 0.25) +
  labs(y = "Quantidade de Movimentações", x = "Tombamento do Bem", 
       title = "Bens Patrimoniais x Quantidade de Movimentações",
       subtitle =  paste(gsub("(?!^)(?=(?:\\d{3})+$)", ".", 
                              sum(prop_teste$Freq.y, na.rm=TRUE), perl=T),
                         " Movimentações - ",
                         format(sum(prop_teste$Freq.x), digits=2, nsmall=2), "% dos bens movimentados", sep = ""), 
       fill = "Descrição do Bem ")
#
#
# Total de Movimentações por Andar (Nível Superior)
#
# INCLUIR AQUI O FILTRO POR ANO
#
mpiSumNSuperior <- mpi %>% filter(year(dataMovimentacao) == 2019) %>% group_by(nivelSuperior) %>%
  tally(name = "bensMovimentados") %>% mutate(ano = 2019, ano = as.factor(ano))
#
# Total de Movimentações poa Andar e por Ano
#
mpiSumNSuperior <- mpi %>% group_by(nivelSuperior, ano = as.factor(year(dataMovimentacao))) %>%
  tally(name = "bensMovimentados") # ,sort = TRUE
# 
#
# Pontos com legendas / Formatado e Colorido
#
nFiltro = 0
#
mpiSumNSuperior %>% filter(bensMovimentados > nFiltro) %>%
  ggplot(mapping = aes(x = str_sub(nivelSuperior,1,15), y = bensMovimentados, fill = nivelSuperior)) +
  theme(axis.text.x = element_text(angle = 90, size = 9), plot.title = element_text(hjust = 0.5), 
        panel.grid.major = element_line(colour = "grey50"), legend.position = "none") + # legend.position = esconde a legenda
  labs(y = "Qtde de Movimentações", x = "nivelSuperior (Destino das Movimentações)", fill = "Nível Superior") +
  ggtitle("Quantidade de Movimentações x Nível Superior", 
          subtitle = paste("Ano(s):", toString(levels(mpiSumNSuperior$ano)), " - ",
                           if_else(nFiltro > 0, 
                                   paste("(* nivelSuperior com mais de ", nFiltro, " Movimentações)"),
                                   paste(as.character(gsub("(?!^)(?=(?:\\d{3})+$)", ".", 
                                                           sum(mpiSumNSuperior$bensMovimentados, na.rm=TRUE), perl=T)),
                                         "Bens Movimentados")))) +
  geom_label(
    label = filter(mpiSumNSuperior, bensMovimentados > nFiltro)[[2]], 
    nudge_x = 0.25, nudge_y = 0.25)
#




library(xlsx)
library(tseries)
library(gdata)

mpi <- read_csv2(".\\data-raw\\movimentacoesPatrimoniaisInternas.csv", 
                 col_types = cols(id = "c", data = col_date(format = "%d/%m/%Y"), tombamento = "c", inventario = "c", 
                                  dataConfirmacaoRecebimento = col_datetime(format = "%d/%m/%Y %H:%M")))

z = read.xlsx(".\\data-raw\\atmosfera.xls",1)
dim(z)

z[1:10,]

x=as.ts(z[,2:3])

plot(x)

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
##################################################################################################
# 
# Sumarizado por por ano & mês (Com Filtro)
# 
################################################################################################## 
#
mpiSumAnoMes <- mpi %>% filter(year(dataMovimentacao) == "2019") %>% group_by(ano = year(dataMovimentacao), 
                                                                              mes = month(dataMovimentacao, 
                                                                                          label = TRUE,  abbr = TRUE)) %>%
  tally(name = "movimentacoes") # ,sort = TRUE
# 
# Gráfico de Pizza () - mês & ano (contacatenados)
# 
pie(mpiSumAnoMes$movimentacoes, labels = paste(mpiSumAnoMes$mes, mpiSumAnoMes$ano, sep = " - "),
    main="Movimentações por Mes", 
    col = rainbow(12))
legend("topright",legend = paste(mpiSumAnoMes[[2]], mpiSumAnoMes[[1]], sep = " - "), cex = 0.8,
       fill = rainbow(12))
#
# Gráfico de Densidade
# 
ggplot(mpiSumAnoMes, aes(x = mes, y = ano, group = ano)) + 
  geom_density_ridges(fill = "#00AFBB", scale = 2) +
  theme_ridges()
#
# Gráfico de Barras
#
ggplot(mpiSumAnoMes,
       aes(x = mpiSumAnoMes$mes, y = mpiSumAnoMes$movimentacoes, fill = mes)) +
  geom_bar(stat = "Identity") +
  labs(y = "Qtde de Movimentações", x = "Responsável (Destino das Movimentações)") +
  ggtitle(paste("Movimentações por Mês", " - Ano: ", mpiSumAnoMes$ano[[1]][1], 
                "(", gsub("(?!^)(?=(?:\\d{3})+$)", ".", sum(mpiSumAnoMes$movimentacoes), perl=T),
                " Movimentações)")) + 
  theme_linedraw(base_size = 16)
# 
# 
# 

df <- spread(mpisumano)


name<- c("abc", "abc", "abc", "xyz", "xyz", "xyz")
type<- c(1, 2, 3, 1,2,3)
count<-c(30,50,39, 40,20,60)

df<-data.frame(name, type, count)
tidyr::spread(df, type, count) 

# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# Realizar uma análise das movimentações por período (de grandes valores)
#
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

mpi_bak <- mpi
# 
meses <-  c("Janeiro", "Fevereiro", "Março", "Abril", "Maio", "Junho", "Julho", "Agosto", "Setembro", "Outubro", "Novembro", "Dezembro")
# 
anoAnalise <- 2019
# 
for (indice in 1:12) { 
  #
  # indice <- 09
  # 
  mpi_sumarizado <- mpi %>% filter(year(dataMovimentacao) == anoAnalise & month(dataMovimentacao) == indice) %>%
    group_by(ano = year(dataMovimentacao), id, responsavel, cedente, sala = str_sub(sala, end = 20) ) %>% summarize("contagem"= n())
  # 
  mpi_sumarizado <- mpi_sumarizado %>% filter(contagem > 10)
  #
  ggplot(mpi_sumarizado, aes(x = mpi_sumarizado$sala, y = mpi_sumarizado$contagem, fill = sala, label = mpi_sumarizado[[6]])) +
    geom_bar(stat = "Identity") +
    labs(y = "Qtde de Bens", x = "Sala Destino") +
    ggtitle(paste(meses[indice]," de ", mpi_sumarizado[[1]][1])) + theme_linedraw(base_size = 16) +
    theme(axis.text.x = element_text(angle = 90, size = 8), plot.title = element_text(hjust = 0.5), 
          panel.grid.major = element_line(colour = "grey50"), legend.position = "none") +
    geom_label()
  
  ggsave(paste("C:/Users/Marcio/Dropbox (Pessoal)/TCC_PUCMinas/PUC_Minas/images/myplot_RAW",indice, ".png"))
  # 
}
# 
cat(dev.off())
# 
sum(mpi_sumarizado$contagem)
# 
# BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO 
# BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO BACKUP DO CÓDIGO 
# 


# 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# 
# Realizar uma análise das movimentações por período (de grandes valores)
#
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
#

#
##################################################################################################
#
# Sumarizando por ano
#
##################################################################################################
# 
mpiSumAno <- mpi %>% group_by(ano = year(dataMovimentacao)) %>% tally(name = "movimentacoes") # ,sort = TRUE
# 
piepercent<- round(100 * mpiSumAno$movimentacoes/sum(mpiSumAno$movimentacoes), 1)
# 
pie(mpiSumAno$movimentacoes, labels = piepercent, main="Movimentações (% por Ano)", col = rainbow(4))
legend("topright",legend = mpiSumAno[[1]], cex = 0.8,
       fill = rainbow(4))
#
# Gráfico de Barras
#
mpiSumAno %>% ggplot(aes(x = ano, y = movimentacoes, fill = ano)) +
  geom_bar(stat = "Identity") +
  labs(y = "Qtde de Movimentações", x = "Ano") +
  ggtitle("Movimentações por Ano") + theme_linedraw(base_size = 16)
#
##################################################################################################
#
# Sumarizando por ano (Por dois ou mais responsáveis ou salas) - Filtrando
#
##################################################################################################
# 
mpiSumAno <- mpi %>% group_by(ano = year(dataMovimentacao)) %>% tally(name = "movimentacoes") # ,sort = TRUE
# 
piepercent<- round(100 * mpiSumAno$movimentacoes/sum(mpiSumAno$movimentacoes), 1)
# 
pie(mpiSumAno$movimentacoes, labels = piepercent, main="Movimentações (% por Ano)", col = rainbow(4))
legend("topright",legend = mpiSumAno[[1]], cex = 0.8,
       fill = rainbow(4))
#
# Gráfico de Barras
#
mpiSumAno %>% ggplot(aes(x = ano, y = movimentacoes, fill = ano)) +
  geom_bar(stat = "Identity") +
  labs(y = "Qtde de Movimentações", x = "Ano") +
  ggtitle("Movimentações por Ano") + theme_linedraw(base_size = 16)
# 
##################################################################################################
# 
# "inventario" == !NA: movimentação automática de grande quantidade de bens 
#                      para ajuste de localidade de inventário/levantamento patrimonial
#                      
################################################################################################## 
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
# Leitura do arquivo geral de bens (patrimonios) para realizar a junção (left_join) dos arquivos (Recuperar a Descrição) 
# 
bensPatrimoniais <- read.csv(".\\data-raw\\BensPorGrupo.csv", sep = ";", encoding = "UTF-8", stringsAsFactors = FALSE)
# 
bensPatrimoniais <- mutate_at(bensPatrimoniais, vars("tombamento"), as.integer)
# 
# left_join(table_1, table_2, by = c("ID_1" = "ID_2"))
# 
# mpi_select <- mpi %>% left_join(bensPatrimoniais, by = c("tombamento"))  # Retorna todos os campos da tabela de bens patrimoniais
#
# Inserindo a descrição do bem patrimonial a partir do arquivo geral de bens
# 
mpi <- mpi%>% mutate(descricaoBem = bensPatrimoniais$descricao[match(tombamento, bensPatrimoniais$tombamento)])
#
##################################################################################################
#
# Gravando um arquivo com a descrição dos bens para elaborar uma Nuvem de Palavras
# 
##################################################################################################
#
write.table(mpi$descricaoBem, file = "./data/ml.txt", sep = "\t",
            row.names = FALSE, col.names = FALSE)
#
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
                       (responsavel == "Daniela Heitor de Moura" |
                          responsavel == "Levy Carlos Caixeta de Sa" |
                          responsavel == "Marcio Guillardi da Silva")))
#
# (15.139 Registros / 1.537 Registros ignorados)
#
# mpi_teste <- mpi[which(!(mpi$tombamento >= 37827 &
#                             mpi$tombamento <= 38623 &
#                             (mpi$responsavel == "Levy Carlos Caixeta de Sa" |
#                              mpi$responsavel == "Daniela Heitor de Moura" |
#                              mpi$responsavel == "Marcio Guillardi da Silva"))
#                         ),]
#
#
# Aquisição e incorporação de painéis divisórios individuais (baias) para todas as salas...
# ... esses bens já existiam e foram incorporados aos bens patrimoniais da PGT e transferidos
# para os gestores responsáveis pelas salas/setor/diretorias.
# 
# mpi_teste <- mpi %>% filter(grepl("DIVIS", descricaoBem) & (grepl("Ricardo Vaz", responsavel) | grepl("Levy", responsavel)))
# 
mpi <- filter(mpi, !(tombamento >= 36843 &        # Dentro da faixa... 36843 ~ 37817
                       tombamento <= 37817 &
                       cedente == "Levy Carlos Caixeta de Sa"))
#
# (14247 Registros / 892 Registros ignorados )
#
#
# Aquisição de livros para a Biblioteca. As transferências foram efetivadas pelos gestores do Setor de Patrimônio.
# 
# Selecionando aquelas MPIs que movimentaram mais de 10 itens, aumentando a margem de confiança nos dados. 
# 
mpi_count <- mpi %>% filter(responsavel == "Vanessa Christina Alves Fernandes" & nivel1 == "BIBLIOTECA LIVROS - CDI" &
                              (cedente == "Levy Carlos Caixeta de Sa" | 
                                 cedente == "Ricardo Vaz Gomes Bastos" | 
                                 cedente == "Andrea Regina da Silva Diana")) %>%
  count(id, name = "total", sort = TRUE) %>% filter(total > 10) %>% select(id)
# 
#  mpi_count <- mpi_teste[[1]]
#  mpi_teste <- mpi %>% filter(!(id %in% mpi_count)) # vector
# 
mpi <- mpi %>% filter(!(id %in% mpi_count$id)) # atomic vector
# 
# view(mpi_teste_ <- mpi %>% filter(id=="66138"))
# 
# 68819   310
# 48530   140
# 49798   127
# 34510   101
# 30358    58
# 28094    57
# 11746    50
# 11747    45
# 37830    30
# 37728    24
# 34506    22
# 66138    22
# 36396    16
# 38790     1
# 70247     1
# 
# mpi_teste <- mpi %>% filter(id %in% c(68819, 34510, 30358, 28094, 11746,11747,37830,37728,34506,66138,36396,49875))
#
#
# 
# Filtrando as movimentações que tenham mais de 9 itens movimentados...
# ... em seguida verificando quais as movimentais possuem números patrimoniais em sequencia...
# ... isso implica em dizer que os patrimonios, normalmente, foram transferidos (movimentados)
# após sua incorporação (tombamento) ou, de outra forma, são bens transferidos de gestor/responsável.
# 
mpi_total <- mpi %>% count(id, name = "total", sort = TRUE) %>% filter(total > 9) # %>% select(id)
# 
# sum(mpi_total$total)
# [1] 7212 (movimentações)
#
# Copiando a estrutura do dataframe para um dataframe em branco (mesmo número de colunas)
#
mpi_blank = data.frame(mpi[0,])
# 
for (mpiId in mpi_total$id) {
  # 
  mpi_pass <- mpi %>% filter(mpi$id == mpiId) %>% select(id, tombamento) %>% arrange(tombamento)
  # 
  mpi_pass <- mutate_at(mpi_pass, vars("tombamento"), as.integer)
  # 
  # SE a contagem de registros de um mesmo ID (de uma MPI) for igual 
  # a soma das differenças entre os números patrimoniais (1), significará
  # que os números patrimoniais estarão em sequencia.
  # 
  if (count(mpi_pass) == sum(diff(mpi_pass[[2]]))+1) {
    #
    # Cria um dataframe para verificação/visualização
    #
    mpi_blank <- rbind(mpi_blank, filter(mpi, id == mpiId))
    # 
    # Se os tombamentos (números patrimoniais) estiverem em sequência serão ignorados
    # 
    mpi <- mpi %>% filter(!id == mpiId)
  }
}
#  
# [1] 384 (Movimentações filtradas/excluídas)
#
# Exemplo do Funcionamento do código acima:
# 
# c1 <- c(201:210)
# c2 <- c(11:20)
# 
# df <- data.frame(c1, c2)
# 
# count(df) == sum(diff(df[[2]]))+1       # Retorna TRUE, pois a quantidade de registros e igual a soma das diferenças entre os números patrimoniais
# 
# count(df):
# 
# A tibble: 1 x 1
# n
#    <int>
# 1    10
# 
# sum(diff(df[[2]]))+1
# 
# [1] 10
#
# 
##################################################################################################
#
# Movimentações de bens diversos para tombamento e distribuição (envio e recolhimento)
#
##################################################################################################
#
# mpi_bak <- mpi_bak %>% filter(id %in% c(56374, 72982, 18255, 45637))
#
mpi <- mpi %>% filter(!(id %in% c(56374, 72982, 18255, 45637)))
# 
# Identificando grandes movimentações (MPIs)
# 
# mpiCountId_ <- mpi %>% count(id, name = "total", sort = TRUE)
#                                                                * INDICA AS MOVIMENTAÇÕES QUE DEVERÃO SER IGNORADAS
# view(mpi_teste <- mpi %>% filter(id=="73887")) # 419 Registros - SL. Nº 101B - DATACENTER (Mudança de Responsabilidade - MR)
# view(mpi_teste <- mpi %>% filter(id=="56374")) # 300 Registros * Fornecimento de WEBCAM LOGITECH, C525 (INCORPORAÇÃO)
# view(mpi_teste <- mpi %>% filter(id=="72982")) # 232 Registros * Recolhimento de WEBCAM LOGITECH, C525 (INCORPORAÇÃO)
# view(mpi_teste <- mpi %>% filter(id=="37202")) # 207 Registros - SL. Nº 603A	CH. DP. INFRAESTRUTURA (MR)
# view(mpi_teste <- mpi %>% filter(id=="12740")) # 168 Registros - SL. Nº 202SS	SALA DE MULTIPLO USO - GSI (MR)
# view(mpi_teste <- mpi %>% filter(id=="69689")) # 159 Registros - SL. Nº 901	DIR. DAE - ARQUITETURA E ENGENHARIA (MR)
# view(mpi_teste <- mpi %>% filter(id=="15293")) # 141 Registros - SL. Nº 203	AUDITORIO - DA (MR)
# view(mpi_teste <- mpi %>% filter(id=="72408")) # 126 Registros - SL. Nº 203	AUDITORIO - DA (MR)
# view(mpi_teste <- mpi %>% filter(id=="18255")) # 120 Registros * SL. Nº 1508C	CHEFE - GSI	- PISTOLAS AUTOMÁTICAS (INCORPORAÇÃO)
# view(mpi_teste <- mpi %>% filter(id=="45637")) # 120 Registros * SL. Nº 203SS	APOIO DA - DL - CADEIRAS RODÍZIO (INCORPORAÇÃO)
# view(mpi_teste <- mpi %>% filter(id=="47406")) # 115 Registros - SL. Nº 504F SEÇÃO DE TREINAMENTO CODEP/DRH (MR)
# view(mpi_teste <- mpi %>% filter(id=="70580")) # 115 Registros - SL. Nº 1703	ASSESSORIA DE COMUNICAÇÃO SOCIAL (MR)
# view(mpi_teste <- mpi %>% filter(id=="81660")) # 114 Registros - SL. Nº 803	DEP. DOCUMENTAÇÃO E GESTÃO DA INFORMAÇÃO (MR)
# view(mpi_teste <- mpi %>% filter(id=="48486")) # 108 Registros - SL. Nº 002	SEÇÃO DE TRANSPORTE - GSI (MR)
# view(mpi_teste <- mpi %>% filter(id=="47634")) # 105 Registros - SL. Nº 002	SEÇÃO DE TRANSPORTE - GSI (MR)
# view(mpi_teste <- mpi %>% filter(id=="27104")) # 103 Registros - SL. Nº 002	SEÇÃO DE TRANSPORTE - GSI (MR)
# 
# view(mpi_teste <- mpi %>% filter(id %in% c(56374, 72982, 18255, 45637))) # 103 Registros - SL. Nº 002	SEÇÃO DE TRANSPORTE - GSI (MR)
# view(mpi_teste <- mpi %>% filter(id == 45637)) # 103 Registros - SL. Nº 002	SEÇÃO DE TRANSPORTE - GSI (MR)
# 
# (* Movimentações para incorporação de bens)
# 
# 
# mpi_sumarizado_responsavel <- mpi %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# (351 Observações)
# 
# ggplot(data = mpi_sumarizado_responsavel) + 
#   geom_point(mapping = aes(x = responsavel, y = movimentacoes)) + theme(axis.text.x=element_blank(),
#                                                                         axis.ticks.x=element_blank()) +
#   labs(y = "Quantidade de Movimentaçõs", x = "Responsável (Destino das Movimentações)")
# 
# mpi_sumarizado_responsavel %>% filter(movimentacoes > 100) %>%
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
# mpi <- mutate_at(mpi, vars("sala"), as.factor); str(mpi); mpi <- mutate_at(mpi, vars("sala"), as.character)
# 
# sala: Factor w/ 368 levels 
# 
# Transformando campos data...
# 
# mpi = dmy(mpi$data)
# mpi$dataConfirmacaoRecebimento = dmy_hm(mpi$dataConfirmacaoRecebimento)
#
# 
# mpi_sumarizado_responsavel <- mpi %>% group_by(responsavel) %>% tally(name = "movimentacoes")
# 
# Gravando arquivo sumarizado para Análise etc (CSV) para o Power BI / Tableau
# 
# write.csv(mpi_sumarizado_responsavel, file = ".\\data\\mpi_Sumarizado.csv", fileEncoding = "UTF-8")
# write.csv(mpi, file = "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT.csv", fileEncoding = "UTF-8")
# 
# Pacote library(readr)
# 
# write_excel_csv2(mpi, "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT.csv", na = "NA", append = FALSE, delim = ";", quote_escape = "double")
# 
# write_csv(mpi, "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT.csv")
#
# 
# ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK 
# ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK  ATE AQUI TUDO OK 
#

# https://sites.icmc.usp.br/ehlers/stemp/praticas/node1.html


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






library(tseries)
library(gdata)
z = read.xlsx("C:\\Users\\Marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data-raw\\atmosfera.xls",1)
dim(z)

z[1:10,]

x=as.ts(z[,2:3])

plot(x)

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
str(sprintf_) <- sprintf("%s teste de concatenação de %s coisas", "Um", "muitas")
# 
mpi_teste <- mpi %>% mutate(nivelSuperior = str_replace_all(nivelSuperior,"SANITÁRIOS, VESTIÁRIOS E COPA","SANITÁRIOS E COPA"))
# 
mpi_teste <- mpi$cedente[mpi$id == "14320"]
# 
# chr [1:2] "Andre Luis Itacarambi Rego" "Andre Luis Itacarambi Rego"
# 
mpi_teste <- mpi[which(mpi$id == "14320"),"cedente"]
# 
# chr [1:2] "Andre Luis Itacarambi Rego" "Andre Luis Itacarambi Rego"
# 
mpi_teste <- mpi[which(mpi$id == "14320"),]
# 
# 'data.frame':	2 obs. of  16 variables:
# $ id                        : int  14320 14320
# $ responsavel               : chr  "Adriana Cristina da Silva" "Adriana Cristina da Silva"
# $ cedente                   : chr  "Andre Luis Itacarambi Rego" "Andre Luis Itacarambi Rego"
# $ sala                      : chr  "SL. Nº 1507A" "SL. Nº 1507A"
# $ nivel1                    : chr  "COORDENADOR DA ASSESSORIA DE PLANEJAMENTO E G" "COORDENADOR DA ASSESSORIA DE PLANEJAMENTO E G"
# $ nivel2                    : chr  "SALA Nº 1507A" "SALA Nº 1507A"
# $ nivel3                    : chr  "COORDENADOR DA ASSESSORIA DE PLANEJAMENTO E G" "COORDENADOR DA ASSESSORIA DE PLANEJAMENTO E G"
# $ nivelSuperior             : chr  "CNC, 15º ANDAR" "CNC, 15º ANDAR"
# $ tombamento                : int  22102 23428
# $ dataMovimentacao          : chr  "05/12/2017" "05/12/2017"
# $ interna                   : chr  "S" "S"
# $ retorno                   : chr  "N" "N"
# $ inventario                : int  NA NA
# $ responsavelCadastro       : chr  "Ricardo Vaz Gomes Bastos" "Ricardo Vaz Gomes Bastos"
# $ dataConfirmacaoRecebimento: chr  "" ""
# $ descricaoBem              : chr  "MONITOR DE VÍDEO LCD" "MONITOR DE VÍDEO"
#
# 
x <- "ED. CNC, 08º ANDAR"
gsub("^(ED\\. CNC\\, )", "CNC, \\2", x)
# 
# [1] "CNC, 08º ANDAR"
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
x <-  gregexpr(pattern ='2',"thequickbrownfoxeswe2retired")
# 
if_else(x[[1]][1] > 0, "Encontrado", "Não Encontrado")
# 
#
# OPERAÇÕES E FILTROS COM DATAS
# 
mpi_teste <- mpi %>% filter(month(data) > 5 & year(data) == 20)
# 
mpi_teste <- mpi %>% filter(month(dataMovimentacao) %in% c(2,4,6))
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
# ************** MAIS SOBRE R **********************
