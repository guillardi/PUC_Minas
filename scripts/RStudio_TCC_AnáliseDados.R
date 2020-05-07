#
# CÓDIGOS TEMPORÁRIOS PARA SEREM COPIADOS PARA O ARQUIVO PRINCIPAL (RStudio_TCC_Tibbles.R)
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
# Contando as movimentações patrimoniais (total de bens movimentados por MPI por ANO e MÊS)
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




