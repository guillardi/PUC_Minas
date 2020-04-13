

df <- data.frame(sala = c("SL. Nº 1208,ASS GAB DRA HELOÍSA MARIA MORAES,RÊGO PIRES,4º OFICIO", 
                          "SL. Nº 1108A,GAB DRA IVANA AUXILIADORA, MENDONÇA SANTOS, 12º OFICIO" ,
                          "SL. Nº 1301, ASS GAB DR JOSE NETO,DA SILVA, 14º OFICIO"))


df <- df %>% separate(sala, c("sala", "nivel1","nivel2", "nivel3"), sep = "(\\,)[\\s]*")

df <- df %>% separate(sala, c("sala", "nivel1","nivel2", "nivel3"), sep = "\\,\\s")

df

mpiSumResponsavel %>% 
  ggplot(aes(x = bensMovimentados)) +
  geom_density()

grepl("((\\,)[\\s]*)", "SL. Nº 1208,ASS GAB DRA HELOÍSA MARIA MORAES RÊGO PIRES - 4º OFICIO")


gsub("((\\,)[\\s]*)", "+++\\2", "SL. Nº 1208,ASS GAB DRA HELOÍSA, MARIA MORAES RÊGO PIRES,4º OFICIO")


gsub("^(SL\\.)([^[:space:]])","\\1 Nº \\2", sala)


str(nIdades) <- data.frame(idades = c(20,25,30,35,40,60,50,70))

sIdades <- cut(nIdades$idades, c(0,30,50,100), labels = c("Jovem", "Adulto", "Idoso"))

install.packages("Amelia")

library(Amelia)

AmeliaView()



prop <- prop.table(table(mpi$sala))


write_excel_csv2(mpi, "C:\\Users\\marcio\\Dropbox (Pessoal)\\TCC_PUCMinas\\PUC_Minas\\data\\movimentacoesPGT.csv", na = "NA", append = FALSE, delim = ";", quote_escape = "double")

# https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/

data(AirPassengers)
class(AirPassengers)
AirPassengers
