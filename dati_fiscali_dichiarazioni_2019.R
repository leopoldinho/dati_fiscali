#Libraries
library(devtools) 
library(tidyverse)
library(googlesheets4)

download.file("https://raw.githubusercontent.com/leopoldinho/dati_fiscali/main/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2018.csv", "dichiarazioni_redditi_2019.csv") # download the file with the data

Dichiarazioni_2019 <- read.csv2("dichiarazioni_redditi_2019.csv")

Dichiarazioni_2019 <- Dichiarazioni_2019 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat.Comune,Regione, Comune=Denominazione.Comune, Contribuenti=Numero.contribuenti, Imponibile=Reddito.imponibile...Ammontare.in.euro, "Imponibile pro capite"=reddito_medio_dichiarato)


Dichiarazioni_2019_Liguria <- Dichiarazioni_2019 %>% 
  filter(Regione=="Liguria")



Dichiarazioni_2020 <- read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2019.csv")

Dichiarazioni_2020 <- Dichiarazioni_2020 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat.Comune,Regione,Sigla.Provincia, Comune=Denominazione.Comune, Contribuenti=Numero.contribuenti, Imponibile=Reddito.imponibile...Ammontare.in.euro, "Imponibile pro capite"=reddito_medio_dichiarato)


Dichiarazioni_2020_Liguria <- Dichiarazioni_2020 %>% 
  filter(Regione=="Liguria") %>% 
  filter(Sigla.Provincia=="SP")

write.csv2(Dichiarazioni_2020_Liguria, "prova_grafico.csv")