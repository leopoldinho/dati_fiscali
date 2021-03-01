#Libraries
library(devtools) 
library(tidyverse)
library(googlesheets4)

download.file("https://raw.githubusercontent.com/leopoldinho/dati_fiscali/main/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2018.csv", "dichiarazioni_redditi_2019.csv") # download the file with the data

Dichiarazioni_2019 <- read.csv2("dichiarazioni_redditi_2019.csv", encoding="UTF-8") 

Dichiarazioni_2019 <- Dichiarazioni_2019 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat.Comune,Regione, Denominazione.Comune=Comune, Numero.contribuenti=Contribuenti, Reddito.imponibile...Ammontare.in.euro=Imponibile, reddito_medio_dichiarato="Imponibile pro capite")
  