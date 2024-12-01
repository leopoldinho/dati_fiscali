#Libraries
library(devtools) 
library(tidyverse)
library(googlesheets4)
library(sf)
library(viridis)
library(showtext)

dir.create("dati_fiscali")
setwd("dati_fiscali")

#2023#

Dichiarazioni_2023_ <- read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2022.csv", sep=";")

Dichiarazioni_2023 <- Dichiarazioni_2023_ %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro+Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)

write.csv2(Dichiarazioni_2023, "Comuni_irpef_2023_b.csv")


summary_dichiarazioni_2023 = Dichiarazioni_2023_ %>%
  group_by(Anno.di.imposta) %>%
  summarise(Reddito_medio=sum(Reddito.imponibile...Ammontare.in.euro)/ sum(Numero.contribuenti))

Dichiarazioni_2023_Liguria <- Dichiarazioni_2023 %>% 
  filter(Regione=="Liguria")

write.csv(Dichiarazioni_2023_Liguria, "Comuni_irpef_2023_Liguria.csv")


#2022#

Dichiarazioni_2022_ <- read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2021.csv", sep=";")

Dichiarazioni_2022 <- Dichiarazioni_2022_ %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro+Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)

write.csv2(Dichiarazioni_2022, "Comuni_irpef_2022_b.csv")


summary_dichiarazioni_2022 = Dichiarazioni_2022_ %>%
  group_by(Anno.di.imposta) %>%
  summarise(Reddito_medio=sum(Reddito.imponibile...Ammontare.in.euro)/ sum(Numero.contribuenti))

Dichiarazioni_2022_Liguria <- Dichiarazioni_2022 %>% 
  filter(Regione=="Liguria")

write.csv2(Dichiarazioni_2022_Liguria, "Comuni_irpef_2022_Liguria.csv")

Dichiarazioni_2022_Genova <- Dichiarazioni_2022 %>% 
  filter(Denominazione.Comune=="GENOVA")

Dichiarazioni_2022_frequenza <- Dichiarazioni_2022_ %>%
  filter(Denominazione.Comune=="GENOVA") %>%
  select(Anno.di.imposta, Reddito.complessivo.da.0.a.10000.euro...Frequenza,Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Reddito.complessivo.da.15000.a.26000.euro...Frequenza,
         Reddito.complessivo.da.26000.a.55000.euro...Frequenza,
         Reddito.complessivo.da.55000.a.75000.euro...Frequenza,
         Reddito.complessivo.da.75000.a.120000.euro...Frequenza,
         Reddito.complessivo.oltre.120000.euro...Frequenza) %>%
  pivot_longer(- Anno.di.imposta, names_to = "Oggetto") %>%
  select(-Anno.di.imposta) %>% rename(Contribuenti=value)

Dichiarazioni_2022_ammontare <- Dichiarazioni_2022_ %>%
  filter(Denominazione.Comune=="GENOVA") %>%
  select(Anno.di.imposta, Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.15000.a.26000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.26000.a.55000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.55000.a.75000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.75000.a.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro) %>%
  pivot_longer(- Anno.di.imposta, names_to = "Oggetto") %>%
  select(-Anno.di.imposta, -Oggetto) %>% rename(Reddito=value)

Dichiarazioni_2022_piramide <- bind_cols(Dichiarazioni_2022_frequenza,Dichiarazioni_2022_ammontare)

write.csv2(Dichiarazioni_2022_Genova, "genova_2021.csv")



#2021#

Dichiarazioni_2021_ <- read.csv2("https://raw.githubusercontent.com/leopoldinho/dati_fiscali/main/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2020.csv")

Dichiarazioni_2021 <- Dichiarazioni_2021_ %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro+Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)

write.csv2(Dichiarazioni_2021, "Comuni_irpef_2021_b.csv")


summary_dichiarazioni_2021 = Dichiarazioni_2021_ %>%
  group_by(Anno.di.imposta) %>%
  summarise(Reddito_medio=sum(Reddito.imponibile...Ammontare.in.euro)/ sum(Numero.contribuenti))

Dichiarazioni_2021_Liguria <- Dichiarazioni_2021 %>% 
  filter(Regione=="Liguria")

write.csv2(Dichiarazioni_2021_Liguria, "Comuni_irpef_2021_Liguria.csv")

Dichiarazioni_2021_Genova <- Dichiarazioni_2021 %>% 
  filter(Denominazione.Comune=="GENOVA")

Dichiarazioni_2021_frequenza <- Dichiarazioni_2021_ %>%
  filter(Denominazione.Comune=="GENOVA") %>%
  select(Anno.di.imposta, Reddito.complessivo.da.0.a.10000.euro...Frequenza,Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Reddito.complessivo.da.15000.a.26000.euro...Frequenza,
         Reddito.complessivo.da.26000.a.55000.euro...Frequenza,
         Reddito.complessivo.da.55000.a.75000.euro...Frequenza,
         Reddito.complessivo.da.75000.a.120000.euro...Frequenza,
         Reddito.complessivo.oltre.120000.euro...Frequenza) %>%
  pivot_longer(- Anno.di.imposta, names_to = "Oggetto") %>%
  select(-Anno.di.imposta) %>% rename(Contribuenti=value)

Dichiarazioni_2021_ammontare <- Dichiarazioni_2021_ %>%
  filter(Denominazione.Comune=="GENOVA") %>%
  select(Anno.di.imposta, Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.15000.a.26000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.26000.a.55000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.55000.a.75000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.75000.a.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro) %>%
  pivot_longer(- Anno.di.imposta, names_to = "Oggetto") %>%
  select(-Anno.di.imposta, -Oggetto) %>% rename(Reddito=value)

Dichiarazioni_2021_piramide <- bind_cols(Dichiarazioni_2021_frequenza,Dichiarazioni_2021_ammontare)

write.csv2(Dichiarazioni_2021_Genova, "genova_2020.csv")

#2020#

Dichiarazioni_2020_ <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2019.csv")

Dichiarazioni_2020 <- Dichiarazioni_2020_ %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro+Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2020_Liguria <- Dichiarazioni_2020 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2020_Genova <- Dichiarazioni_2020 %>% 
  filter(Denominazione.Comune=="GENOVA")

Dichiarazioni_2020_frequenza <- Dichiarazioni_2020_ %>%
  filter(Denominazione.Comune=="GENOVA") %>%
  select(Anno.di.imposta, Reddito.complessivo.da.0.a.10000.euro...Frequenza,Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Reddito.complessivo.da.15000.a.26000.euro...Frequenza,
         Reddito.complessivo.da.26000.a.55000.euro...Frequenza,
         Reddito.complessivo.da.55000.a.75000.euro...Frequenza,
         Reddito.complessivo.da.75000.a.120000.euro...Frequenza,
         Reddito.complessivo.oltre.120000.euro...Frequenza) %>%
  pivot_longer(- Anno.di.imposta, names_to = "Oggetto") %>%
  select(-Anno.di.imposta) %>% rename(Contribuenti=value)

Dichiarazioni_2020_ammontare <- Dichiarazioni_2020_ %>%
  filter(Denominazione.Comune=="GENOVA") %>%
  select(Anno.di.imposta, Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.15000.a.26000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.26000.a.55000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.55000.a.75000.euro...Ammontare.in.euro,
         Reddito.complessivo.da.75000.a.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro) %>%
  pivot_longer(- Anno.di.imposta, names_to = "Oggetto") %>%
  select(-Anno.di.imposta, -Oggetto) %>% rename(Reddito=value)

Dichiarazioni_2020_piramide <- bind_cols(Dichiarazioni_2020_frequenza,Dichiarazioni_2020_ammontare)


#2019#

Dichiarazioni_2019 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2018.csv")

Dichiarazioni_2019 <- Dichiarazioni_2019 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro+Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2019_Liguria <- Dichiarazioni_2019 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2019_Genova <- Dichiarazioni_2019 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2018#

Dichiarazioni_2018 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2017.csv")

Dichiarazioni_2018 <- Dichiarazioni_2018 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro+Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2018_Liguria <- Dichiarazioni_2018 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2018_Genova <- Dichiarazioni_2018 %>% 
  filter(Denominazione.Comune=="GENOVA")


#2017#

Dichiarazioni_2017 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2016.csv")

Dichiarazioni_2017 <- Dichiarazioni_2017 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro+Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2017_Liguria <- Dichiarazioni_2017 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2017_Genova <- Dichiarazioni_2017 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2016#

Dichiarazioni_2016 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2015.csv")

Dichiarazioni_2016 <- Dichiarazioni_2016 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare.in.euro+Reddito.complessivo.da.10000.a.15000.euro...Ammontare.in.euro, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2016_Liguria <- Dichiarazioni_2016 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2016_Genova <- Dichiarazioni_2016 %>% 
  filter(Denominazione.Comune=="GENOVA")


#2015#

Dichiarazioni_2015 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2014.csv")

Dichiarazioni_2015 <- Dichiarazioni_2015 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare+Reddito.complessivo.da.10000.a.15000.euro...Ammontare, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)

Dichiarazioni_2015_Liguria <- Dichiarazioni_2015 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2015_Genova <- Dichiarazioni_2015 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2014#

Dichiarazioni_2014 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2013.csv")

Dichiarazioni_2014 <- Dichiarazioni_2014 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare+Reddito.complessivo.da.10000.a.15000.euro...Ammontare, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2014_Liguria <- Dichiarazioni_2014 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2014_Genova <- Dichiarazioni_2014 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2013#

Dichiarazioni_2013 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2012.csv")

Dichiarazioni_2013 <- Dichiarazioni_2013 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare+Reddito.complessivo.da.10000.a.15000.euro...Ammontare, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2013_Liguria <- Dichiarazioni_2013 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2013_Genova <- Dichiarazioni_2013 %>% 
  filter(Denominazione.Comune=="GENOVA")


#2012#

Dichiarazioni_2012 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2011.csv")

Dichiarazioni_2012 <- Dichiarazioni_2012 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare+Reddito.complessivo.da.10000.a.15000.euro...Ammontare, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2012_Liguria <- Dichiarazioni_2012 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2012_Genova <- Dichiarazioni_2012 %>% 
  filter(Denominazione.Comune=="GENOVA")


#2011#

Dichiarazioni_2011 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2010.csv")

Dichiarazioni_2011 <- Dichiarazioni_2011 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare+Reddito.complessivo.da.10000.a.15000.euro...Ammontare, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2011_Liguria <- Dichiarazioni_2011 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2011_Genova <- Dichiarazioni_2011 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2010#

Dichiarazioni_2010 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2009.csv")

Dichiarazioni_2010 <- Dichiarazioni_2010 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare+Reddito.complessivo.da.10000.a.15000.euro...Ammontare, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2010_Liguria <- Dichiarazioni_2010 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2010_Genova <- Dichiarazioni_2010 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2009#

Dichiarazioni_2009 <- read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2008.csv")

Dichiarazioni_2009 <- Dichiarazioni_2009 %>%
  mutate(reddito_medio_dichiarato=
        Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare.in.euro*100,
         Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100, scaglione_redditi_bassi=Reddito.complessivo.da.0.a.10000.euro...Ammontare+Reddito.complessivo.da.10000.a.15000.euro...Ammontare, contribuenti.bassi=Reddito.complessivo.da.0.a.10000.euro...Frequenza+Reddito.complessivo.da.10000.a.15000.euro...Frequenza,
         Perc_redditi_bassi=scaglione_redditi_bassi/Reddito.imponibile...Ammontare.in.euro*100, Perc_contr_bassi=contribuenti.bassi/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max,scaglione_redditi_bassi,Perc_redditi_bassi,contribuenti.bassi,Perc_contr_bassi)


Dichiarazioni_2009_Liguria <- Dichiarazioni_2009 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2009_Genova <- Dichiarazioni_2009 %>% 
  filter(Denominazione.Comune=="GENOVA")

##Serie storica

Dichiaraz_genova_Serie_storica <- bind_rows(Dichiarazioni_2009_Genova,Dichiarazioni_2010_Genova,
                                            Dichiarazioni_2011_Genova,Dichiarazioni_2012_Genova, Dichiarazioni_2013_Genova,
                                            Dichiarazioni_2014_Genova,Dichiarazioni_2015_Genova,Dichiarazioni_2016_Genova,
                                            Dichiarazioni_2017_Genova,Dichiarazioni_2018_Genova,Dichiarazioni_2019_Genova,Dichiarazioni_2020_Genova)

#2020 CAp

Dichiarazioni_2020_cap <- read.csv2("https://raw.githubusercontent.com/leopoldinho/dati_fiscali/main/Redditi_e_principali_variabili_IRPEF_su_base_subcomunale_CSV_2019.csv", sep=";")

#Genova
Dichiarazioni_2020_Genova_cap <- Dichiarazioni_2020_cap %>% 
  filter(Denominazione.Comune=="GENOVA")

Dichiarazioni_2020_Genova_cap <- Dichiarazioni_2020_Genova_cap %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, 
         Perc_cont_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100,
         red_medio_cont_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.complessivo.oltre.120000.euro...Frequenza
         ) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(CAP, Contribuenti=Numero.contribuenti, Imponibile=Reddito.imponibile...Ammontare.in.euro, Contribuenti_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Frequenza, reddito_medio_dichiarato,Perc_cont_scaglione_alto,red_medio_cont_scaglione_alto)


#2021 CAp

Dichiarazioni_2021_cap <- read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_subcomunale_CSV_2020.csv")

#Genova
Dichiarazioni_2021_Genova_cap <- Dichiarazioni_2021_cap %>%
  filter(Denominazione.Comune=="GENOVA")

Dichiarazioni_2021_Genova_cap <- Dichiarazioni_2021_Genova_cap %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti,
         Perc_cont_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100,
         red_medio_cont_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.complessivo.oltre.120000.euro...Frequenza
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(CAP, Contribuenti=Numero.contribuenti, Imponibile=Reddito.imponibile...Ammontare.in.euro, Contribuenti_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Frequenza, reddito_medio_dichiarato,Perc_cont_scaglione_alto,red_medio_cont_scaglione_alto)


#2022 Cap

Dichiarazioni_2022_cap_ <-
  read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_subcomunale_CSV_2021.csv",
            sep = ";")

Dichiarazioni_2022_cap <- Dichiarazioni_2022_cap_ %>%
  mutate(
    reddito_medio_dichiarato =
      Reddito.imponibile...Ammontare.in.euro / Numero.contribuenti,
    Perc_cont_scaglione_alto = Reddito.complessivo.oltre.120000.euro...Frequenza /
      Numero.contribuenti * 100,
    red_medio_cont_scaglione_alto = Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro /
      Reddito.complessivo.oltre.120000.euro...Frequenza
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(
    Denominazione.Comune,
    CAP,
    Contribuenti = Numero.contribuenti,
    Imponibile = Reddito.imponibile...Ammontare.in.euro,
    Contribuenti_scaglione_alto = Reddito.complessivo.oltre.120000.euro...Frequenza,
    reddito_medio_dichiarato,
    Perc_cont_scaglione_alto,
    red_medio_cont_scaglione_alto
  )


Dichiarazioni_2022_Genova_cap = Dichiarazioni_2022_cap %>%
  filter(Denominazione.Comune=="GENOVA")

write.csv2(Dichiarazioni_2022_Genova_cap, "Irpef_2021_GE.csv")

Dichiarazioni_2022_Roma_cap = Dichiarazioni_2022_cap %>%
  filter(Denominazione.Comune=="ROMA")

write.csv2(Dichiarazioni_2022_Roma_cap, "Irpef_2021_RM.csv")

Dichiarazioni_2022_Milano_cap = Dichiarazioni_2022_cap %>%
  filter(Denominazione.Comune=="MILANO")

write.csv2(Dichiarazioni_2022_Milano_cap, "Irpef_2021_MI.csv")

Dichiarazioni_2022_Torino_cap = Dichiarazioni_2022_cap %>%
  filter(Denominazione.Comune=="TORINO")

write.csv2(Dichiarazioni_2022_Torino_cap, "Irpef_2021_To.csv")

Dichiarazioni_2022_Napoli_cap = Dichiarazioni_2022_cap %>%
  filter(Denominazione.Comune=="NAPOLI")

write.csv2(Dichiarazioni_2022_Napoli_cap, "Irpef_2021_NA.csv")

write.csv(Dichiarazioni_2022_cap, "Irpef_2021_tot.csv")

# Cap 2023

Dichiarazioni_2023_cap_ <-
  read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_subcomunale_CSV_2022.csv",
            sep = ",")

Dichiarazioni_2023_cap <- Dichiarazioni_2023_cap_ %>%
  mutate(
    reddito_medio_dichiarato =
      Reddito.imponibile...Ammontare.in.euro / Numero.contribuenti,
    Perc_cont_scaglione_alto = Reddito.complessivo.oltre.120000.euro...Frequenza /
      Numero.contribuenti * 100,
    red_medio_cont_scaglione_alto = Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro /
      Reddito.complessivo.oltre.120000.euro...Frequenza
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(
    Denominazione.Comune,
    CAP,
    Contribuenti = Numero.contribuenti,
    Imponibile = Reddito.imponibile...Ammontare.in.euro,
    Contribuenti_scaglione_alto = Reddito.complessivo.oltre.120000.euro...Frequenza,
    reddito_medio_dichiarato,
    Perc_cont_scaglione_alto,
    red_medio_cont_scaglione_alto
  )


Dichiarazioni_2023_Genova_cap = Dichiarazioni_2023_cap %>%
  filter(Denominazione.Comune=="GENOVA")

write.csv2(Dichiarazioni_2023_Genova_cap, "Irpef_2022_GE.csv")

Dichiarazioni_2023_Spezia_cap = Dichiarazioni_2023_cap %>%
  filter(Denominazione.Comune=="LA SPEZIA")

write.csv2(Dichiarazioni_2023_Spezia_cap, "Irpef_2022_SP.csv")


Dichiarazioni_2023_Roma_cap = Dichiarazioni_2023_cap %>%
  filter(Denominazione.Comune=="ROMA")

write.csv2(Dichiarazioni_2023_Roma_cap, "Irpef_2022_RM.csv")

Dichiarazioni_2023_Milano_cap = Dichiarazioni_2023_cap %>%
  filter(Denominazione.Comune=="MILANO")

write.csv2(Dichiarazioni_2023_Milano_cap, "Irpef_2022_MI.csv")

Dichiarazioni_2023_Torino_cap = Dichiarazioni_2023_cap %>%
  filter(Denominazione.Comune=="TORINO")

write.csv2(Dichiarazioni_2023_Torino_cap, "Irpef_2022_To.csv")

Dichiarazioni_2023_Napoli_cap = Dichiarazioni_2023_cap %>%
  filter(Denominazione.Comune=="NAPOLI")

write.csv2(Dichiarazioni_2023_Napoli_cap, "Irpef_2022_NA.csv")


Dichiarazioni_2023_Firenze_cap = Dichiarazioni_2023_cap %>%
  filter(Denominazione.Comune=="FIRENZE")

write.csv2(Dichiarazioni_2023_Firenze_cap, "Irpef_2022_FI.csv")


Dichiarazioni_2023_Bologna_cap = Dichiarazioni_2023_cap %>%
  filter(Denominazione.Comune=="BOLOGNA")

write.csv2(Dichiarazioni_2023_Bologna_cap, "Irpef_2022_BO.csv")


write.csv(Dichiarazioni_2023_cap, "Irpef_2022_cap.csv")





#Confronti 2019-2020

Dichiarazioni_2020_Genova_cap_tmp <- Dichiarazioni_2020_Genova_cap %>%select(CAP, reddito_medio_dichiarato_2019=reddito_medio_dichiarato)

Confronti_19_20_genova <- left_join(Dichiarazioni_2021_Genova_cap,Dichiarazioni_2020_Genova_cap_tmp, 
                                    by="CAP") %>%
  mutate(diff=reddito_medio_dichiarato-reddito_medio_dichiarato_2019, 
         diff_perc=diff/reddito_medio_dichiarato_2019*100)

write.csv(Confronti_19_20_genova, "redditi_confronto.csv")

#Milano
Dichiarazioni_2021_Milano_cap <- Dichiarazioni_2021_cap %>%
  filter(Denominazione.Comune=="MILANO")

Dichiarazioni_2021_Milano_cap <- Dichiarazioni_2021_Milano_cap %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti,
         Perc_cont_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100,
         red_medio_cont_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.complessivo.oltre.120000.euro...Frequenza
  ) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(CAP, Contribuenti=Numero.contribuenti, Imponibile=Reddito.imponibile...Ammontare.in.euro, Contribuenti_scaglione_alto=Reddito.complessivo.oltre.120000.euro...Frequenza, reddito_medio_dichiarato,Perc_cont_scaglione_alto,red_medio_cont_scaglione_alto)


write.csv2(Dichiarazioni_2020_piramide, "prova_grafico.csv")


#VEcchio
download.file("https://raw.githubusercontent.com/leopoldinho/dati_fiscali/main/Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2018.csv", "dichiarazioni_redditi_2019.csv") # download the file with the data

Dichiarazioni_2019 <- read.csv2("dichiarazioni_redditi_2019.csv")

Dichiarazioni_2019 <- Dichiarazioni_2019 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat.Comune,Regione, Comune=Denominazione.Comune, Contribuenti=Numero.contribuenti, Imponibile=Reddito.imponibile...Ammontare.in.euro, "Imponibile pro capite"=reddito_medio_dichiarato)


Dichiarazioni_2019_Liguria <- Dichiarazioni_2019 %>% 
  filter(Regione=="Liguria")

#Affluenza europee 2024

affluenza_euro_24 = read.csv("affluenza _new.csv")

Dichiarazioni_2023_solo_reddito = Dichiarazioni_2023 %>%
  select(Codice.Istat, Regione, Contribuenti, "Imponibile pro capite")

affluenza_euro_24_redditi = left_join(
  affluenza_euro_24,
  Dichiarazioni_2023_solo_reddito,
  by = c("CODICE.ISTAT" = "Codice.Istat")
) %>%
  mutate(
    diff_voti = vot_m - vot_f,
    perc_m = vot_m / ele_m * 100,
    perc_f = vot_f / ele_f * 100,
    diff_per=perc_m-perc_f,
    perc_voto_f=vot_f/vot_t*100
  )%>%
  mutate_if(is.numeric, round, 2)


write.csv(affluenza_euro_24_redditi, "affluenza_euro_24_redditi.csv")

affluenza_euro_24_redditi_1000 = affluenza_euro_24_redditi %>%
  filter(Contribuenti > 1000)

write.csv(affluenza_euro_24_redditi_1000,
          "affluenza_euro_24_redditi-1000.csv")


#voti europee 2024

risultati_euro_24_ = read.csv("Europee 2019 e 2024 - Euro 2024 mod.csv")

risultati_euro_24_red = left_join(
  risultati_euro_24_,
  Dichiarazioni_2023_solo_reddito,
  by = c("CODICE.ISTAT" = "Codice.Istat")
)

affluenza_euro_24_redditi_solo_donne = affluenza_euro_24_redditi %>%
  select(desc, CODICE.ISTAT, perc_voto_f)

risultati_euro_24_red_donne = left_join(risultati_euro_24_red,
                                        affluenza_euro_24_redditi_solo_donne,
                                        by = "CODICE.ISTAT")

write.csv(risultati_euro_24_red_donne,
          "risultati_euro_24_red_donne.csv")





#MAPPA REDDITI COMUNE 

#Scarico dati geografici e unisco a dati redditi

Italia_comuni <- st_read("Confini_Comuni_Istat_2022.json")

Italia_comuni_redditi_geo = left_join(Italia_comuni, Dichiarazioni_2021, by=c("PRO_COM"="Cod_com"))%>%
  rename(Imponibile_procapite="Imponibile pro capite 2020")

# check with map

Italia_comuni_redditi_geo |>
  ggplot() +
  geom_sf()


#simple choromap

simple_choro_map <- Italia_comuni_redditi_geo %>% 
  ggplot(aes(fill = Imponibile_procapite)) + # create a ggplot object and 
  # change its fill colour according to median_age
  geom_sf() # plot all local authority geometries

simple_choro_map

#font

font_add("SkyText", "Sky.ttf")
showtext_auto()
font = "SkyText"

#prettier choromap
pretty_choro_map <- Italia_comuni_redditi_geo %>%
  ggplot(aes(fill = Imponibile_procapite))+ 
         geom_sf(colour = NA) + # Adding 'colour = NA' removes boundaries
           #around each comune (lasciare solo geom_Sf se voglio far vedere i confini dei comuni)
           
           #Opzioni di scale
           
           scale_fill_viridis("Imponibile_procapite",option="magma") +
           
           #scale_fill_met_c(
           #  "Hokusai2",
            # override.order = TRUE,
             #breaks = c(10000, 20000, 30000, 40000)) +
           
           #Apparato testuale
           
           labs(title = "Redditi dichiarati, quel divario tra nord e sud",
                subtitle = "Il reddito imponibile medio di ciascun comune nel 2021",
                caption = "Fonte: Elaborazione Sky TG24 su dati Ministero delle Finanze ") +
    guides(fill=guide_legend(
    direction = "horizontal",
    keyheight = unit(1.15, units = "mm"),
    keywidth = unit(15, units = "mm"),
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = .5,
    nrow = 1,
    byrow = T,
    reverse = F,
    label.position = "bottom"
  ))+
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.background = element_blank(),
        legend.position = "top",
        panel.border = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "white", size = 0),
        plot.title = element_text(size=18, color="grey60", hjust=0, vjust=0,family = font, face="bold"),
        plot.subtitle = element_text(size=14, color="grey60", hjust=0, vjust=0,family = font),
        plot.caption = element_text(size=8, color="grey60", hjust=0, vjust=0,family = font),
        axis.title.x = element_text(size=7, color="grey60", hjust=0, vjust=5,family = font),
        legend.text = element_text(size=8, color="grey20"),
        legend.title = element_blank(),
        strip.text = element_text(size=12),
        plot.margin = unit(c(10, 10, 10, 10), "mm"),
        axis.title.y = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank()
        )
  
pretty_choro_map

#Salvo il grafico
map1 <- pretty_choro_map
ggsave(
  map1,
  filename = "redditi_comuni_2021_sky.jpg",
  width = 1000,
  height = 1000,
  units = "px",
  bg="white", dpi = 140
)


#continuare

# https://r-charts.com/spatial/choropleth-map-ggplot2/
# https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html



# create color palette

library(MetBrewer)
library(RColorBrewer)
library(viridis)

#esempi
c1 <- met.brewer("Hokusai2")
mypalette<-brewer.pal(7,"Greens")
pal <- hcl.colors(6, "Inferno", rev = TRUE, alpha = 0.7)


#MAPPA

mappa_redditi = ggplot(Italia_comuni_redditi_geo) +
  geom_sf(color = "white",
          size = 0.05,
          aes(fill = Imponibile_procapite)) +
  #scale_fill_met_c("Hokusai3", override.order=TRUE)+
  #scale_fill_binned_sequential(palette = "Blues 2",begin=0.25, end=1, alpha=1)+
  #scale_fill_manual(values = c1,drop = FALSE,na.value = "grey80")+
  #scale_fill_manual(values=met.brewer("Greek", 5))
  theme_void() +
  theme(legend.position = "none")

mappa_redditi

#interessante per mappe con geom_sf: https://r-spatial.org/r/2018/10/25/ggplot2-sf-2.html


ggsave(
  mappa_redditi,
  filename = "redditi_prova.jpg",
  width = 1000,
  height = 2000,
  units = "px",
  bg="white", dpi = 140
)

#Mappa Choropleth
Italia_comuni_redditi_geo |>
  ggplot() +
  geom_sf(aes(fill = "Imponibile pro capite"), color=NA) +
  labs(title = "Reddito Istat procapite nei comuni italiani",
       subtitle = "",
       caption = "",
       fill = "???") +
  # Custom palette
  scale_fill_manual(values = pal,
                    drop = FALSE,
                    label = "labs_plot",
                    # Legend
                    guide = guide_legend(direction = "horizontal",
                                         nrow = 1,
                                         label.position = "bottom")) + 
  theme_void()

pal <- hcl.colors(6, "Inferno", rev = TRUE, alpha = 0.7)

+
  geom_sf(aes(fill = values), 
        color = "white",
        linetype = 1,
        lwd = 0.25)

#https://r-graph-gallery.com/custom-fonts-in-R-and-ggplot2.html
#https://r-charts.com/spatial/choropleth-map-ggplot2/

