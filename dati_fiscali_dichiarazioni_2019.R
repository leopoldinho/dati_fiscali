#Libraries

library(tidyverse)
library(googlesheets4)


dir.create("dati_fiscali")
setwd("C:/Users/Raffo/Documents/dati_fiscali")


#2024#

Dichiarazioni_2024_ <- read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2023.csv", sep=";")

Dichiarazioni_2024 <- Dichiarazioni_2024_ %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti,
         reddito_medio_frequenza=Reddito.imponibile...Ammontare.in.euro/Reddito.imponibile...Frequenza) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,Dichiaranti=Reddito.imponibile...Frequenza,
         Reddito=reddito_medio_dichiarato, "Reddito medio"=reddito_medio_frequenza)

write.csv(Dichiarazioni_2024, "Comuni_irpef_2024_b.csv")

dichiarazioni_2024_lomb = Dichiarazioni_2024 %>%
  filter(Regione=="Lombardia")

write.csv(dichiarazioni_2024_lomb, "dichiarazioni_2024_lomb.csv")

summary_dichiarazioni_2024 = Dichiarazioni_2024_ %>%
  group_by(Anno.di.imposta) %>%
  summarise(Reddito_medio=sum(Reddito.imponibile...Ammontare.in.euro)/ sum(Reddito.imponibile...Frequenza))

Dichiarazioni_2024_Liguria_tot <- Dichiarazioni_2024_ %>% 
  filter(Regione=="Liguria")


Dichiarazioni_2024_Liguria <- Dichiarazioni_2024 %>% 
  filter(Regione=="Liguria")

write.csv(Dichiarazioni_2024_Liguria, "Comuni_irpef_2024_Liguria.csv")

Dichiarazioni_2024_Liguria_Comuni_scaglioni = Dichiarazioni_2024_ %>% 
  filter(Regione=="Liguria")

write.csv(Dichiarazioni_2024_Liguria_Comuni_scaglioni, "Dichiarazioni_2024_Liguria_Comuni_scaglioni.csv")

# Cap 2024

Dichiarazioni_2024_cap_ <-
  read.csv("Redditi_e_principali_variabili_IRPEF_su_base_subcomunale_CSV_2023.csv",
           sep = ";")

Dichiarazioni_2024_cap <- Dichiarazioni_2024_cap_ %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti,
         reddito_medio_frequenza=Reddito.imponibile...Ammontare.in.euro/Reddito.imponibile...Frequenza) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(
    Denominazione.Comune,
    CAP,
    Contribuenti = Numero.contribuenti,
    Dichiaranti=Reddito.imponibile...Frequenza,
    Reddito=reddito_medio_dichiarato,
    "Reddito medio"=reddito_medio_frequenza
  )


Dichiarazioni_2024_Genova_cap = Dichiarazioni_2024_cap %>%
  mutate(Paese="Italy")%>%
  filter(Denominazione.Comune=="GENOVA")%>%
  mutate(Paese="Italy")

write.csv(Dichiarazioni_2024_Genova_cap, "Irpef_2023_GE.csv")

Dichiarazioni_2024_Spezia_cap = Dichiarazioni_2024_cap%>%
  mutate(Paese="Italy")%>%
  filter(Denominazione.Comune=="LA SPEZIA")%>%
  mutate(Paese="Italy")

write.csv(Dichiarazioni_2024_Spezia_cap, "Irpef_2023_SP.csv")


Dichiarazioni_2024_Roma_cap = Dichiarazioni_2024_cap %>%
  filter(Denominazione.Comune=="ROMA")%>%
  mutate(Paese="Italy")

write.csv2(Dichiarazioni_2024_Roma_cap, "Irpef_2023_RM.csv")

Dichiarazioni_2024_Milano_cap = Dichiarazioni_2024_cap %>%
  filter(Denominazione.Comune=="MILANO")%>%
  mutate(Paese="Italy")

write.csv2(Dichiarazioni_2024_Milano_cap, "Irpef_2023_MI.csv")

Dichiarazioni_2024_Torino_cap = Dichiarazioni_2024_cap %>%
  filter(Denominazione.Comune=="TORINO")%>%
  mutate(Paese="Italy")

write.csv2(Dichiarazioni_2024_Torino_cap, "Irpef_2023_To.csv")

Dichiarazioni_2024_Napoli_cap = Dichiarazioni_2024_cap %>%
  filter(Denominazione.Comune=="NAPOLI")%>%
  mutate(Paese="Italy")

write.csv2(Dichiarazioni_2024_Napoli_cap, "Irpef_2023_NA.csv")


Dichiarazioni_2024_Firenze_cap = Dichiarazioni_2024_cap %>%
  filter(Denominazione.Comune=="FIRENZE")%>%
  mutate(Paese="Italy")

write.csv2(Dichiarazioni_2024_Firenze_cap, "Irpef_2023_FI.csv")


Dichiarazioni_2024_Bologna_cap = Dichiarazioni_2024_cap %>%
  filter(Denominazione.Comune=="BOLOGNA")%>%
  mutate(Paese="Italy")

write.csv2(Dichiarazioni_2024_Bologna_cap, "Irpef_2023_BO.csv")


Dichiarazioni_2024_Palermo_cap = Dichiarazioni_2024_cap %>%
  filter(Denominazione.Comune=="PALERMO")%>%
  mutate(Paese="Italy")

write.csv2(Dichiarazioni_2024_Palermo_cap, "Irpef_2023_PA.csv")


write.csv(Dichiarazioni_2024_cap, "Irpef_2023_cap.csv")



#PAST#

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

dichiarazioni_2023_lomb = Dichiarazioni_2023 %>%
  filter(Regione=="Lombardia")

write.csv(dichiarazioni_2023_lomb, "dichiarazioni_2023_lomb.csv")

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

#Classi di reddito
exclude_vars <- c("Classi.di.reddito.complessivo.in.euro", "Regione") # Specifica le variabili da escludere

Classi_reddito_2024_ <- read.csv2("REG_calcolo_irpef_2024.csv", sep=";") 

Classi_reddito_2024_ = Classi_reddito_2024_ %>%
  mutate(across(
    .cols = -all_of(exclude_vars),
    .fns = ~ as.numeric(
      gsub("\\.", "", .)  # tolgo i separatori di migliaia "."
        )
  ))

Classi_reddito_2024_ = Classi_reddito_2024_ %>%
  mutate(across(everything(), ~replace_na(., 0)))

Classi_reddito_2024_Liguria = Classi_reddito_2024_ %>%
  filter(Regione == "Liguria") %>%
  select(
    Classi.di.reddito.complessivo.in.euro,
    Numero.contribuenti,
    Reddito.imponibile...Frequenza,
    Reddito.imponibile...Ammontare.in.euro,
    Imposta.netta...Frequenza,
    Imposta.netta...Ammontare.in.euro
  )

#calcolo tipo reddito 

Tipo_reddito_2024_ <- read.csv2("REG_tipo_reddito_2024.csv", sep=";") 

Tipo_reddito_2024_ = Tipo_reddito_2024_ %>%
  mutate(across(
  .cols = -all_of(exclude_vars),
  .fns = ~ as.numeric(
    gsub("\\.", "", .)  # tolgo i separatori di migliaia "."
  )
))

Tipo_reddito_2024_ = Tipo_reddito_2024_ %>%
  mutate(across(everything(), ~replace_na(., 0)))

Tipo_reddito_2024_Liguria = Tipo_reddito_2024_ %>%
  filter(Regione == "Liguria") %>%
  select(
    Classi.di.reddito.complessivo.in.euro,
    Reddito.da.fabbricati...Frequenza,
    Reddito.da.fabbricati...Ammontare.in.euro,
    Reddito.da.pensione...Frequenza,
    Reddito.da.pensione...Ammontare.in.euro,
    Reddito.da.lavoro.dipendente.e.assimilati...Frequenza,
    Reddito.da.lavoro.dipendente.e.assimilati...Ammontare.in.euro,
    Reddito.da.lavoro.autonomo...Frequenza,
    Reddito.da.lavoro.autonomo...Ammontare.in.euro
  )

Redditi_Tasse_liguria = Classi_reddito_2024_Liguria %>%
  left_join(Tipo_reddito_2024_Liguria, by = "Classi.di.reddito.complessivo.in.euro") %>%
  bind_rows(summarise(., across(where(is.numeric), sum), across(where(is.character), ~
                                                                  "Totale")))

Redditi_Tasse_liguria_ <- Redditi_Tasse_liguria %>%
  mutate(Reddito = case_when(
    row_number() %in% 1:3 ~ "Zero o minore di zero",
    row_number() %in% 1:13 ~ "Fino a 7.500",
    row_number() %in% 14:16 ~ "Da 7.500 a 15.000",
    row_number() %in% 17:17 ~ "Da 15.000 a 20.000",
    row_number() %in% 18:19 ~ "Da 20.000 a 29.000",
    row_number() %in% 20:20 ~ "Da 29.000 a 35.000",
    row_number() %in% 21:23 ~ "Da 35.000 a 55.000",
    row_number() %in% 24:29 ~ "Da 55.000 a 100.000",
    row_number() %in% 30:32 ~ "Da 100.000 a 200.000",
    row_number() %in% 30:32 ~ "Da 100.000 a 200.000",
    row_number() %in% 33:33 ~ "Da 200.000 a 300.000",
    row_number() %in% 34:34 ~ "Da 200.000 a 300.000",
    row_number() %in% 35:35 ~ "TOTALE",
    TRUE ~ NA_character_
  )) %>%
  group_by(Reddito) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(Reddito))



Redditi_Tasse_liguria_def <- bind_rows(Redditi_Tasse_liguria_) %>%
  mutate(Reddito = factor(
    Reddito,
    levels = c(
      "Zero o minore di zero",
      "Fino a 7.500",
      "Da 7.500 a 15.000",
      "Da 15.000 a 20.000",
      "Da 20.000 a 29.000",
      "Da 29.000 a 35.000",
      "Da 35.000 a 55.000",
      "Da 55.000 a 100.000",
      "Da 100.000 a 200.000",
      "Da 200.000 a 300.000",
      "TOTALE"
    )
  )) %>%
  arrange(Reddito) %>%
  rename(Contribuenti = Numero.contribuenti,
         Dichiaranti = Reddito.imponibile...Frequenza,
         Versanti=Imposta.netta...Frequenza,
         Imponibile = Reddito.imponibile...Ammontare.in.euro,
         Imposta=Imposta.netta...Ammontare.in.euro
         )%>%
  mutate("Imponibile medio x contribuente"=Imponibile/Contribuenti,
         "Imposta media per contribuente"=Imposta/Contribuenti,
         "Perc contribuenti sul totale"=Contribuenti/1199819*100)%>%
  mutate_if(is.numeric, round, 2)%>%
  relocate(Reddito, 
           Contribuenti,
           "Perc contribuenti sul totale",
           Dichiaranti,
           Versanti,
           Imponibile,
           Imposta,
           "Imponibile medio x contribuente",
           "Imposta media per contribuente")

#aggiungo le percentuali

Redditi_Tasse_liguria_def  <- Redditi_Tasse_liguria_def  %>%
  mutate(across(c(),
                ~ .x / df_final %>% filter(Reddito == "TOTALE") %>% pull(cur_column()) * 100,
                .names = "{.col} perc"))

write.csv(Redditi_Tasse_liguria, "Redditi_Tasse_liguria.csv")



#TIR E ADDIZIONALE (da rivedere)
Tir_2024_ <- read.csv("addizionale_PFTOT2024tab_02_02_.csv", sep=";") %>%
  mutate(
    Trattamento.spettante...Ammontare = Trattamento.spettante...Ammontare *
      1000,
    Addizionale.regionale.dovuta...Ammontare = Addizionale.regionale.dovuta...Ammontare *
      1000,
    Addizionale.comunale.dovuta...Ammontare = Addizionale.comunale.dovuta...Ammontare *
      1000
  ) %>%
  select(
    Classi.di.reddito.complessivo.in.euro,
    Trattamento.spettante...Ammontare,
    Addizionale.regionale.dovuta...Ammontare,
    Addizionale.comunale.dovuta...Ammontare
  )


#Elaborazione nazionale

Elaborazione_2024 = Classi_reddito_2024_ %>%
  select(
    Classi.di.reddito.complessivo.in.euro,
    Imposta.netta...Frequenza,
    Imposta.netta...Ammontare.in.euro
  ) %>%
  group_by(Classi.di.reddito.complessivo.in.euro) %>%
  summarise(
    Imposta.netta = sum(Imposta.netta...Ammontare.in.euro),
    Dichiaranti = sum(Imposta.netta...Frequenza)
  ) %>%
  ungroup() %>%
  left_join(Tir_2024_, by = "Classi.di.reddito.complessivo.in.euro") %>%
  mutate(
    Irpef = Imposta.netta - Trattamento.spettante...Ammontare + Addizionale.regionale.dovuta...Ammontare +
      Addizionale.comunale.dovuta...Ammontare
  )%>%
  bind_rows(summarise(.,
                      across(where(is.numeric), sum),
                      across(where(is.character), ~"Totale")))
  
write.csv(Elaborazione_2024, "Elaborazione_2024.csv")


#elaborazioni LIGURIA
Classi_reddito_2024_Liguria = Classi_reddito_2024_ %>%
  filter(Regione=="Liguria")

Classi_reddito_2024_Liguria_Summary = Classi_reddito_2024_Liguria %>%
  group_by(Regione) %>%
  summarise(
    sum(Numero.contribuenti),
    sum(Reddito.imponibile...Frequenza),
    sum(Reddito.complessivo...Frequenza),
    sum(Reddito.complessivo...Ammontare.in.euro),
    sum(Reddito.imponibile...Ammontare.in.euro),
    sum(Imposta.netta...Ammontare.in.euro),
    sum(Imposta.netta...Frequenza),
    sum(Differenza...Ammontare.in.euro),
    sum(Reddito.complessivo.al.netto.della.cedolare.secca...Ammontare.in.euro),
    sum(Irpef.a.debito...Ammontare.in.euro),
    sum(Imposta.lorda...Ammontare.in.euro)
  )



classi_reddito_italia = Classi_reddito_2024_ %>%
  group_by(Classi.di.reddito.complessivo.in.euro) %>%
  summarise(
    sum(Numero.contribuenti),
    sum(Reddito.imponibile...Frequenza),
    sum(Reddito.complessivo...Frequenza),
    sum(Reddito.complessivo...Ammontare.in.euro),
    sum(Reddito.imponibile...Ammontare.in.euro),
    sum(Imposta.netta...Ammontare.in.euro),
    sum(Imposta.netta...Frequenza),
    sum(Differenza...Ammontare.in.euro),
    sum(Reddito.complessivo.al.netto.della.cedolare.secca...Ammontare.in.euro),
    sum(Irpef.a.debito...Ammontare.in.euro),
    sum(Imposta.lorda...Ammontare.in.euro)
  )
write.csv(classi_reddito_italia, "classi_reddito_italia.csv")
write.csv(Classi_reddito_2024_Liguria, "Classi_reddito_2024_Liguria.csv")

write.csv(Classi_reddito_2024_Liguria_Summary, "Classi_reddito_2024_Liguria_Summary.csv" )

classi_reddito_italia_sum = Classi_reddito_2024_ %>%
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE)))


Classi_reddito_eta_ = read.csv2("cla_anno_calcolo_irpef_2024.csv", sep=";")