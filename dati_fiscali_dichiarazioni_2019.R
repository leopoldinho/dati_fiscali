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

#2020#

Dichiarazioni_2020 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2019.csv")

Dichiarazioni_2020 <- Dichiarazioni_2020 %>%
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

#2019#

Dichiarazioni_2019 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2018.csv")

Dichiarazioni_2019 <- Dichiarazioni_2019 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2019_Liguria <- Dichiarazioni_2019 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2019_Genova <- Dichiarazioni_2019 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2018#

Dichiarazioni_2018 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2017.csv")

Dichiarazioni_2018 <- Dichiarazioni_2018 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2018_Liguria <- Dichiarazioni_2018 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2018_Genova <- Dichiarazioni_2018 %>% 
  filter(Denominazione.Comune=="GENOVA")


#2017#

Dichiarazioni_2017 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2016.csv")

Dichiarazioni_2017 <- Dichiarazioni_2017 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2017_Liguria <- Dichiarazioni_2017 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2017_Genova <- Dichiarazioni_2017 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2016#

Dichiarazioni_2016 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2015.csv")

Dichiarazioni_2016 <- Dichiarazioni_2016 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro/Reddito.imponibile...Ammontare.in.euro*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare.in.euro,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2016_Liguria <- Dichiarazioni_2016 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2016_Genova <- Dichiarazioni_2016 %>% 
  filter(Denominazione.Comune=="GENOVA")


#2015#

Dichiarazioni_2015 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2014.csv")

Dichiarazioni_2015 <- Dichiarazioni_2015 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2015_Liguria <- Dichiarazioni_2015 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2015_Genova <- Dichiarazioni_2015 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2014#

Dichiarazioni_2014 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2013.csv")

Dichiarazioni_2014 <- Dichiarazioni_2014 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2014_Liguria <- Dichiarazioni_2014 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2014_Genova <- Dichiarazioni_2014 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2013#

Dichiarazioni_2013 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2012.csv")

Dichiarazioni_2013 <- Dichiarazioni_2013 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat=Codice.Istat.Comune,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2013_Liguria <- Dichiarazioni_2013 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2013_Genova <- Dichiarazioni_2013 %>% 
  filter(Denominazione.Comune=="GENOVA")


#2012#

Dichiarazioni_2012 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2011.csv")

Dichiarazioni_2012 <- Dichiarazioni_2012 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2012_Liguria <- Dichiarazioni_2012 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2012_Genova <- Dichiarazioni_2012 %>% 
  filter(Denominazione.Comune=="GENOVA")


#2011#

Dichiarazioni_2011 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2010.csv")

Dichiarazioni_2011 <- Dichiarazioni_2011 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2011_Liguria <- Dichiarazioni_2011 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2011_Genova <- Dichiarazioni_2011 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2010#

Dichiarazioni_2010 <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2009.csv")

Dichiarazioni_2010 <- Dichiarazioni_2010 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


Dichiarazioni_2010_Liguria <- Dichiarazioni_2010 %>% 
  filter(Regione=="Liguria")

Dichiarazioni_2010_Genova <- Dichiarazioni_2010 %>% 
  filter(Denominazione.Comune=="GENOVA")

#2009#

Dichiarazioni_2009 <- read.csv2("Redditi_e_principali_variabili_IRPEF_su_base_comunale_CSV_2008.csv")

Dichiarazioni_2009 <- Dichiarazioni_2009 %>%
  mutate(reddito_medio_dichiarato=
           Reddito.imponibile...Ammontare.in.euro/Numero.contribuenti, Perc_reddito_scaglione_max=Reddito.complessivo.oltre.120000.euro...Ammontare/Reddito.imponibile...Ammontare.in.euro*100,Perc_ricchi=Reddito.complessivo.oltre.120000.euro...Frequenza/Numero.contribuenti*100) %>%
  mutate_if(is.numeric, round, 2) %>%
  select(Anno.di.imposta, Codice.Istat,Regione,Sigla.Provincia,
         Denominazione.Comune, Contribuenti=Numero.contribuenti,
         Imponibile=Reddito.imponibile...Ammontare.in.euro,Perc_ricchi,
         "Imponibile pro capite"=reddito_medio_dichiarato,Reddito_complessivo_ricchi=Reddito.complessivo.oltre.120000.euro...Ammontare,
         Reddito.complessivo.oltre.120000.euro...Frequenza,Perc_reddito_scaglione_max)


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

Dichiarazioni_2020_cap <- read.csv("Redditi_e_principali_variabili_IRPEF_su_base_subcomunale_CSV_2019.csv")

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




write.csv2(Dichiarazioni_2020_Genova, "prova_grafico.csv")

