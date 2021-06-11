install.packages("xlsx")
install.packages("readxl")

library(tidyverse)
library(googlesheets4)
library(xlsx)
library(readxl)

Supermercati_Ita_2019 <- read_excel("Tav_3_SM.xls") %>%
  mutate(tipo="SM")

Supermercati_Ita_2019_Prov <- read_excel("Tav_5_SM.xls") %>%
  mutate(tipo="SM")