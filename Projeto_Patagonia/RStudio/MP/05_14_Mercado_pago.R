##############
# Date: 05 14 2021
# Project: Mercado Pago
##############


# 1.0 Packate----

library(rJava)
library(readxl) # Load file
library(tidyverse) #Manipulation Str
library(openxlsx)

# 2.0 Load File----

df <- read_excel("MP/dados/ARQUIVO ORIGINAL MP.xlsx")



# 3.0 Data Cleaning---

df$Importe <- as.numeric(df$Importe) # Converte Columns in numeric

df$`Tipo de Operación`<- stringr::str_to_lower(df$`Tipo de Operación`) # Converte all Sentence in lower 


df$`Tipo de Operación`<- gsub("anulación parcial de ","", df$`Tipo de Operación`) # remove string in anyone
df$`Tipo de Operación`<- gsub("anulación de ","", df$`Tipo de Operación`)
df$`Tipo de Operación`<- gsub("devolución parcial de ","", df$`Tipo de Operación`)
df$`Tipo de Operación`<- gsub("devolución de ","", df$`Tipo de Operación`)
df$`Tipo de Operación`<- gsub("movimiento general","retención impuesto al valor agregado", df$`Tipo de Operación`)
df$`Tipo de Operación`<- gsub("percepción general impuesto al valor agregado","retención impuesto al valor agregado", 
                              df$`Tipo de Operación`) #Remove string in other string


# 4.0 SQL---

sqd <- df %>%
  group_by(`Tipo de Operación`) %>%
  summarise(n = sum(Importe))

data.frame(`Soma`= "Total general",
                     Valores = sum(sqd$n))
