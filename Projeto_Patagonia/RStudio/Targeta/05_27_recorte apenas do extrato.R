
# 1.0 Package

library(tidyverse)
library(pdftools)

data <- pdf_text("Targeta/Dados/resumen_mensual_202104 BARILOCHE .pdf") %>% 
  str_split("\n")


extrato <- data.frame("",'','','')

colnames(extrato) <- c("Detalle De Las Transacciones",'Monto Presentado','Detalle de Descuentos','Neto Percebido')
# Resumen Mensual de Liquidação


for (i in 1:length(data)){

df <- data.frame(matrix(unlist(data[[i]]), nrow=length(data[[i]]), byrow=TRUE))

df <- df %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

colnames(df) <- "x"

df_ext <- df[grep("^DET", df$x):nrow(df),]


df_ext<- df_ext %>%
  separate(x, into = c("Detalle De Las Transacciones",'Monto Presentado','Detalle de Descuentos','Neto Percebido'), sep = "[$]")

df_ext<- subset(df_ext, df_ext$`Detalle De Las Transacciones` != "")


df_ext[,2:4][is.na(df_ext[,2:4])] = ""

extrato <- rbind(extrato, df_ext)


}



