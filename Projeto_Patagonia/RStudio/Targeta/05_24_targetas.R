##############
# Date: 05 19 2021
# Project: Targetas
##############


# 1.0 Packate----


library(pdftools)
library(tidyverse)


# 2.0 Load pdf

txt <- pdf_text("Targeta/Dados/resumen_mensual_202104.pdf")

# first page text
cat(txt[1])


page_01 <- as.data.frame(cat(txt[1]))


df <- pdf_data("Targeta/Dados/resumen_mensual_202104 PARQUE .pdf")

df <- df[[1]]

matrix( 
     c(1,2,3,4,5,6), # the data elements 
     nrow=2,              # number of rows 
     ncol=3,              # number of columns 
     byrow = TRUE)
