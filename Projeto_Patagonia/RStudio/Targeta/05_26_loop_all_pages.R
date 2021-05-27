
# 1.0 Packate----

library(pdftools)
library(dplyr)
library(stringr)
library(xlsx)
library(tidyverse)

# 2.0 Loop---

txt <- pdf_text("Targeta/Dados/resumen_mensual_202104 PARQUE.pdf")%>%
  str_split("\n")

# Sum Page
length(txt)


for (i in 1:length(txt)){
  
  df <- as.data.frame(txt[i])
  
  df <- df %>%
    rowwise() %>%
    mutate_all(funs(str_squish(.))) %>%
    ungroup()
  
  colnames(df) <-c('text')
  
  df$x1 <- df$text
  
  df$x2 <- df$x1
  
  excluir <- c("[$]",'[,]','[.]','[0-9]',"[/]","-",':')
  
  for (j in 1:length(excluir)){
    
    df$x1 <- gsub(excluir[j],"", df$x1)
  }
  
  excluir1 <- c("[$]",'[áéíóúaèìòùâôêîôûãõü]','[A-z]', "[:]",'[/]',"°","º")
  
  
  for (k in 1:length(excluir1)){
    
    df$x2 <- gsub(excluir1[k],"", df$x2)
  }
  
  
  df <- df %>%
    rowwise() %>%
    mutate_all(funs(str_squish(.))) %>%
    ungroup()
  
  
  df <- df %>%
    separate(x2, into = c('x3',"x4","x5",'x6','x7','x8'), sep = " ")
  
  df[is.na(df)]= ""
  
  df <- df[,c(2:8,1)]
  
  
  write.xlsx(df, file = paste0("Targeta/df/Extrato",".xlsx"),  
             sheetName = paste0("Ext",i), append = TRUE, col.names = FALSE)
  
  
  
}



# 3.0 Draft----




path <- dir("../RStudio/Targeta/Dados")


for (p in 1:length(path)){
  

  dir.create(paste0("../RStudio/Targeta/Dados/Extrado_",Sys.Date()))
  
  
txt <- pdf_text(paste0("Targeta/Dados/",path[p])) %>%
  str_split("\n")


df <-  as.data.frame(txt[p]) %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

colnames(df) <-c('text')

write.xlsx(df, file = paste0("Targeta/Dados/Extrado_",Sys.Date(),"/",path[p],".xlsx"),  
           sheetName = paste0("Ext",p), append = FALSE, col.names = FALSE)

}


