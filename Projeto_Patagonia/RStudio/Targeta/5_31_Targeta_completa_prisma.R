
# 1.0 Packate----

library(pdftools)
library(dplyr)
library(stringr)
library(xlsx)
library(tidyverse)
source("../RStudio/Targeta/_05_27_DB_Bandeiras.R")

# 2.0 Load Data---


path <- dir("../RStudio/Targeta/Dados/Prisma")

folder <- paste0("Targeta_",format(Sys.Date(), "%m-%d-%y"))

dir.create(paste0(getwd(),"/Targeta/Dados/Prisma/",folder))


for (k in 1:length(path)){

data <- pdf_text(paste0("Targeta/Dados/Prisma/",path[k])) %>%
  str_split("\n")

# Sum Page

length(data)


extrato <- data.frame("",'','','')

colnames(extrato) <- c("Detalle De Las Transacciones",'Monto Presentado','Detalle de Descuentos','Neto Percebido')

# Extrato Complet----

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


for ( i in 2:4){
  extrato[,i] <- gsub("[.]","", extrato[,i])
  extrato[,i] <- gsub(",",".", extrato[,i])
  extrato[,i] <- gsub(" ","", extrato[,i])
  
}

# Resumen Mensual de Liquidação

Resumen <- data[[1]][1:grep('ESTABLECIMIENTO',data[[1]])]

 


Resumen <- data.frame(matrix(unlist(Resumen), nrow=length(Resumen), byrow=TRUE))

Resumen <- Resumen %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

colnames(Resumen) <- "a"


Resumen <- Resumen[(grep('HOJA',Resumen$a)+1):nrow(Resumen),]

Resumen <- Resumen %>%
  separate(a, into = c("x1","x2"), sep = ":")




# Monto Grravado

Monto_Gravado <- extrato[grep("Gravado", extrato$`Detalle De Las Transacciones`):grep('Deducciones Impositivas', extrato$`Detalle De Las Transacciones`),2]

Monto_Gravado<- as.numeric(Monto_Gravado)

Monto_Gravado[is.na(Monto_Gravado)] = 0
Monto_Gravado<- sum(as.numeric(Monto_Gravado))





# Total Presentado


tnt <- data[[1]][1:grep("DETALLE", data[[1]])]

lst <- grep(",",data[[1]][1:grep("DETALLE", data[[1]])])
nome <- c("Total Presente", "Total Descuento","Saldo")


Valores_head <- data.frame(x1 = c(""),
                 x2 = c(""))


for (i in 1: length(lst)){
  

  soma <- gsub(" ","",str_sub(data[[1]][lst[i]], start = as.integer(str_count(data[[1]][lst[i]])/2),
                              end = -10))
  
  Valores_head <- rbind(Valores_head, data.frame(x1 = nome[i],
                             x2 = soma))

}

Valores_head <- Valores_head[-1,]



#Valores_head
#############################################



# Estabelecimento

d1<- subset(Resumen, Resumen$x1 == "Nº DE ESTABLECIMIENTO")

# Bandeira e banco

d1$x2 <- gsub(" ","", d1$x2)

d1 <- merge(d1, bandeira, by.x = "x2",by.y = "N_Est", all.x = TRUE)

d1 <- d1[,c(2,1,3:6)]


# Deducciones Impositivas


ded_Imp <- extrato


ded_Imp <- ded_Imp[(grep('Deducciones Impositivas', ded_Imp$`Detalle De Las Transacciones`)):
                     (grep('DGI', ded_Imp$`Detalle De Las Transacciones`)-1),]

ded_Imp <- ded_Imp[,1:2]

ded_Imp$`Monto Presentado`<- as.numeric(ded_Imp$`Monto Presentado`)

ded_Imp <- ded_Imp[complete.cases(ded_Imp$`Monto Presentado`),]



colnames(ded_Imp) <- c("x1",'x2')

#Remover os CR

remov <- grep('CR', ded_Imp$x1)

for (x in 1:length(remov)){
  ded_Imp <- ded_Imp[-(sort(-remov)*(-1))[x],]
}
          


# Retención


Ret_iva <- extrato

colnames(Ret_iva) <- c("x1",'x2','x3','x4')


Ret_iva<- Ret_iva[1:max(grep('_', Ret_iva$x1))-1,]

Ret_iva<- Ret_iva[max(grep('Fin', Ret_iva$x1)):nrow(Ret_iva),]

Ret_iva <- Ret_iva[grep("^Retenci", Ret_iva$x1):nrow(Ret_iva),]


Ret_iva <- Ret_iva[min(grep("IVA", Ret_iva$x1)):nrow(Ret_iva),]


grep("_", Ret_iva$x1)

######

Ret_gan <- Ret_iva[grep("Ganancias", Ret_iva$x1):nrow(Ret_iva),]

Ret_gan$x3 <- as.numeric(Ret_gan$x3)
Ret_gan$x3[is.na(Ret_gan$x3)] = 0
Ret_gan <- sum(Ret_gan$x3)

#####

Ret_iva <- Ret_iva %>%
  mutate(x2 =  ifelse(Ret_iva$x3 == "",Ret_iva$x2,Ret_iva$x3))

Ret_iva <- Ret_iva[,1:2]




###################################################


# Loop
tnt <- Ret_iva



df <- data.frame(x1 = c(""),
                 x2 = c(""))


tnt <- tnt[-grep("Total", tnt$x1),]

for (i in 1:length(grep("_", tnt$x1))){

lst <- grep("_", tnt$x1)

soma <- tnt[1:lst[1],]

soma$x2 <- as.numeric(soma$x2)
soma$x2[is.na(soma$x2)] = 0
soma <- soma[-nrow(soma),]


df <- rbind(df, data.frame(x1 = soma[1,1],
                        x2 = round(sum(soma$x2),digits= 2)))
tnt <- tnt[lst[1]+1:nrow(tnt),]

}

df <- df[-1,]

##############################################


    
geral <- rbind(Valores_head, data.frame(x1 = c("Monto Gravado","Retención Ganancias"),
                                        x2 = c(Monto_Gravado,Ret_gan)),ded_Imp,
               df)


colnames(geral) <- c("Item", "Somatória")



# Loading Data



name <- str_sub(path[k], end = (str_locate(path[k],"[.]")[2])-1)

file <- paste0(getwd(),"/Targeta/Dados/Prisma/",folder,'/',name,".xlsx")


write.xlsx(Resumen, file = file,  
           sheetName = "Resumen Mensual de Liq.", append = TRUE, col.names = FALSE)

write.xlsx(d1, file = file,  
           sheetName = "Bandeira", append = TRUE, col.names = FALSE)

write.xlsx(geral, file = file,  
           sheetName = "Somatória", append = TRUE, col.names = FALSE)

#write.xlsx(ded_Imp, file = file,  
#           sheetName = "Deducciones Impositivas", append = TRUE, col.names = FALSE)

write.xlsx(Ret_iva, file = file,  
           sheetName = "Retención IVA", append = TRUE, col.names = FALSE)

write.xlsx(extrato, file = file,  
           sheetName = "Extrato", append = TRUE, col.names = FALSE)


}


