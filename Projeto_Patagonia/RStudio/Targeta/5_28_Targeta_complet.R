
# 1.0 Packate----

library(pdftools)
library(dplyr)
library(stringr)
library(xlsx)
library(tidyverse)

# 2.0 Load Data---

data <- pdf_text("Targeta/Dados/resumen_mensual_202104 PARQUE.pdf")%>%
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


# Resumen Mensual de Liquidação

Resumen <- data[[1]][1:14]


Resumen <- data.frame(matrix(unlist(Resumen), nrow=length(Resumen), byrow=TRUE))

Resumen <- Resumen %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

colnames(Resumen) <- "a"

Resumen <- Resumen[-1,]

Resumen <- Resumen %>%
  separate(a, into = c("x1","x2"), sep = ":")

Resumen[1,2] <- Resumen[1,1]
Resumen[1,1] <- gsub("[0-9]","", Resumen[1,1])
Resumen[1,2] <- gsub("[A-z]","", Resumen[1,2])


# Total Presentado

Total_Presente <- gsub(" ","",str_sub(data[[1]][22], start = as.integer(str_count(data[[1]][27])/2),
                                      end = -10))

# Total Descuento

total_Descuento <- gsub(" ","",str_sub(data[[1]][27], start = as.integer(str_count(data[[1]][27])/2),
                                       end = -10))

# Saldo

saldo <- gsub(" ","",str_sub(data[[1]][30], start = as.integer(str_count(data[[1]][27])/2),
                             end = -10))

# Monto Grravado

Monto_Gravado <- extrato[grep("Gravado", extrato$`Detalle De Las Transacciones`):grep('Deducciones Impositivas', extrato$`Detalle De Las Transacciones`),2]

Monto_Gravado <- gsub(" ","",Monto_Gravado)
Monto_Gravado <- gsub("[.]","",Monto_Gravado)
Monto_Gravado <- gsub("[a-Z]","",Monto_Gravado)
Monto_Gravado <- gsub("[,]",".",Monto_Gravado)
Monto_Gravado<- as.numeric(Monto_Gravado)

Monto_Gravado[is.na(Monto_Gravado)] = 0
Monto_Gravado<- sum(as.numeric(Monto_Gravado))


# Estabelecimento

end <- data[[1]][c(24,26,27,28)]


end[3] <- str_sub(end[3], start = 1, end = as.integer(str_count(end[3])/2))

end <- data.frame(matrix(unlist(end), nrow=length(end), byrow=TRUE)) %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

end[2,] <- paste(end[2,],end[3,],end[4,], sep =",")

end <- cbind(data.frame(Name = c("Razón Social", "Estabelecimento"), end[1:2,]))

colnames(end) <- c("x1",'x2')

end <- rbind(end, subset(Resumen, Resumen$x1 == "Nº DE ESTABLECIMIENTO"))


d1<- subset(Resumen, Resumen$x1 == "Nº DE ESTABLECIMIENTO")

# Bandeira e banco

d1$x2 <- gsub(" ","", d1$x2)

d1 <- merge(d1, bandeira, by.x = "x2",by.y = "N_Est", all.x = TRUE)


end <- rbind(data.frame(x1 = c("Nome"),
                        x2 = c(d1[,4])),
             end, data.frame(x1 = c("Razon","Banco","Bandeira"),
                             x2 = c(d1[,5], d1[,6])))

data.frame(x1 = c("Banco","Bandeira"),
           x2 = c(d1[,5], d1[,6]))



# Merge

Resumen_Valores <- data.frame(x1 = c("Total Presente", "Total Descuento","Saldo", "Monto Gravado"),
                               x2 = c(Total_Presente, total_Descuento, saldo, Monto_Gravado))


# Deducciones Impositivas


ded_Imp <- extrato


ded_Imp <- ded_Imp[grep('Deducciones Impositivas', ded_Imp$`Detalle De Las Transacciones`):(grep('DGI', ded_Imp$`Detalle De Las Transacciones`)-1),]
ded_Imp <- ded_Imp[1:grep('^_', ded_Imp$`Detalle De Las Transacciones`),]

grep('DGI', ded_Imp$`Detalle De Las Transacciones`)


sum_ded_Imp<- as.numeric(Monto_Gravado)

ded_Imp <- ded_Imp[,1:2]

colnames(ded_Imp) <- c("x1",'x2')

ded_Imp <- rbind(ded_Imp, data.frame(x1 = "Soma",
                                     x2 = sum_ded_Imp))
                   
                   

Monto_Gravado <- extrato[grep("Gravado", extrato$`Detalle De Las Transacciones`):,2]



# Retención


Ret_iva <- extrato

colnames(Ret_iva) <- c("x1",'x2','x3','x4')


max(grep('^Fin', Ret_iva$x1), )

Ret_iva<- Ret_iva[1:max(grep('_', Ret_iva$x1))-1,]

Ret_iva<- Ret_iva[max(grep('Fin', Ret_iva$x1)):nrow(Ret_iva),]

Ret_iva <- Ret_iva[grep("^Retenci", Ret_iva$x1):nrow(Ret_iva),]


Ret_iva$x2<- gsub("[.]","", Ret_iva$x2)
Ret_iva$x2<- gsub(",",".", Ret_iva$x2)

Ret_iva <- Ret_iva[min(grep("IVA", Ret_iva$x1)):nrow(Ret_iva),]


grep("_", Ret_iva$x1)



Ret_iva <- Ret_iva %>%
  mutate(x2 =  ifelse(Ret_iva$x3 == "",Ret_iva$x2,Ret_iva$x3))

Ret_iva <- Ret_iva[,1:2]




###################################################


# Loop
tnt <- Ret_iva

tnt$x2 <- gsub(" ","",tnt$x2)
tnt$x2 <- gsub(",","",tnt$x2)


df <- data.frame(x1 = c(""),
                 x2 = c(""))


for (i in 1:length(grep("_", tnt$x1))){

lst <- grep("_", tnt$x1)

soma <- tnt[1:lst[1]-1,]

soma$x2 <- as.numeric(soma$x2)
soma$x2[is.na(soma$x2)] = 0
soma <- soma[-nrow(soma),]


df <- rbind(df, data.frame(x1 = soma[1,1],
                        x2 = round(sum(soma$x2),digits= 2)))
tnt <- tnt[lst[1]+1:nrow(tnt),]

}

df <- df[-1,]

##############################################



    
    
    





