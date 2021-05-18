##############
# Date: 05 17 2021
# Project: Hoja 1 Mercado Pago
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

sql <- df %>%
  group_by(`Tipo de Operación`) %>%
  summarise(n = sum(Importe))



data.frame(`Soma`= "Total general",
           Valores = sum(sql$n))



# 5.0 Create DataFrame with Hoja1 names

hoja1 <- data.frame(names = c("Bancos Tranferencias",
                              "Retencion de ganancia",
                              "Documento a cobrar",
                              "Banco Iguales - Esta em BANCO",
                              "Banco saldo anterior - Saldo que no entro de 12",
                              "Deuda x Banco no entro - Bajada Banco",
                              "Deuda x retenido - Bajada MP",
                              "Deuda anterior",
                              "Cobranza Bevy",
                              "Percepción Ing. Brutos CAP. FED.",
                              "Retención de ingresos brutos de Córdoba",
                              "Percepción Ing. Brutos Pcia. Bs. As.",
                              "Retención de ingresos brutos de La Pampa",
                              "Retención de ingresos brutos de Santa Fe",
                              "Retención de ingresos brutos de Mendoza",
                              "Retención de ingresos brutos de Tucumán",
                              "Retención de ingresos brutos de Entre Ríos",
                              "Retención de ingresos brutos de Catamarca",
                              "Retención de ingresos brutos de Jujuy",
                              "Retención de Ingresos brutos de Río Negro",
                              "Retención de ingresos brutos de Neuquén",
                              "Retención de ingresos brutos de Santiago del Estero",
                              "Retención de ingresos brutos de Buenos Aires",
                              "Retención de ingresos brutos de CABA",
                              "Retención de ingresos brutos de Salta",
                              "Retención Impuesto Ingresos Brutos Régimen SIRTAC",
                              "Retención Impuesto al valor agregado",
                              "Retención de Ganancias",
                              "Anulación de costo por intereses absorbidos",
                              "IVA",
                              "Retención Impuesto Ingresos Brutos Régimen SIRTAC",
                              "Costo por intereses absorbidos",
                              "Dinero retenido por contracargo",
                              "Costo Mercado",
                              "IVA"),
                   x2 = rep(0,35),
                   x3 =rep(0,35),
                   id = 1:35)

hoja1$names <- stringr::str_to_lower(hoja1$names)

sql$`Tipo de Operación`<- gsub("a los iibb","de ingresos brutos", sql$`Tipo de Operación`) # remove string in anyone



# 6.0 Join----

hoja1 <- merge(x = sql, y = hoja1, by.x= "Tipo de Operación",
      by.y = "names", all.y = TRUE)


colnames(hoja1) <- c("x1",'x2','x3','x4',"ID")

hoja1 <- hoja1 %>%
  arrange(ID)

hoja1$ID<- NULL

hoja1$x2[is.na(hoja1$x2)] = 0




# 6.0
hoja1[34,2] <- abs(filter(sql, `Tipo de Operación` ==
                            "costo de mercado pago")[2])

hoja1[31,2] <- abs(filter(sql, `Tipo de Operación` ==
                            "retención impuesto ingresos brutos régimen sirtac")[2])

hoja1[29,2] <- abs(filter(sql, `Tipo de Operación` ==
                            "costo por intereses absorbidos")[2])

hoja1[28,2] <- abs(filter(sql, `Tipo de Operación` ==
                            "retención impuesto a las ganancias")[2])

hoja1[27,2] <- abs(filter(sql, `Tipo de Operación` ==
                            "retención impuesto al valor agregado")[2])

hoja1[25,2] <- abs(filter(sql, `Tipo de Operación` ==
                            "retención de ingresos brutos de salta")[2])


# Columns 4

hoja1[4,2] <- 14220130.74
hoja1[5,2] <- 0
hoja1[6,2] <- 308479.93
hoja1[7,2] <- 11799191.14
hoja1[8,3] <- 14546527.99


hoja1[4,4] <- as.numeric(hoja1[4,2]) + as.numeric(hoja1[5,2])

hoja1[6,4] <- as.numeric(hoja1[6,2]) + as.numeric(hoja1[7,2])


#Deuda x retenido - Bajada MP



hoja1[9,3] <- sum((hoja1[4:35,2])) - hoja1[8,3]


hoja1[29,4] <- round((as.numeric(hoja1[29,2])/1.21),digits = 2)

hoja1[30,4] <- as.numeric(hoja1[29,2]) - as.numeric(hoja1[29,4])

hoja1[29,4] <- round((as.numeric(hoja1[29,2])/1.21),digits = 2)

hoja1[34,4] <- round((as.numeric(hoja1[34,2])/1.21),digits = 2)

hoja1[35,4] <- as.numeric(hoja1[34,2]) - as.numeric(hoja1[34,4])


hoja1$x1<- gsub(0,"", hoja1$x1) 
hoja1$x2<- gsub(0,"", hoja1$x2) 
hoja1$x3<- gsub(0,"", hoja1$x3) 
hoja1$x4<- gsub(0,"", hoja1$x4) 




