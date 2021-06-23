# 1.0 Packages----

source('script/bd.R')
pacman::p_load('shiny', 'rJava', 'readxl','tidyverse','xlsx','RTerra')
library(RTerra)


# 2.0 Information----

path <- dir("../Version_1/Fontes/Base_de_dados")
file <- "Bariloche.xlsx"
start_date <- '05-04-2021'
#end_date  <- format(Sys.Date() - 1, "%d-%m-%Y")
end_date <- '17-04-2021'


# ICG

# Higieniza??O no banco de dados
icg <- icg %>%
  separate(Fecha, into = c('m','d','y'), sep = "[/]")

columns <- c("m",'d')


for (c in 1:length(columns)){
  
  for (m in 1:nrow(icg)){
    if(str_count(icg[m,columns[c]]) == 1) {
      
      icg[m,columns[c]] <- paste0('0',icg[m,columns[c]])
      
    }else{
      icg[m,columns[c]] <- icg[m,columns[c]]
    }
  }
}

icg$Fecha <- paste0(icg$d,'-',icg$m,'-',icg$y)
icg<- icg[,-c(2:4)]

icg <- subset(icg, Fecha <= end_date & Fecha >= start_date)



# 3.0 Load Data (Bariloche)----

df_bariloche<- bariloche(path = path, file = file)

df_bariloche <- subset(df_bariloche, FECHA <= end_date & FECHA >= start_date)

targeta <- df_bariloche %>%
  gather( targeta, valor, "VISA DEBITO":"OTRAS") %>%
  summarise(sum(as.numeric(valor)))


bar_MP <- sum(as.numeric(df_bariloche$MERCADOPAGO))
bar_efetivo_real <- sum(as.numeric(df_bariloche$`Efectivo REAL`))
bar_rappi <- sum(as.numeric(df_bariloche$`RAPPI PLATAFORMA`)) + sum(as.numeric(df_bariloche$`RAPPI VOUCHER`))
bar_rappi_efetivo <- sum(as.numeric(df_bariloche$`RAPPI EFECTIVO`))
bar_ya <- sum(as.numeric(df_bariloche$`PEDIDOS YA PLATAFORMA`)) + sum(as.numeric(df_bariloche$`PEDIDOS YA VOUCHER`))
bar_ya_efetivo <- sum(as.numeric(df_bariloche$`PEDIDOS YA EFECTIVO`))

# 3.1 Controle de Vendas Bariloche----

cont_vend_b <- cont_vend


options(digits = 15)
bar_iva <- sum(subset(icg$`IVA 21`, icg$Nombre == "Bariloche"))
bar_imp_int <- sum(subset(icg$IMPUESTO_INTERNO, icg$Nombre == "Bariloche"))
bar_venta <- sum(subset(icg$`Importe Neto Gravado`, icg$Nombre == "Bariloche"))
cont_vend_b[1,"Ipte Mon Doc"] <- bar_iva
cont_vend_b[2,"Ipte Mon Doc"] <- bar_imp_int
cont_vend_b[3,"Ipte Mon Doc"] <- bar_venta
cont_vend_b[4,"Ipte Mon Doc"] <- bar_targeta
cont_vend_b[5,"Ipte Mon Doc"] <- bar_MP
cont_vend_b[6,"Ipte Mon Doc"] <- bar_efetivo_real
cont_vend_b[7,"Ipte Mon Doc"] <- bar_rappi
cont_vend_b[8,"Ipte Mon Doc"] <- bar_rappi_efetivo
cont_vend_b[9,"Ipte Mon Doc"] <- bar_ya
cont_vend_b[10,"Ipte Mon Doc"] <- bar_ya_efetivo

cruzada <- bar_iva + bar_imp_int + bar_venta - bar_targeta - bar_MP - bar_efetivo_real - bar_rappi - bar_rappi_efetivo - bar_ya - bar_ya_efetivo
cruzada <- 0

if(cruzada < 0){
  cont_vend_b[nrow(cont_vend_b)-1,"Ipte Mon Doc"] <- cruzada
} else{
  cont_vend_b[nrow(cont_vend_b)-1,"Ipte Mon Doc"] <- 0
}


if(cruzada > 0){
  cont_vend_b[nrow(cont_vend_b),"Ipte Mon Doc"] <- cruzada
} else{
  cont_vend_b[nrow(cont_vend_b),"Ipte Mon Doc"] <- 0
}


# 4.0 Load Data (Stella)----


for (k in 1:2) {
  
  df <- read_excel(paste0("../Version_1/Fontes/Base_de_dados/",
                          path[grep("Stella", path)[k]]), sheet = 1)
  
  # Colnames----
  
  colnames(df) <- paste0('x',rep(1:ncol(df)))
  
  num <- grep('Fecha', df$x1)
  
  colnames(df) <- df[num,]
  df <- df[-(1:num),]
  
  
  # Remove Letter
  
  df$Fecha <- gsub('[A-z]',NA, df$Fecha)
  df <- subset(df, !is.na(df$Fecha))
  
  # Converte for date
  
  df$Fecha <- as.numeric(df$Fecha)
  df$Fecha <- format((as.Date(df$Fecha, origin = "2010-01-01") - 40179), "%d-%m-%Y")
  
  
  # Table VendasBares
  
  names <- gsub("\n","",names(df))
  colnames(df) <- names
  
  df <- df[,1:grep('Control cobranzas', names(df))]
  
  df$Estabelecimento <- rep(list[grep("Stella",list)[k]],nrow(df))
  
  colnames(df_all) <- names(df)
  
  df_all <- rbind(df_all, df)
  
  df_all <- to.missing(df_all, colnames(df_all))
  

}


df_all <- subset(df_all, Fecha <= end_date & Fecha >= start_date)


# 4.1.1 Controle de Vendas stella_LP----



cont_vend_s_lp <- stella_lp()



options(digits = 15)
s_lp_iva <- sum(subset(icg$`IVA 21`, icg$Caja == "E51"))
s_lp_imp_int <- sum(subset(icg$IMPUESTO_INTERNO, icg$Caja == "E51"))
s_lp_venta <- sum(subset(icg$`Importe Neto Gravado`, icg$Caja == "E51"))


df_stella_lp <- subset(df_all, Estabelecimento == "StellaLP")

s_lp_targeta <- df_stella_lp %>%
  gather(targeta, valor, "VISA DEBITO":"Otras")
s_lp_targeta <- sum(as.numeric(s_lp_targeta$valor))





s_lp_MP <- sum(as.numeric(df_stella_lp$`Mercado Pago`))
s_lp_efetivo_real <- sum(as.numeric(df_stella_lp$`Efectivo REAL`))
s_lp_rappi <- sum(as.numeric(df_stella_lp$`RAPPI PLATAFORMA`)) + sum(as.numeric(df_stella_lp$`RAPPI VOUCHER`))
s_lp_rappi_efetivo <- sum(as.numeric(df_stella_lp$`RAPPI EFECTIVO`))
s_lp_ya <- sum(as.numeric(df_stella_lp$`PEDIDOS YA PLATAFORMA`)) + sum(as.numeric(df_stella_lp$`PEDIDOS YA VOUCHER`))
s_lp_ya_efetivo <- sum(as.numeric(df_stella_lp$`PEDIDOS YA EFECTIVO`))


cont_vend_s_lp[1,"Ipte Mon Doc"] <- s_lp_iva
cont_vend_s_lp[2,"Ipte Mon Doc"] <- s_lp_imp_int
cont_vend_s_lp[3,"Ipte Mon Doc"] <- s_lp_venta
cont_vend_s_lp[4,"Ipte Mon Doc"] <- s_lp_targeta
cont_vend_s_lp[5,"Ipte Mon Doc"] <- s_lp_MP
cont_vend_s_lp[6,"Ipte Mon Doc"] <- s_lp_efetivo_real
cont_vend_s_lp[7,"Ipte Mon Doc"] <- s_lp_rappi
cont_vend_s_lp[8,"Ipte Mon Doc"] <- s_lp_rappi_efetivo
cont_vend_s_lp[9,"Ipte Mon Doc"] <- s_lp_ya
cont_vend_s_lp[10,"Ipte Mon Doc"] <- s_lp_ya_efetivo

cruzada <- s_lp_iva + s_lp_imp_int + s_lp_venta - s_lp_targeta - s_lp_MP - s_lp_efetivo_real - s_lp_rappi - s_lp_rappi_efetivo - s_lp_ya - s_lp_ya_efetivo

if(cruzada < 0){
  cont_vend_s_lp[nrow(cont_vend_s_lp)-1,"Ipte Mon Doc"] <- cruzada
} else{
  cont_vend_s_lp[nrow(cont_vend_s_lp)-1,"Ipte Mon Doc"] <- 0
}


if(cruzada > 0){
  cont_vend_s_lp[nrow(cont_vend_s_lp),"Ipte Mon Doc"] <- cruzada
} else{
  cont_vend_s_lp[nrow(cont_vend_s_lp),"Ipte Mon Doc"] <- 0
}


# 5.0 Controle de Vendas stella_VC----



cont_vend_s_vc <- stella_vc()

options(digits = 15)
s_vc_iva <- sum(subset(icg$`IVA 21`, icg$Caja == "BI1"))
s_vc_imp_int <- sum(subset(icg$IMPUESTO_INTERNO, icg$Caja == "BI1"))
s_vc_venta <- sum(subset(icg$`Importe Neto Gravado`, icg$Caja == "BI1"))


df_stella_vc <- subset(df_all, Estabelecimento == "StellaVC")

s_vc_targeta <- df_stella_vc %>%
  gather(targeta, valor, "VISA DEBITO":"Otras")
s_vc_targeta <- sum(as.numeric(s_vc_targeta$valor))





s_vc_MP <- sum(as.numeric(df_stella_vc$`Mercado Pago`))
s_vc_efetivo_real <- sum(as.numeric(df_stella_vc$`Efectivo REAL`))
s_vc_rappi <- sum(as.numeric(df_stella_vc$`RAPPI PLATAFORMA`)) + sum(as.numeric(df_stella_vc$`RAPPI VOUCHER`))
s_vc_rappi_efetivo <- sum(as.numeric(df_stella_vc$`RAPPI EFECTIVO`))
s_vc_ya <- sum(as.numeric(df_stella_vc$`PEDIDOS YA PLATAFORMA`)) + sum(as.numeric(df_stella_vc$`PEDIDOS YA VOUCHER`))
s_vc_ya_efetivo <- sum(as.numeric(df_stella_vc$`PEDIDOS YA EFECTIVO`))


cont_vend_s_vc[1,"Ipte Mon Doc"] <- s_vc_iva
cont_vend_s_vc[2,"Ipte Mon Doc"] <- s_vc_imp_int
cont_vend_s_vc[3,"Ipte Mon Doc"] <- s_vc_venta
cont_vend_s_vc[4,"Ipte Mon Doc"] <- s_vc_targeta
cont_vend_s_vc[5,"Ipte Mon Doc"] <- s_vc_MP
cont_vend_s_vc[6,"Ipte Mon Doc"] <- s_vc_efetivo_real
cont_vend_s_vc[7,"Ipte Mon Doc"] <- s_vc_rappi
cont_vend_s_vc[8,"Ipte Mon Doc"] <- s_vc_rappi_efetivo
cont_vend_s_vc[9,"Ipte Mon Doc"] <- s_vc_ya
cont_vend_s_vc[10,"Ipte Mon Doc"] <- s_vc_ya_efetivo

cruzada <- s_vc_iva + s_vc_imp_int + s_vc_venta - s_vc_targeta - s_vc_MP - s_vc_efetivo_real - s_vc_rappi - s_vc_rappi_efetivo - s_vc_ya - s_vc_ya_efetivo

if(cruzada < 0){
  cont_vend_s_vc[nrow(cont_vend_s_vc)-1,"Ipte Mon Doc"] <- cruzada
} else{
  cont_vend_s_vc[nrow(cont_vend_s_vc)-1,"Ipte Mon Doc"] <- 0
}


if(cruzada > 0){
  cont_vend_s_vc[nrow(cont_vend_s_vc),"Ipte Mon Doc"] <- cruzada
} else{
  cont_vend_s_vc[nrow(cont_vend_s_vc),"Ipte Mon Doc"] <- 0
}







# 5.0 Macro----

macro <- filter(cont_vend_b, `Ipte Mon Doc` !=0)
macro <- rbind(macro, filter(cont_vend_s_lp, `Ipte Mon Doc` !=0))
macro <- rbind(macro, filter(cont_vend_s_vc, `Ipte Mon Doc` !=0))


macro$`Clase Mov` <- rep("CU",nrow(macro))
macro$Data <- rep(format(Sys.Date(), "%d/%m/%Y"),nrow(macro))
macro$`Ipte Mon Doc`<- abs(as.numeric(macro$`Ipte Mon Doc`))

               
      





