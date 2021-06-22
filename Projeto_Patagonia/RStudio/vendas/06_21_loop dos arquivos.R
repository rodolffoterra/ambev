# 1.0 Packages----

source('script/bd.R')
pacman::p_load('shiny', 'rJava', 'readxl','tidyverse','xlsx','RTerra')


# 2.0 Information----

path <- dir("../Version_1/Fontes/Base_de_dados")
file <- "Bariloche.xlsx"
start_date <- '05-04-2021'
#end_date  <- format(Sys.Date() - 1, "%d-%m-%Y")
end_date <- '17-04-2021'


# ICG

# Higienização no banco de dados
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

icg$Fecha <- paste0(icg$d,'/',icg$m,'/',icg$y)
icg<- icg[,-c(2:4)]




# Just the columns

icg1 <- icg[,c('Fecha', "Importe Neto Gravado", "IVA 21", "IMPUESTO_INTERNO","PERC_IIBB_BSAS","PERC_IIBB_CABA","Importe Total",
               "Nombre")] 

options(digits = 15)
bar_iva <- sum(subset(icg$`IVA 21`, icg$Nombre == "Bariloche"))
bar_imp_int <- sum(subset(icg$IMPUESTO_INTERNO, icg$Nombre == "Bariloche"))
bar_venta <- sum(subset(icg$`Importe Neto Gravado`, icg$Nombre == "Bariloche"))




# 3.0 Load Data (Bariloche)----

df_bariloche<- df_bariloche(path = path, file = file)


bar_targeta <- df_bariloche %>%
  gather( targeta, valor, "VISA DEBITO":"OTRAS") %>%
  summarise(sum(as.numeric(valor)))


bar_MP <- sum(as.numeric(df_bariloche$MERCADOPAGO))
bar_efetivo_real <- sum(as.numeric(df_bariloche$`Efectivo REAL`))
bar_rappi <- sum(as.numeric(df_bariloche$`RAPPI PLATAFORMA`)) + sum(as.numeric(df_bariloche$`RAPPI VOUCHER`))
bar_rappi_efetivo <- sum(as.numeric(df_bariloche$`RAPPI EFECTIVO`))
bar_ya <- sum(as.numeric(df_bariloche$`PEDIDOS YA PLATAFORMA`)) + sum(as.numeric(df_bariloche$`PEDIDOS YA VOUCHER`))
bar_ya_efetivo <- sum(as.numeric(df_bariloche$`PEDIDOS YA EFECTIVO`))

# 4.0 Controle de Vendas ----

# 4.1. Bariloche ---

cont_vend_b <- cont_vend

cont_vend_b <- cont_vend_b[,-ncol(cont_vend_b)]

options(digits = 15)
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

bar_iva + bar_imp_int + bar_venta - bar_targeta - bar_MP - bar_efetivo_real - bar_rappi - bar_rappi_efetivo - bar_ya - bar_ya_efetivo






# 5.0 Macro----

macro <- filter(cont_vend_b, `Ipte Mon Doc` !='')






