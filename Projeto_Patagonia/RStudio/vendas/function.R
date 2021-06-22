# Remove Missing Value


to.missing <- function(df,variables){
  for (variable in variables){
    df[[variable]][is.na(df[[variable]])] = 0
  }
  return(df)
}


# Function Bariloche

bariloche <- function(path, file) {
  
  # 3.0 Load Data (Bariloche)----
  
  df <- read_excel(paste0("../Version_1/Fontes/Base_de_dados/",
                          path[grep("Bariloche", path)]), sheet = 1)
  
  
  # 4.0 Colnames----
  
  num <- grep('FECHA', df$...1)
  
  colnames(df) <- df[num,]
  df <- df[-(1:num),]
  
  
  #Remove Letter
  
  df$FECHA <- gsub('[A-z]',NA, df$FECHA)
  df <- subset(df, !is.na(df$FECHA))
  
  # Converte for date
  
  df$FECHA <- as.numeric(df$FECHA)
  df$FECHA <- format((as.Date(df$FECHA, origin = "2010-01-01") - 40179), "%d-%m-%Y")
  
  
  # Table VendasBares
  
  df <- df[,1:grep('Control cobranzas', names(df))]
  
  df$Estabelecimento <- rep("Bariloche", nrow(df))
  
  colnames(df) <- gsub("\n", "", names(df))
  colnames(df) <- gsub("\r", "", names(df))
  
  
  colnames(df_all) <- names(df)
  
  df_all <- rbind(df_all, df)
  
  return(df_all)
  
}



stella_lp <- function(){
  
cont_vend_s_lp <- cont_vend
cont_vend_s_lp$Cuenta <- c('4730009','4730002','5370088','30040935','30040932','30040933','30033334','30033334','30033334','30033334','6104757','6104757')
cont_vend_s_lp$Texto <- c('Impuesto Interno - Stella Artois LP','IVA - Stella Artois LP','Venta - Stella Artois LP', 'Cobranza Efectivo - Stella Artois LP','Cobranza Tarjetas - Stella Artois LP','Cobranza Mercado Pago - Stella Artois LP','Pedidos Ya - Stella Artois LP','Pedidos Ya efectivo - Stella Artois LP','Rappi - Stella Artois LP','Rappi efectivo - Stella Artois LP','diferencia por cobranza - Stella Artois LP','diferencia por cobranza - Stella Artois LP')
cont_vend_s_lp$Div. <- rep('A087', nrow(cont_vend_s_lp))
cont_vend_s_lp$CECO <- rep('A087060005', nrow(cont_vend_s_lp))

return(cont_vend_s_lp)

}

stella_vc <- function(){
  
  cont_vend_s_vc <- cont_vend
  cont_vend_s_vc$Cuenta <- c('4730009','4730002','5370088','30040939','30040937','30040938','30033334','30033334','30033334','30033334','6104757','6104757')
  cont_vend_s_vc$Texto <- c('Cobranza Efectivo - Stella Artois VC','Cobranza Tarjetas - Stella Artois VC','Cobranza Mercado Pago - Stella Artois VC','Pedidos Ya - Stella Artois VC','Pedidos Ya efectivo - Stella Artois VC','Rappi - Stella Artois VC','Rappi efectivo - Stella Artois VC','diferencia por cobranza - Stella Artois VC','diferencia por cobranza - Stella Artois VC','Impuesto Interno - Stella Artois VC','IVA - Stella Artois VC','Venta - Stella Artois VC')
  cont_vend_s_vc$Div. <- rep('A088', nrow(cont_vend_s_lp))
  cont_vend_s_vc$CECO <- rep('A088060005', nrow(cont_vend_s_lp))
  
  return(cont_vend_s_vc)
  
}




