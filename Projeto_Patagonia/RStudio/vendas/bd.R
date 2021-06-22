

# Data Frame Bariloche
df_all <- data.frame(matrix(nrow = 1, ncol = 38))
colnames(df_all) <- paste0('x',rep(1:38))
df_all <- df_all[-1,]


cajas <- read_excel("Fontes/Maestro de cajas ICG.xlsx")


# Lista Estabelecimentos

# bar
list <- c()

for ( k in 1:length(path)){
  a <- str_sub(path[k], start = 1,
               end = str_locate(path[k], ".x")[1]-1)
  list <- append(list,a)
  
}


#icg <- read_excel('Fontes/Iva ventas.xlsx')
icg <- read_excel("Fontes/teste.xlsx", sheet = 'ICG')

icg <- merge(x = icg, y = cajas, by.x = 'Caja',
             by.y  = 'Caja', all.x = TRUE)

icg <- icg[complete.cases(icg$Fecha), ]



cont_vend <- data.frame(a1 = c('50','50','50','01','01','01','01','01','01','01','50','40'),
                        a2 = c('4730002','4730009','5370088','30027097','30035478','30027086','30033334','30033334','30033334','30033334','6104757','6104757'),
                        a3 = rep("",12),
                        a4 = rep("",12),
                        a5 = rep("",12),
                        a6 = rep("",12),
                        a7 = rep("LATAM06000",12),
                        a8 = c('IVA DF A PAGAR - Bariloche','Impostos internos - Bariloche','VENTA - Bariloche','Cobranza Tarjeta - Bariloche','Cobranza Mercadopago - Bariloche','Cobranza efectivo - Bariloche','Cobranza Rappi - Bariloche','Cobranza Rappi efectivo - Bariloche','Cobranza Pedidos Ya - Bariloche','Cobranza Pedidos Ya efectivo - Bariloche','Diferencia entre iCG y planilla - Bariloche','Diferencia entre iCG y planilla - Bariloche'),
                        a9 = rep("A001",12),
                        a10 = rep("A001060000",12))



colnames(cont_vend) <- c('Cl. Mov',	'Cuenta',	'Ipte Mon Doc',	'Imprtorte ML',
                         'Imprtorte ML2',	'ASIGNACION',	'LATAM','Texto','Div.',	'CECO')




