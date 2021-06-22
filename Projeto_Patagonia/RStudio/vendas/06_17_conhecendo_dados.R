# 1.0 Packages----

pacman::p_load(tidyverse, RTerra, readxl)


# 2.0 Information----

path <- "Fontes/Base_de_dados"
file <- "Bariloche.xlsx"
start_date <- '05-04-2021'
#end_date  <- format(Sys.Date(), "%d-%m-%Y")
end_date <- '17-04-2021'

# 3.0 Load Data----

df <- read_excel(paste0(path,'/',file), sheet = 'Summary')


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


df <- subset(df, df$FECHA >= start_date & df$FECHA <= end_date)

# Table VendasBares

df <- df[,1:grep('Control cobranzas', names(df))]




# Name da Sheet




str_sub(path[1], start = 1)


str_locate(path[1], ".x")


path <- dir("../Version_1/Fontes/Base_de_dados")
length(path)
