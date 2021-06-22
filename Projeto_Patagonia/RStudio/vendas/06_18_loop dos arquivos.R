# 1.0 Packages----

source('script/bd.R')
pacman::p_load('shiny', 'rJava', 'readxl','tidyverse','xlsx','RTerra')


# 2.0 Information----

path <- dir("../Version_1/Fontes/Base_de_dados")
file <- "Bariloche.xlsx"
start_date <- '05-04-2021'
#end_date  <- format(Sys.Date(), "%d-%m-%Y")
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


# bar
list <- c()

for ( k in 1:length(path)){
  a <- str_sub(path[k], start = 1,
               end = str_locate(path[k], ".x")[1]-1)
  list <- append(list,a)
  
}


# 3.0 Load Data (Bariloche)----

df_bariloche<- df_bariloche(path = path, file = file)
