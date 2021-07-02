#################
# Project: Intercompany

# Departament: Patagonia

# Date: 07-02-2021


# 0.0 Options

year <- 2021
month <- 6
day_start <- 5
day_end <- 10

# Function----

to.numeric <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.numeric(paste(df[[variable]]))
  }
  return(df)
}


to.missing <- function(df,variables){
  for (variable in variables){
    df[[variable]][is.na(df[[variable]])] = ""
  }
  return(df)
}


# 1.0 Package Load----

library(tidyverse)
library(readxl) # Data read
library(xlsx) # Data write

# 2.0 Data Load----

dir(paste0(getwd(),'/Fontes'))
ico <- data.frame(read_excel("Fontes/Bajada.xlsx"))

# 3.0 Data Cleaning----

ico <- ico[,(grep('Conta', ico)[1]):ncol(ico)]

# Names Nol
ncol(ico)
name <- list()

for (n in 1:ncol(ico)){
  name1 <- paste0("x",n)
  name <- append(name, name1)
} 

colnames(ico) <- name


# Remove NA
ico <- ico[complete.cases(ico$x1),]


name <- list()

for (n in 1:ncol(ico)){
  name1 <- paste0("",ico[1,n])
  name <- append(name, name1)
}

colnames(ico) <- name

# Clean NA Head 

ico <- ico[grep("Conta", ico$Conta)+1:nrow(ico),]

ico <- ico[complete.cases(ico$Conta),]

# Clean Date Columns Data Doc.

ico$Date <- ico$`Data doc.`

ico <- ico %>%
  separate(Date, into = c('d','m','y'), sep ='[.]')

#Converte numeric

number_float.vars <- c('d','m','y')

ico <- to.numeric(df = ico, variables = number_float.vars)

# Filter Date----

ico <- subset(ico, y == year & m == month & d >= day_start & d <= day_end)


excluir <- c("d","m","y")

ico <- ico[,!(names(ico)%in% excluir)]

ico$`Data doc.`<- gsub("[.]","/",ico$`Data doc.`)


# Remove the missing

names <- c("NA","Cliente","Tax Object Code","Cen.lucro","Venc.líq.", "S","DocCompens","Centro cst","Fornecedor","Pré-edição")

ico <- to.missing(ico, names)

# Ativo AR11

ar11 <- subset(ico, Empr == "AR11")
ar21 <- subset(ico, Empr == "AR21")


ref_21 <- ar21 %>%
  group_by(Referência, `Data doc.`) %>%
  summarise(sum = sum(as.numeric(`Mont.moeda doc.`)))

ref_11 <- ar11 %>%
  group_by(Referência, `Data doc.`) %>%
  summarise(sum = sum(as.numeric(`Mont.moeda doc.`)))

df <- merge(ref_11, ref_21, by = 'Referência', all.x = TRUE)

df <- df %>%
  mutate(Saldo = sum.x + sum.y)

df <- df[,c(1,2,3,6)]

colnames(df) <- c("Check Referências", "Data Correta", "Valores","Status")
                   
df <- df[,c(1,3,4,2)]

verificar <- subset(df, is.na(Status))

verificar$Status[is.na(verificar$Status)] = "Verificar"



# 4.0 Save DataSet----

file <- paste0(getwd(),'/Resultado/Intercompany_',format(Sys.Date(), "%d_%m_%Y"),'.xlsx')


write.xlsx(ar11, file = file, sheetName = "Ativo AR11", append = TRUE)
write.xlsx(ar21, file = file, sheetName = "Passivo AR21", append = TRUE)
write.xlsx(verificar, file = file, sheetName = "Verificar", append = TRUE)



