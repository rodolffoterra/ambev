##############
# Date: 05 12 2021
# Project: Cleaning Date
##############


# 1.0 Packate----

library(rJava)
library(readxl) # Load file
library(tidyverse) #Manipulation Str
library(openxlsx)
library(data.table)



# 2.0 Load File ----

df <- read_excel("AR21/Data/AR21.xlsx", sheet = "SAP")

# 3.0 Data Organizy----

names <- as.list(df[4,])

colnames(df) <- names

df <- df[-c(1:4),]

length(levels(as.factor(df$Texto)))

# 4.0 SQL---

# Unique Value in Texto Columns
length(unique(df[,"Texto"]))

# 4.1 Organiza Date

df$Texto <- gsub("[A-z]","",df$Texto)
df$Texto <- gsub(" ","",df$Texto)
df$Texto <- gsub("[(]","",df$Texto)
df$Texto <- gsub("[)]","",df$Texto)
df$Texto <- gsub("[-]","/",df$Texto)
df$Texto <- gsub("[.]","/",df$Texto)


date <- df


date$Texto <- date %>%
  mutate(Texto = ifelse(str_sub(Texto, 1,1) =="/",str_remove(Texto, "/"),Texto),
         Texto = ifelse(str_sub(Texto, 1,1) =="/",str_remove(Texto, "/"),Texto),
         Texto = ifelse(str_sub(Texto, 1,1) =="/",str_remove(Texto, "/"),Texto),
         Texto = ifelse(str_sub(Texto, 1,1) =="/",str_remove(Texto, "/"),Texto),
         Texto = ifelse(str_sub(Texto, 1,1) =="/",str_remove(Texto, "/"),Texto),
         Texto = ifelse(str_sub(Texto, 1,1) =="/",str_remove(Texto, "/"),Texto)
           )

date$Texto <- date %>%
  mutate(Texto = ifelse(str_sub(Texto, -1,-1) =="/",str_sub(Texto, end = -2),str_sub(Texto, end = -1)))


texto <- "/30/06/2020///"

str_sub(texto, -1,-1)
str_sub(texto, end = -1)



date$Texto <- substr(date$Texto, start=(str_locate(date$Texto, "202")[1]-6), stop=(str_locate(date$Texto, "202")[2]+3))




# Top Ok. Down not ok, It's model 

texto <- "/30/06/2020"

substr(texto, start=str_locate(texto, "202")[1]-6, stop=str_locate(texto, "202")[2]+1)

str_locate(texto, "202")[2]

head(substr(df$Texto, star = (str_locate(df$Texto, "/20")[1])-4,  stop=(str_locate(df$Texto, "/20")[2])+4))

df$Texto <- substr(df$Texto, star = (str_locate(df$Texto, "/202")[1])-5,  stop=(str_locate(df$Texto, "/20")[2])+2)



      
#####



# 4.0 Write DataFrame----

write.xlsx(df, "AR21/Data/AR21_text.xlsx")
write.xlsx(df, file.choose())






