##############
# Date: 05 13 2021
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

row <- nrow(df)

names <- as.list(df[4,])

colnames(df) <- names

df <- df[-c(1:4),]

df$Texto_01 <- df$Texto

df$Id <- c(5:row)

length(levels(as.factor(df$Texto_01)))

# 4.0 SQL---

# Unique Value in Texto_01 Columns
length(unique(df[,"Texto_01"]))

# 4.1 Organiza Date

df$Texto_01 <- gsub("[A-z]","",df$Texto_01)
df$Texto_01 <- gsub(" ","",df$Texto_01)
df$Texto_01 <- gsub("[(]","",df$Texto_01)
df$Texto_01 <- gsub("[)]","",df$Texto_01)
df$Texto_01 <- gsub("[-]","/",df$Texto_01)
df$Texto_01 <- gsub("[.]","/",df$Texto_01)
df$Texto_01 <- gsub("[/]","D",df$Texto_01)


View(df$Texto_01)

date <- df

# 4.2 Year---
date$Year <- str_sub(date$Texto_01, start = str_locate(date$Texto_01, "D202")+1, end = (str_locate(date$Texto_01, "D202")[2]+1))

# 4.3 Month---

date$month <- str_sub(date$Texto_01, start = str_locate(date$Texto_01, "D202")-2, end = (str_locate(date$Texto_01, "D202")[2]-6))
date$month_01 <- str_sub(date$month, start = 0, end = 2)

date$month_01<- gsub("D","",date$month_01)

date<- date %>%
  mutate(count = str_count(month_01))

date <- date %>%
  mutate(month_01 = ifelse(str_count(month_01) ==2, month_01,
                        paste0("0",month_01)
  ))

# 4.4 Day----


date$texto_02<- date$Texto_01

date$texto_02<- gsub("D","",date$texto_02)

date$day <- str_sub(date$texto_02, start= str_count(date$texto_02)-7,
                    end = str_count(date$texto_02))

date$day_01 <- str_sub(date$day, start= 0,
                    end = 2)

date$day_01<- gsub("[áéíóúãiõâêîôû]","0",date$day_01)

#date$day_01 <- str_sub(date$day_01, start= 2,
#                       end = 2)


# 4.5 Date----


date <- date %>%
  mutate(date = ifelse(is.na(Year),"Sem Vencimento",
    paste0(day_01,"/",month_01, "/", Year)))




#date<- date %>%
#  mutate(day = str_sub(Texto_01, end = str_locate(Texto_01, "D202")[2]+1))



# 5.0 Remove Columns----

date$month <- NULL 
date$month_01 <- NULL
date$count <- NULL
date$Texto_01 <- NULL
date$Year <- NULL
date$texto_02 <- NULL
date$day <- NULL
date$day_01 <- NULL

# 6.0 Write DataFrame----

write.xlsx(date, "AR21/Data/AR21_text.xlsx")
write.xlsx(df, file.choose())

# Parte de teste


#lista <- "21"



#str_count(lista)-8

#str_sub(lista, start= 2,
#        end = 2)


#(str_locate(lista, "D202"))

#str_sub(lista, start = (str_locate(lista, "D202")[1]-5),
#        end = (str_locate(lista, "D202")[1]-4)
#         )


#str_sub(lista, end = str_locate(lista, "D202")[2]+1)





#head(substr(list_date, start=(str_locate(list_date, "D202")[1]), stop=(str_locate(list_date, "D202")[2]+1)))


#head(substr(date$Texto_01, start=(str_locate(date$Texto_01, "D202")[1]), stop=(str_locate(date$Texto_01, "D202")[2]+1)))



#lista <- "D30D06D2020DDD"

#substr(lista, start=(str_locate(lista, "D202")[1]), stop=(str_locate(lista, "D202")[2]+1))





#date$Texto_01 <- date %>%
#  mutate(Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01)
#           )

#date$Texto_01 <- date %>%
#  mutate(Texto_01 = ifelse(str_sub(Texto_01, -1,-1) =="/",str_sub(Texto_01, end = -2),str_sub(Texto_01, end = -1)))


#Texto_01 <- "/30/06/2020///"

#str_sub(Texto_01, -1,-1)
#str_sub(Texto_01, end = -1)



#date$Texto_01 <- substr(date$Texto_01, start=(str_locate(date$Texto_01, "202")[1]-6), stop=(str_locate(date$Texto_01, "202")[2]+3))




# Top Ok. Down not ok, It's model 

#Texto_01 <- "/30/06/2020"

#substr(Texto_01, start=str_locate(Texto_01, "202")[1]-6, stop=str_locate(Texto_01, "202")[2]+1)

#str_locate(Texto_01, "202")[2]

#head(substr(df$Texto_01, star = (str_locate(df$Texto_01, "/20")[1])-4,  stop=(str_locate(df$Texto_01, "/20")[2])+4))

#df$Texto_01 <- substr(df$Texto_01, star = (str_locate(df$Texto_01, "/202")[1])-5,  stop=(str_locate(df$Texto_01, "/20")[2])+2)



      
#####









