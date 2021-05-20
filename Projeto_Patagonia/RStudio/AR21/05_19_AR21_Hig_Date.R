##############
# date: 05 13 2021
# Project: Cleaning df
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

# 4.1 Organiza df

df$Texto_01 <- gsub("[A-z]","",df$Texto_01)
df$Texto_01 <- gsub(" ","",df$Texto_01)
df$Texto_01 <- gsub("[(]","",df$Texto_01)
df$Texto_01 <- gsub("[)]","",df$Texto_01)
df$Texto_01 <- gsub("[-]","/",df$Texto_01)
df$Texto_01 <- gsub("[.]","/",df$Texto_01)
df$Texto_01 <- gsub("[/]","D",df$Texto_01)




# 4.2 Year---
df$Year <- str_sub(df$Texto_01, start = str_locate(df$Texto_01, "D202")+1, end = (str_locate(df$Texto_01, "D202")[2]+1))

# 4.3 Month---

df$month <- str_sub(df$Texto_01, start = str_locate(df$Texto_01, "D202")-2, end = (str_locate(df$Texto_01, "D202")[2]-6))
df$month_01 <- str_sub(df$month, start = 0, end = 2)

df$month_01<- gsub("D","",df$month_01)

df<- df %>%
  mutate(count = str_count(month_01))

df <- df %>%
  mutate(month_01 = ifelse(str_count(month_01) ==2, month_01,
                        paste0("0",month_01)
  ))

# 4.4 Day----

df$day <- str_sub(df$Texto_01, start = str_locate(df$Texto_01, "D202")-5, end = (str_locate(df$Texto_01, "D202")[2]-6))

df$day<- gsub("[áéíóúãiõâêîôû]","",df$day)

df$day <- ifelse(str_sub(df$day, start = 0, end = 1) =="D",str_sub(df$day, start = 2, end = str_count(df$day)),
                                                                       df$day)

df <- df %>%
  separate(day, into = c("day"), sep = "D")

df$day <- as.integer(df$day)

df <- df %>%
  mutate(day_01 = ifelse(day > 31, 
                   day/10,
                   day))

df$day_01[is.na(df$day_01)] = 0
df$day[is.na(df$day)] = 0


df$day_01<- ifelse(df$day - df$day_01 == 0, df$day_01,
                    (df$day_01 - as.integer(df$day_01))*10)

df$day_01 <- as.integer(df$day_01)

df$day_01 <- ifelse(df$day_01 == 0, 1, df$day_01)



# 4.5 df----

df <- df %>%
  mutate(Data = ifelse(is.na(Year),"Sem Vencimento",
                       ifelse(month_01 <= 12,
                              paste0(day_01,"/",as.character(month_01), "/", Year),
                              paste0(month_01,"/",day_01, "/", Year))))


df<- df %>%
  separate(Data, into = c('d',"m",'y' ), sep = "[/]")

df$d <- ifelse(df1$d == "1","01",
                ifelse(df1$d == "2","02",
                       ifelse(df1$d == "3","03",
                              ifelse(df1$d == "4","04",
                                     ifelse(df1$d == "5","05",
                                            ifelse(df1$d == "6","06",
                                                   ifelse(df1$d == "7","07",
                                                          ifelse(df1$d == "8","08",
                                                                 ifelse(df1$d == "9","09",df1$d)))))))))

df$m <- ifelse(df1$m == "1","01",
                ifelse(df1$m == "2","02",
                       ifelse(df1$m == "3","03",
                              ifelse(df1$m == "4","04",
                                     ifelse(df1$m == "5","05",
                                            ifelse(df1$m == "6","06",
                                                   ifelse(df1$m == "7","07",
                                                          ifelse(df1$m == "8","08",
                                                                 ifelse(df1$m == "9","09",df1$m)))))))))


df$Data <- ifelse(is.na(df$Year),"Sem Vencimento",
                    paste0(df$d,"/",df$m,"/",df$y))


#df<- df %>%
#  mutate(day = str_sub(Texto_01, end = str_locate(Texto_01, "D202")[2]+1))



# 5.0 Remove Columns----

df$month <- NULL 
df$month_01 <- NULL
df$count <- NULL
df$Texto_01 <- NULL
df$Year <- NULL
df$texto_02 <- NULL
df$day <- NULL
df$day_01 <- NULL
df$d<- NULL
df$m <- NULL
df$y <- NULL

# 6.0 Write DataFrame----

write.xlsx(df, "AR21/Data/AR21_text.xlsx")
write.xlsx(df, file.choose())

# Parte df teste


#lista <- "21"



#str_count(lista)-8

#str_sub(lista, start= 2,
#        end = 2)


#(str_locate(lista, "D202"))

#str_sub(lista, start = (str_locate(lista, "D202")[1]-5),
#        end = (str_locate(lista, "D202")[1]-4)
#         )


#str_sub(lista, end = str_locate(lista, "D202")[2]+1)





#head(substr(list_df, start=(str_locate(list_df, "D202")[1]), stop=(str_locate(list_df, "D202")[2]+1)))


#head(substr(df$Texto_01, start=(str_locate(df$Texto_01, "D202")[1]), stop=(str_locate(df$Texto_01, "D202")[2]+1)))



#lista <- "D30D06D2020DDD"

#substr(lista, start=(str_locate(lista, "D202")[1]), stop=(str_locate(lista, "D202")[2]+1))





#df$Texto_01 <- df %>%
#  mutate(Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01),
#         Texto_01 = ifelse(str_sub(Texto_01, 1,1) =="D",str_remove(Texto_01, "D"),Texto_01)
#           )

#df$Texto_01 <- df %>%
#  mutate(Texto_01 = ifelse(str_sub(Texto_01, -1,-1) =="/",str_sub(Texto_01, end = -2),str_sub(Texto_01, end = -1)))


#Texto_01 <- "/30/06/2020///"

#str_sub(Texto_01, -1,-1)
#str_sub(Texto_01, end = -1)



#df$Texto_01 <- substr(df$Texto_01, start=(str_locate(df$Texto_01, "202")[1]-6), stop=(str_locate(df$Texto_01, "202")[2]+3))




# Top Ok. Down not ok, It's modfl 

#Texto_01 <- "/30/06/2020"

#substr(Texto_01, start=str_locate(Texto_01, "202")[1]-6, stop=str_locate(Texto_01, "202")[2]+1)

#str_locate(Texto_01, "202")[2]

#head(substr(df$Texto_01, star = (str_locate(df$Texto_01, "/20")[1])-4,  stop=(str_locate(df$Texto_01, "/20")[2])+4))

#df$Texto_01 <- substr(df$Texto_01, star = (str_locate(df$Texto_01, "/202")[1])-5,  stop=(str_locate(df$Texto_01, "/20")[2])+2)



      
#####









