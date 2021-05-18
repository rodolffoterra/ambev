
# 1.0 Packate----

library(rJava)
library(readxl) # Load file
library(tidyverse) #Manipulation Str
library(openxlsx)
library(data.table)


df <- iris


write.xlsx(df, "AR21/Data/iris.xlsx")


# File zip



df <- read_excel(unzip("AR21/Data/AR21.zip"), sheet = "SAP")

unzip("AR21/Data/AR21.zip")

