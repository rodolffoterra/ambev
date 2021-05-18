library(rJava)
library(readxl)
library(tidyverse)

# Descobrir apenas a estensão do arquivo
# tools::file_ext("dados/UrbanPop.xlsx")


a <- excel_sheets("dados/UrbanPop.xlsx")

df <- read.csv("dados/iris.csv")

df <- iris

gb <- "Species"

lst <- names(df)


df %>% 
  group_by(gb) %>%
  summarise(Count = sum(Petal.Width)) %>%
  arrange(desc(.$Count))
  
