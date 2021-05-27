#1.0 Package
library(tabulizer)



f2 <- "Targeta/Dados/resumen_mensual_202104.pdf"
lst<- extract_tables(f2, pages = 2)

renames <- function(x)
{
  colnames(x) <- x[1,]
  x <- x[2:dim(x)[1],,drop=F]
  return(as.data.frame(x))
}
#Apply
lst21 <- lapply(lst,renames)
#Bind all
df <- do.call(rbind,lst21)



#2.0 Package

data <- data.frame(x1 = 1:5,    # Example data
                   x2 = LETTERS[1:5],
                   x3 = 3,
                   x4 = c("Hello", "What's up", "AAA", "x", "Y"))
data 


library(gridExtra)



pdf("data_gridExtra.pdf")       # Export PDF
grid.table(data)
dev.off()




install.packages("stargazer")   # Install & load gridExtra
library("stargazer")

stargazer(data,                 # Export txt
            summary = FALSE,
            type = "text",
            out = "data_stargazer_txt.txt")



# 3.0 Package

library(tidyverse)
library(pdftools)

data <- pdf_text("Targeta/Dados/resumen_mensual_202104.pdf") %>% 
  str_split("\n")


for(i in 1:5) { #sets the iteration to go through all 17 pages
  data[[i]] <- data[[i]][-1:-10]
}


for(i in 5) {
  data[[i]] <- data[[i]][-1:-10]
  a <- length(data[[i]])
  b <- a-8
  data[[i]] <- data[[i]][-b:-a]
}



for(i in 1:length(data)) {
  data[[i]][1] <- data[[i]][1] %>%
    stringr::str_extract("(?<=c[:punct:]\\\").*")
}


numbers_ex = list()
for(i in 1:length(data[[i]])){
  numbers <- data[[i]] %>% str_extract("[:digit:]+.*")
  numbers_df <- data.frame(numbers)
  numbers_ex[[i]] <- numbers_df
  NH_numbers <- dplyr::bind_rows(numbers_ex)
}

