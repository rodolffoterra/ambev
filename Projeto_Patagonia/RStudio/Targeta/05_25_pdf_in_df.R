# https://medium.com/swlh/the-adventure-of-pdf-to-data-frame-in-r-f90609035600



# 1.0 Package 

library(tidyverse)
library(pdftools)

PDF_2013 <- pdf_text("Targeta/Dados/resumen_mensual_202104.pdf") %>% 
  str_split("\n")

for(i in 1:length(PDF_2013)) {
  PDF_2013[[i]][1] <- PDF_2013[[i]][1] %>%
    stringr::str_extract("(?<=c[:punct:]\\\").*")
}


for(i in 1:length(PDF_2013)) {
  for(j in 1:length(PDF_2013)){
    PDF_2013[[i]][j] %>% str_extract(".*[:alpha:]+|\\&|\\-") %>% 
      print()#extracts the words
  }
}


names_ex = list()
for(i in 1:length(PDF_2013)) {
  words <- PDF_2013[[i]] %>% str_extract(".*[:alpha:]+|\\&|\\-") 
  words_df <- data.frame(words) #turns into data frame for list
  names_ex[[i]] <- words_df
  NH_names <- dplyr::bind_rows(names_ex)
}
print(NH_names)



numbers_ex = list()
k=1
for(i in 1:length(PDF_2013)) {
  for(j in 1:length(PDF_2013[[i]])){
    numbers <- PDF_2013[[i]][j] %>% str_extract("[:digit:]+.*")
    numbers_df <- data.frame(numbers)
    # need to figure out how to get a list of the numbers going!
    while(k <= 1000) {
      numbers_ex[[k]]<- numbers_df
      k <- k+1
      break
    }
  }
  NH_numbers <- dplyr::bind_rows(numbers_ex)
}

NH_numbers %>% 
  separate(numbers, c("A","B","C","D","E","F","G","H","I","J"), sep="\\s") -> NH_numbers
  
DF1 <- cbind(NH_names, NH_numbers)


write.xlsx(DF1, file = "extrato.xlsx",
           sheetName = "Cartao", append = FALSE, col.names = FALSE)

