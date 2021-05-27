
# 1.0 Package

library(tidyverse)
library(pdftools)

data <- pdf_text("Targeta/Dados/resumen_mensual_202104 BARILOCHE .pdf") %>% 
  str_split("\n")


# Resumen Mensual de Liquidação

Resumen <- data[[1]][1:14]


x <- data.frame(matrix(unlist(Resumen), nrow=length(Resumen), byrow=TRUE))

x <- x %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

colnames(x) <- "a"

x <- x[-1,]

x <- x %>%
  separate(a, into = c("x1","x2"), sep = ":")

x[1,2] <- x[1,1]
x[1,1] <- gsub("[0-9]","", x[1,1])
x[1,2] <- gsub("[A-z]","", x[1,2])


# Total Presentado

Total_Presente <- gsub(" ","",str_sub(data[[1]][22], start = as.integer(str_count(data[[1]][27])/2),
                                      end = -10))

# Total Descuento

total_Descuento <- gsub(" ","",str_sub(data[[1]][27], start = as.integer(str_count(data[[1]][27])/2),
        end = -10))

# Saldo

saldo <- gsub(" ","",str_sub(data[[1]][30], start = as.integer(str_count(data[[1]][27])/2),
                    end = -10))

Resumen <- rbind(x, data.frame(x1 = c("Total Presente", "Total Descuento","Saldo"),
              x2 = c(Total_Presente, total_Descuento, saldo)))
  


end <- data[[1]][c(24,26,27,28)]


end[3] <- str_sub(end[3], start = 1, end = as.integer(str_count(end[3])/2))

end <- data.frame(matrix(unlist(end), nrow=length(end), byrow=TRUE)) %>%
  rowwise() %>%
  mutate_all(funs(str_squish(.))) %>%
  ungroup()

end[2,] <- paste(end[2,],end[3,],end[4,], sep =",")

end <- cbind(data.frame(Name = c("Razón Social", "Estabelecimento"), end[1:2,]))

colnames(end) <- c("x1",'x2')

end <- rbind(end, subset(Resumen, Resumen$x1 == "Nº DE ESTABLECIMIENTO"))


d1<- subset(Resumen, Resumen$x1 == "Nº DE ESTABLECIMIENTO")

             

