# # # # # # # # # 
#     Server    #
# # # # # # # # # 

# Project: Mercado pago

# Company: Ambev

# 1.0 Laoind Package----

library(shiny)
library(rJava)
library(readxl) # Load file
library(tidyverse) #Manipulation Str
library(openxlsx)


# 2.0 Server----

server <- function(input, output) {
  
  values <- reactiveValues()
  
  
  # Page 1 - Intercompany----

  output$tableIntercompany<- renderTable({
    
 
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "zip", "Please upload a .zip file"))
    
    df <- read_excel(unzip(file$datapath), sheet = "SAP")
    
    # 3.0 Data Organizy----
    
    row <- nrow(df)
    
    names <- as.list(df[4,])
    
    colnames(df) <- names
    
    df <- df[-c(1:4),]
    
    df$Texto_01 <- df$Texto
    
    # 4.0 SQL---
 
    
    # 4.1 Organiza Date
    
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
    
    
    df$texto_02<- df$Texto_01
    
    df$texto_02<- gsub("D","",df$texto_02)
    
    df$day <- str_sub(df$texto_02, start= str_count(df$texto_02)-7,
                        end = str_count(df$texto_02))
    
    df$day_01 <- str_sub(df$day, start= 0,
                           end = 2)
    
    df$day_01<- gsub("[áéíóúãiõâêîôû]","0",df$day_01)
    
    #df$day_01 <- str_sub(df$day_01, start= 2,
    #                       end = 2)
    
    
    # 4.5 df----
    
    
    df <- df %>%
      mutate(Data = ifelse(is.na(Year),"Sem Vencimento",
                           paste0(day_01,"/",month_01, "/", Year)))
    
    # 5.0 Remove Columns----
    
    df$month <- NULL 
    df$month_01 <- NULL
    df$count <- NULL
    df$Texto_01 <- NULL
    df$Year <- NULL
    df$texto_02 <- NULL
    df$day <- NULL
    df$day_01 <- NULL
    
    df$Id <- c(5:row)
    
    #df[,c(30,16,29)]
    
    values$b <- df[,-30]
    values$a <- df[,c(30,16,29)]
    
    
    #ifelse(input$radio == "Intercompany",
    #View(values$a),
    #print(values$b))
    
  })
  

  output$downloadDatainter<- downloadHandler(
    filename = function() {
      paste("Intercompany-", Sys.Date(), ".xlsx", sep="")
    },
    content = function(file) {
      write.xlsx(values$b, file)
    }
  )
  

  
  # Page 2 - Mercado Pago----
  

    
  output$tablemp<- renderTable({
    
    
    file_mp <- input$file2
    ext <- tools::file_ext(file_mp$datapath)
    
    req(file_mp)
    validate(need(ext == "zip", "Please upload a .zip file"))
    
    df_sql <- read_excel(unzip(file_mp$datapath))
    
    # 3.0 Data Cleaning---
    
    df_sql$Importe <- as.numeric(df_sql$Importe) # Converte Columns in numeric
    
    df_sql$`Tipo de Operación`<- stringr::str_to_lower(df_sql$`Tipo de Operación`) # Converte all Sentence in lower 
    
    
    df_sql$`Tipo de Operación`<- gsub("anulación parcial de ","", df_sql$`Tipo de Operación`) # remove string in anyone
    df_sql$`Tipo de Operación`<- gsub("anulación de ","", df_sql$`Tipo de Operación`)
    df_sql$`Tipo de Operación`<- gsub("devolución parcial de ","", df_sql$`Tipo de Operación`)
    df_sql$`Tipo de Operación`<- gsub("devolución de ","", df_sql$`Tipo de Operación`)
    df_sql$`Tipo de Operación`<- gsub("movimiento general","retención impuesto al valor agregado", df_sql$`Tipo de Operación`)
    df_sql$`Tipo de Operación`<- gsub("percepción general impuesto al valor agregado","retención impuesto al valor agregado", 
                                  df_sql$`Tipo de Operación`) #Remove string in other string
    
    
    # 4.0 SQL---
    
    df_sql <- df_sql %>%
      group_by(`Tipo de Operación`) %>%
      summarise(n = sum(Importe))
    
    colnames(df_sql) <- c("Etiquetas de Fila",
                          "Suma de Importe")
    
    values$c <- df_sql
    
    
  })
  

  

  
  #ifelse(input$radio == "Intercompany",
  #View(values$a),
  #print(values$b))
  


output$downloadmp<- downloadHandler(
  filename = function() {
    paste("Mercado_Pago", Sys.Date(), ".xlsx", sep="")
  },
  content = function(file2) {
    write.xlsx(values$c, file2
               )
  }
)
  
  
  
output$tablesum<- renderTable({
  
  req(values$c)
  
  Sum_df <- data.frame(Soma= "Total general",
             Valores = sum(values$c$`Suma de Importe`))
})
  
  
  
  
  
  
  
  
  

  
}