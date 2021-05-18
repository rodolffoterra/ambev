# # # # # # # # # 
#     Server    #
# # # # # # # # # 

# Project: filter SQL

# Company: Ambev

# 1.0 Laoind Package----

library(shiny)
library(tidyverse)
library(readxl)


# 2.0 Server----

server = function(input, output){
  output$contents <- renderTable({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    
    sheet <- ifelse(input$text == "Enter text...",
                    1,
                    input$text)
    read_excel(paste(inFile$datapath, ext,  sep="."), 
               sheet = sheet)
  })
  
  
  output$contents1 <- renderTable({
    inFile <- input$file1
    
    if(is.null(inFile))
      return(NULL)
    
    ext <- tools::file_ext(inFile$name)
    file.rename(inFile$datapath,
                paste(inFile$datapath, ext, sep="."))
    
    sheet <- ifelse(input$text == "Enter text...",
                    1,
                    input$text)
    read_excel(paste(inFile$datapath, ext,  sep="."), 
               sheet = sheet)%>% 
      group_by(input$gb) %>%
      summarise(Count = sum(input$sum)) %>%
      arrange(desc(.$Count))
      
  
  })
  
}