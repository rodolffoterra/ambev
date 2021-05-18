# # # # # # # # # 
#     Server    #
# # # # # # # # # 

# Project: filter SQL

# Company: Ambev

# 1.0 Laoind Package----

library(shiny)
library(tidyverse)
#library(rJava)
#library(readxl)


# 2.0 Server----

server <- function(input, output) {
  output$contents <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    read.csv(file$datapath, header = input$header)
  })
  
  output$contents1 <- renderTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    req(file)
    
    filter_sql <- read.csv(file$datapath) %>% 
      group_by(Species) %>%
      summarise(Count = sum(Petal.Width)) %>%
      arrange(desc(.$Count))
    
    print(filter_sql)
    
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("Intercompany", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filter_sql, file, row.names = FALSE)
    }
  )


  
  
}