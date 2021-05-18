# # # # # # # # # 
#      u i      #
# # # # # # # # # 

# Project: filter SQL

# Company: Ambev


# 1.0 Laoind Package----

library(shinydashboard)
library(shinythemes)
library(readxl)


# 2.0 Server----


ui <- dashboardPage(
  
  # 2.1 Header
  dashboardHeader(
    title = h2("Ambev")
    
    
  ),
  # # # # # # # # #
  
  
  # 2.2 Sidebar----
  dashboardSidebar(
    textInput("text", "Sheet Name", 
              value = "Enter text..."),
    textInput("gb", "Groupby", 
              value = "Enter text..."),
    textInput("sum", "Somatória", 
              value = "Enter text..."),
    fileInput('file1', 'Choose File',
              accept = c(".xlsx")
    )
  ),
  # # # # # # # # #
  
  # 2.3 Sidebar
  
  dashboardBody(    
    fluidRow(
      br(),
      h5(em(strong("Filtro Por Categoria", style="color:darkblue;font-size:210%")),align = "center"),
      
      h5(tabsetPanel(type = "tabs",
                     tabPanel("Tabela Completa",
                              HTML("<br><br>"),
                              h5("Gráfico de Tendência das palavras."),
                              tableOutput("contents")
                     ),
                     tabPanel("Filto por Categoria",
                              HTML("<br><br>"),
                              tableOutput("contents1")
                     )),
         style="padding-left: 20px;"
      )
      
      
    )
  ))