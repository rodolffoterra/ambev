# # # # # # # # # 
#      u i      #
# # # # # # # # # 

# Project: Intercompany

# Company: Ambev

# Date: May 13 2021


# 1.0 Laoind Package----

library(shinydashboard)
library(shinythemes)



# 2.0 Server----


ui <- dashboardPage(
  
  # 2.1 Header
  dashboardHeader(
    title = "Ambev"
    
    
  ),
  # # # # # # # # #
  
  
  # 2.2 Sidebar----
  dashboardSidebar(
    fileInput("file1", "Carregar o Arquivo Excel", accept = "zip"),
    mainPanel(br()
              ,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
              img(src="https://raw.githubusercontent.com/rodolffoterra/rodolffoterra.github.io/main/images/ambev_teck.png", width = 200),
              br(),br(),
              h6("Rodolfo Terra  Engenheiro de Dados", style="text-align: right;")
    )
    
  ),
  # # # # # # # # #
  
  # 2.3 Sidebar
  
  dashboardBody(    
    fluidRow(
      br(),
      h5(em(strong("Patagônia", style="color:darkblue;font-size:210%")),align = "center"),
      
      h5(tabsetPanel(type = "tabs",
                     tabPanel("Intercompany",
                              HTML("<br><br>"),
                              h5("Carregue o arquivo Excel entrando no Browser"),
                              tableOutput("contents")
                     ),
                     tabPanel("Download",
                              HTML("<br><br>"),
                              tableOutput("contents1"),
                              # Button
                              downloadButton("downloadData", "Download")
                     )),
         style="padding-left: 20px;"
      )
      
      
    )
  ))
