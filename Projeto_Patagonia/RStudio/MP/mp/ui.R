# # # # # # # # # 
#      u i      #
# # # # # # # # # 

# Project: Mercado Pago

# Company: Ambev

# Date: May 17 2021


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
    mainPanel(img(src="https://raw.githubusercontent.com/rodolffoterra/rodolffoterra.github.io/main/images/ambev_teck.png", width = 200),
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
                     
                     
                     # Page 1
                     tabPanel("Intercompany",
                              HTML("<br><br>"),
                              fluidRow(
                                column(6,
                              fileInput("file1", "Carregar o Arquivo Excel (zip)", accept = "zip")),
                              column(6,
                                     downloadButton("downloadDatainter", "Download")
                                     )),
                              tableOutput("tableIntercompany")
                              
                     ),
                     
                     
                     # Page 2
                     tabPanel("Mercado Pago",
                              HTML("<br><br>"),
                              fluidRow(
                                column(6,
                                       fileInput("file2", "Carregar o Arquivo Excel (zip)", accept = "zip")),
                                column(6,
                                       downloadButton("downloadmp", "Download")
                                       
                                )),
                                
                                #tableOutput("tablesum"),
                              
                              fluidRow(
                                column(8,
                                       tableOutput("tablemp")),
                                column(4,
                                       tableOutput("tablesum"))
                                )
                                       
                              
                     )
                     ),
         
         style="padding-left: 20px;"
      )
      
      
    )
  ))
