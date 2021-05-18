# # # # # # # # # 
#      u i      #
# # # # # # # # # 

# Project: filter SQL

# Company: Ambev


# 1.0 Laoind Package----

library(shinydashboard)
library(shinythemes)
library(rJava)
library(readxl)


# 2.0 Server----


ui <- fluidPage(
  titlePanel("Use readxl"),
  sidebarLayout(
    sidebarPanel(
      fileInput('file1', 'Choose xlsx file',
                accept = c(".xlsx")
      )
    ),
    mainPanel(
      tableOutput('contents'))
  )
)