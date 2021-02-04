library(shiny)
require(nlme)
library(Hmisc)
library(blme)
library(cluster)
library(knitr)
library(kml)
library(ggplot2)
library(dplyr)
library(rmarkdown)
library(knitr)
library(agricolae)
library(lubridate)
library(plotly)
library(shinythemes)
library(shinyjs)
library(googleLanguageR)


ui <- fluidPage(
#  
  # App title ----
  titlePanel("SIGA"),

# Sidebar layout with input and output definitions ----
  navbarPage(
    theme = shinytheme("cerulean"),
    " ",
     tabPanel("SIGA",
              useShinyjs(),
             
                       mainPanel(
                        tabsetPanel(
                          
                          tabPanel("Información General",
                                   h3("Información general de asesorías académicas"),
                                   verbatimTextOutput("dateText2"),
                                   tableOutput("summary4"),
                                   h3("Semanas académicas"),
                                   dataTableOutput("tabless9"),
                                   plotOutput("pred_plot4",height = "300px", width="100%"),
                                   h3("Dias académicos"),
                                   dataTableOutput("tables3"),
                                   plotOutput("pred_plot5",height = "300px", width="100%"),
                                   dataTableOutput("tables4")
                                   # plotlyOutput("preee1",height = "600px", width="100%")
                                   ),
                          tabPanel("Asignaturas", 
                                   h4("Asesorias con repetición"),
                                   dataTableOutput("tables1"),
                                   plotOutput("preee",height = "300px", width="100%"),
                          verbatimTextOutput("table1")
                                   ),
                          tabPanel("Estudiantes", 
                                   dataTableOutput("tables2"),
                                   h4("Veces que un estudiante ha asistido a asesoria academica"),
                                   sliderInput("upper", "Valor máximo del histograma:",
                                               min = 3, max =9,
                                               value = 4, step =1),
                                   dataTableOutput("tabless5"),
                                   plotOutput("pred_plot21",height = "600px", width="100%")
                                   ),
                          tabPanel("Docentes", 
                                   verbatimTextOutput("tabless20")),
                          tabPanel("Datos", 
                                   dataTableOutput("table"))
                                                                              ))
                      )
             
        
    
    )
      
      )
       

