library(shiny)
library(readxl)
library(tidyverse)
library(janitor)
library(DT)
library(shinyjs)
library(bibliometrix)

ui <- fluidPage(
  
  titlePanel("PubMed search strategy validation"),
  
  useShinyjs(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$h4("1. Build your PICOS"),
      htmlOutput("instruction_1"),
      
      br(),
      
      downloadButton("download_picos_template", label = "Download PICOS Template"),
      
      br(),
      br(),
      
      fileInput("upload_picos",
                "Load PICOS",
                accept = c(".xlsx")
      ),
      
      verbatimTextOutput("display_picos"),
      
      rclipboardSetup(),
      uiOutput("copy_picos"),
      
      br(),
      
      htmlOutput("instruction_2"),

      br(),
      
      tags$h4("2. Upload reference studies and search results"),
      tags$p("Reference studies have to be uploaded in a .csv file with a specific structure (see example)."), 
      
      
      downloadButton("download_example_ref_studies", label = "Example file"),
      
      br(),
      br(),
      
      fileInput("upload_ref_studies",
                "Load reference studies (.csv)",
                accept = c(".csv") 
      ),
      
      fileInput("upload_search_results",
                "Load PubMed search results (.nbib)",
                accept = c(".nbib") 
      )
    ),
    
    mainPanel(
      
      htmlOutput("text_missing_PMID"),
      DT::DTOutput("display_missing_PMID"),
      
      br(), 
      br(),
      
      textOutput("text_missing_studies"),
      DT::DTOutput("display_missing_studies"),
      
      br(),
      
      hidden(downloadButton("download_studies_not_included", label = "Download references")),
    
      br(),
      br(),
      
      htmlOutput("text_search_metrics"),
      tableOutput("display_search_metrics"),
      htmlOutput("link_search_metrics")
    )
  )
)
