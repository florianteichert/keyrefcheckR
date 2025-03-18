library(shiny)
library(readxl)
library(tidyverse)
library(rclipboard)
library(janitor)
library(DT)
library(shinyjs)
library(bibliometrix)

ui <- fluidPage(
  
  titlePanel("PubMed search strategy validation"),
  
  useShinyjs(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$h4("1. Upload reference studies"),
      tags$p("Reference studies have to be uploaded in a .csv file with a specific structure (see example)."), 
      
      downloadButton("download_example_ref_studies", label = "Example file"),
      
      br(),
      br(),
      
      fileInput("upload_ref_studies",
                "Load reference studies (.csv)",
                accept = c(".csv") 
      ),
      
      uiOutput("copy_pmid"),
    
      br(),
      
      tags$h4("2. Build your PICOS"),
      tags$p("Follow instructions in the PICOS template."),
      
      downloadButton("download_picos_template", label = "Download PICOS Template"),
      
      br(),
      br(),
      
      htmlOutput("instruction_1"),
      
      br(),
      
      fileInput("upload_mesh", "Upload MeSH file (.txt)", accept = ".txt"),
      uiOutput("copy_mesh_terms"),
      
      br(),
      
      fileInput("upload_picos",
                "Load modified PICOS template",
                accept = c(".xlsx")
      ),
      
      verbatimTextOutput("display_picos"),
      
      rclipboardSetup(),
      uiOutput("copy_picos"),
      
      br(),
      
      htmlOutput("instruction_2"),

      br(),
      
      tags$h4("3. Upload search results"),
      
      br(),
      br(),
      
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
