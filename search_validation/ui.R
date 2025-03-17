library(shiny)
library(readxl)
library(rclipboard)
library(janitor)
library(tidyverse)
library(revtools)
library(DT)
library(shinyjs)

ui <- fluidPage(
  
  titlePanel("PubMed search strategy validation"),
  
  useShinyjs(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      tags$h4("1. Build your PICOS"),
      htmlOutput("instruction_1"),
      
      br(),
      
      downloadButton("download_picos_template", label = "Download PICOS Template"),
      
      # tags$a("Go to Word Frequency Analyzer", href="https://sr-accelerator.com/#/wordfreq", target="_blank"),  
      
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
      tags$p("Reference studies have to be in a .csv file with a column named PMID containing the individual PubMed IDs."), # example file?
      
      fileInput("upload_ref_studies",
                "Load reference studies (.csv)",
                accept = c(".csv") # ".nbib"write coder bei nbib column umbennent pubmed_id -> pmid
      ),
      
      fileInput("upload_search_results",
                "Load PubMed search results (.nbib)",
                accept = c(".nbib") 
      )
    ),
    
    mainPanel(
      
      #htmlOutput("text_missing_PMID"),
      htmlOutput("text_missing_PMID"),
      DT::DTOutput("display_missing_PMID"),
      
      br(), 
      br(),
      
      textOutput("text_missing_studies"),
      DT::DTOutput("display_missing_studies"),
      
      br(),
      
      # uiOutput("open_missing_studies"),
      #uiOutput("copy_pmids_not_included"),
      #uiOutput("download_studies_not_included"),
      
      hidden(downloadButton("download_studies_not_included", label = "Download references")),
    
      br(),
      br(),
      
      htmlOutput("text_search_metrics"),
      tableOutput("display_search_metrics"),
      htmlOutput("link_search_metrics")
    )
  )
)
