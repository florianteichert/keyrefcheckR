library(shiny)
library(readxl)
library(tidyverse)
library(rclipboard)
library(janitor)
library(DT)
library(shinyjs)
library(bibliometrix)

options(shiny.maxRequestSize = 50 * 1024^2)

server <- function(input, output) {

  output$download_picos_template <- downloadHandler(
    filename = "picos_template.xlsx",
    content = function(file) {
      file.copy("picos_template.xlsx", file)
    }
  )
  
  observeEvent(input$upload_mesh, {
    req(input$upload_mesh)
    
    pubreminer <- read.delim(input$upload_mesh$datapath, header = FALSE, comment.char = "#")
    
    mesh_terms <- pubreminer %>%
      select(V2 = V2, V1 = V1) %>%
      rename(mesh_freq = V1, mesh_terms = V2) %>%
      filter(grepl("\\[mh\\]|\\[sh\\]", mesh_terms)) %>%
      mutate(mesh_terms = tolower(mesh_terms),
             id = 1:n())
    
    output$copy_mesh_terms <- renderUI({
      rclipButton(
        inputId = "copy_mesh_terms",
        label = "Copy Mesh Terms",
        clipText = paste(mesh_terms$mesh_terms, collapse = "\n"),
        icon = icon("clipboard")
      )
    })
  })
  
  
  output$instruction_1 <- renderText({
    HTML("Optional: Extract MeSH terms from a .txt file from <a href='https://hgserver2.amc.nl/cgi-bin/miner/miner2.cgi' target='_blank'>PubReminer</a>.<br>
         <a href='https://youtu.be/wQ79ajl8Cjs' target='_blank'>How to export the file?</a>")
  })
  
  output$instruction_2 <- renderText({
    HTML("Run search in <a href='https://pubmed.ncbi.nlm.nih.gov/' target='_blank'>PubMed</a> and export search results.<br>
         <a href='https://youtu.be/wQ79ajl8Cjs' target='_blank'>How to export citations?</a>")
  })

  
  output$display_picos <- renderText({
    
    picos <- input$upload_picos
    if (is.null(picos))
      return(NULL)
    
    picos <- readxl::read_excel(picos$datapath, sheet = "copy final PICOS here")
    picos <- picos %>%
      map(~ .[!is.na(.)]) %>%
      keep(~ length(.) > 0) %>%
      map(~ {
        case_when(
          str_detect(., "\\[mh\\]") ~ paste0("\"", str_replace(., "\\[mh\\]", ""), "\"[MeSH Terms]"),
          str_detect(., "\\[mh:noexp\\]") ~ paste0("\"", str_replace(., "\\[mh:noexp\\]", ""), "\"[MeSH Terms:noexp]"),
          str_detect(., "\\[majr\\]") ~ paste0("\"", str_replace(., "\\[majr\\]", ""), "\"[MeSH Major Topic]"),
          str_detect(., "\\[majr:noexp\\]") ~ paste0("\"", str_replace(., "\\[majr:noexp\\]", ""), "\"[MeSH Major Topic:noexp]"),
          str_detect(., "\\[sh\\]") ~ paste0("\"", str_replace(., "\\[sh\\]", ""), "\"[MeSH Subheading]"),
          str_detect(., "\\[sh:noexp\\]") ~ paste0("\"", str_replace(., "\\[sh:noexp\\]", ""), "\"[MeSH Subheading:noexp]"),
          str_detect(., "~\\d+") ~ paste0("\"", str_replace(., "~\\d+", ""), "\"[Title/Abstract: ~", str_extract(., "\\d+"), "]"),
          TRUE ~ paste0("\"", ., "\"[Title/Abstract]")
        )
      }) %>%
      map(~ str_replace_all(., "&", "and")) %>% 
      map(~ paste(., collapse = " OR ")) %>%
      paste0("(", ., ")") %>%
      paste(collapse = " AND ")
    
    output$copy_picos <- renderUI({
      rclipButton(
        inputId = "copy_picos",
        label = "Copy PICOS",
        clipText = picos, 
        icon = icon("clipboard")
      )
    })
    return(picos)
  })
  
  output$download_example_ref_studies <- downloadHandler(
    filename = "example_ref_studies.csv",
    content = function(file) {
      file.copy("example_ref_studies.csv", file)
    }
  )
  
  observeEvent(input$upload_ref_studies, {
    req(input$upload_ref_studies)
    
    infile <- input$upload_ref_studies
    ref_studies <- read_csv(infile$datapath) %>% clean_names()
    
    if ("pmid" %in% names(ref_studies)) {
      pmid_list <- paste(na.omit(ref_studies$pmid), collapse = ",")
      
      output$copy_pmid <- renderUI({
        rclipButton(
          inputId = "copy_pmid",
          label = "Copy PMIDs",
          clipText = pmid_list,
          icon = icon("clipboard")
        )
      })
    }
  })
  
  
  output$display_missing_PMID <- DT::renderDT({
    
    if (is.null(input$upload_ref_studies)) {
      return(NULL)
    }
    
    infile <- input$upload_ref_studies
    ref_studies <- read_csv(infile$datapath) %>% clean_names()
    
    ref_studies_pmid <- ref_studies$pmid
    
    if (any(is.na(ref_studies_pmid))) {
      
      output$text_missing_PMID <- renderText({
        HTML("<b><font color='red'>Warning!</font></b> References below do not contain a PubMed Identifier (PMID) and will not be considered for search validation")
      })
      
      no_pmid <- which(is.na(ref_studies_pmid)) 
      no_pmid_table <- ref_studies[no_pmid,] %>% 
        mutate(authors = word(authors), url = word(ur_ls, sep = ";"), publication_year = as.character(publication_year)) %>%
        mutate(url = paste0("<a href='", url,"' target='_blank'>", url,"</a>")) %>%
        arrange(publication_year) %>% 
        select("First author" = authors, Journal = journal, "Publication year" = publication_year, URL = url) 
      
      return(no_pmid_table)
      
    } else {
      
      output$text_missing_PMID <- NULL
      return(NULL)
      
    }
    
  }, options = list(dom = "t", scrollY="13vh", ordering = FALSE), escape = FALSE)
  
  
  output$display_search_metrics <- renderTable({
    
    if (is.null(input$upload_ref_studies) || is.null(input$upload_search_results)) {
      return(NULL)
    }
    
    infile <- input$upload_ref_studies
    ref_studies <- read_csv(infile$datapath) %>% clean_names() 
    
    ref_studies_pmid <- ref_studies$pmid
    
    infile <- input$upload_search_results

    search_results <- convert2df(file = infile$datapath, dbsource = "pubmed", format = "plaintext") %>% clean_names() %>% tibble()
   
    ref_studies_pmid <- na.omit(ref_studies_pmid)
    
    results <- sapply(ref_studies_pmid, function(a_list) {
      if (any(search_results$pmid == a_list)) {
        return(c(a_list, 1))
      } else {
        return(c(a_list, 0))
      }
    })
    
    included <- sum(results[2,] == 1)
    not_included <- sum(results[2,] == 0)
    total_results <- nrow(search_results)
    sensitivity <- included / (included + not_included) 
    precision <- included / total_results 
    number_needed_to_read <- as.character(round(1 / precision, digits = 0)) 
    
    search_metrics <- data.frame("Total results" = total_results, Included = included, "Not included" = not_included, 
                                 Sensitivity = sensitivity, Precision = precision, "Number needed to read" = number_needed_to_read
                                 , check.names = FALSE)
    
    search_metrics$Sensitivity <- scales::percent(search_metrics$Sensitivity)
    search_metrics$Precision <- scales::percent(search_metrics$Precision)
    
    
    output$text_search_metrics <- renderUI({
      tags$h4("Search metrics")
    })
    
    output$display_missing_studies <- DT::renderDT ({ 
      if(not_included > 0) {
        
        shinyjs::show("download_studies_not_included")
        
        output$text_missing_studies <- renderText({
          "Reference studies not included in the PubMed search results"
        })
        
        refs_not_included <- ref_studies %>% 
          filter(pmid %in% results[1, results[2,] == 0]) 
        
        write_csv(refs_not_included, "references_not_included.csv")
        
        refs_not_included <- refs_not_included %>%
          mutate(authors = word(authors), url = paste0("https://pubmed.ncbi.nlm.nih.gov/", pmid), publication_year = as.character(publication_year)) %>%
          arrange(publication_year) 
        
          output$download_studies_not_included <- downloadHandler(
            filename = "references_not_included.csv",
            content = function(file) {
              file.copy("references_not_included.csv", file)
            }
          )
        
        refs_not_included_disp <- refs_not_included %>% 
          mutate(url = paste0("<a href='", url,"' target='_blank'>", url,"</a>")) %>% 
          select("First author" = authors, Journal = journal, "Publication year" = publication_year, URL = url)
        
        return(refs_not_included_disp)
        
      }
      shinyjs::hide("download_studies_not_included")
      return(NULL)
    }, options = list(dom = "t", scrollY="13vh", ordering = FALSE), escape = FALSE)
    
    output$link_search_metrics <- renderText({
      HTML("Sensitivity is defined as the number of relevant reports identified divided by the total number of relevant reports 
             in the resource (in this case the reference studies). Precision is defined as the number of relevant reports identified divided by the total number of reports identified. <a href='https://training.cochrane.org/handbook/current/chapter-04#section-4-4-3' target='_blank'>Cochrane Handbook</a><br><br>Learn more about search metrics <a href='https://www.ncbi.nlm.nih.gov/pubmed/29526555' target='_blank'>here</a>.")
    })
    
    return(search_metrics)
    
  })
}

