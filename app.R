#Load packages ----
library(shiny)
library(shinydashboard)
library(tidyverse)
library(RColorBrewer)
library(data.table)
library(httr)
library(jsonlite)
library(DT)
library(crosstalk)


# APP SETTINGS ---------------------------------------------------------------- 

if(!file.exists("settings.R")){
  source("settings-default.R")} else {
    source("settings.R")} # To customise settings, please create settings.R

# LOAD GRAPHQL ----------------------------------------------------------------

source("graphql_functions/getAssessments.R")

# LOAD UTILITY ----------------------------------------------------------------


source("utility_functions/inactivity.R")


inactivity = inactivity(timeoutSeconds)


# USER INTERFACE ##------------------------------------------------------------

ui <- dashboardPage(skin = "purple",
                    title = "MHIRA",
                    
                    # HEADER ------------------------------------------------------------------ 
                    dashboardHeader(
                      title = tags$a(href = 'http://mhira.app',
                                     tags$img(src = 'mhira_logo.png', height = '50', width = '150'),
                                     'MHIRA')
                    ),
                    
                    # SIDEBAR ------------------------------------------------------------------
                    dashboardSidebar(
                      width = 250,
                      collapsed = FALSE,
                      tags$script(inactivity), # For timeout
                      
                      tags$script(HTML( # This javascript code gets data from localStorage of the browser
                        "$(document).on('shiny:connected', function() {
            const LS = window.localStorage.getItem('auth_app_token');
            Shiny.setInputValue('accessToken', LS);
            const CL = window.localStorage.getItem('currentLang');
            Shiny.setInputValue('currentLang', CL);
            });"
                      )),
            
            uiOutput("dateRangeSelector")

            
                    ),
            
            # BODY -------------------------------------------------------------------
            dashboardBody(
              
              #  includeCSS("www/myCSS.css"),
              fluidRow(
                h1("Assessments by user"),
                DTOutput("outputTable", width = "60%")

                
                
                
                
              ) 
              
            )
            
# CLOSE USER INTERFACE UI ---------------------------------------------------
            
)

## SERVER ## ----------------------------------------------------------------- 

server = function(input, output, session) {
  
  # OBSERVE INACTIVITY AND CLOSE APP ------------------------------------------  
  
  observeEvent(input$timeOut, {
    print(paste0("Session was closed at", " ", Sys.time()))
    showModal(modalDialog(
      title = "Timeout",
      paste("Session was closed afer",
            input$timeOut
      ),
      footer = NULL
    ))
    session$close()
  })
  
  
  
  # STORE LOCAL STORAGE EXTRACTED TOKEN TO SHINY SESSION OBJECT ----------------
  observe({ 
    print("writing token to session object")
    session$userData  = fromJSON(input$accessToken)$accessToken
  }) %>%  bindEvent(input$accessToken)
  
  
  
  # GET ASSESSMENT DATA --------------------------------------------------------
  
  assessments = reactiveVal() # the imported data as dataframe
  
  
  observe({
    req(!is_empty(session$userData))
    print("get patient report via graphql")
    
    assessments = getAssessments(token = session$userData, url = url)
      
    if(is_empty(assessments)){
      showNotification(
        "No data obtained from MHIRA",
        type = "error",
        duration = 20)
      session$close()}
    
    assessments(assessments)
    
    print("data has been obtained from API")
    
  }) %>%  bindEvent(input$accessToken)
  
  
  # MAKE PLOTS
  
  df = reactiveVal() # the imported data as dataframe
  
  observe({
    req(!is_empty(assessments()))
    print("reformatting data and creating date range widget")
    
    assessments = assessments()
    
    df = assessments %>%
      unnest(cols = clinician) %>%
      mutate(
        clinician = paste(firstName, lastName, sep = " "),
        createdAt = lubridate::as_datetime(createdAt),
        updatedAt = lubridate::as_datetime(updatedAt),
        submissionDate = lubridate::as_datetime(submissionDate),
        dateTime = createdAt,
        status = factor(status, levels = c("COMPLETED", "PARTIALLY_COMPLETED", "PLANNED","CANCELLED", "OPEN_FOR_COMPLETION", "PENDING", "EXPIRED" )))  %>%
      arrange(dateTime) %>%
      mutate(questInAssessment = map(assessments$questionnaires, nrow) %>% unlist) %>%
      group_by(status) %>% 
      mutate(questCount = cumsum(questInAssessment)) %>%
      ungroup()

    df(df)
    
    output$dateRangeSelector <- renderUI({
      date_min <- min(df$createdAt)
      date_max <- max(df$createdAt)
      
      dateRangeInput("dateRange", "Custom Range:",
                     start = Sys.Date() - 30,
                     end = Sys.Date(),
                     min = date_min,
                     max = Sys.Date(),
                     format = "yyyy-mm-dd",
                     separator = " - ")
                    
      
       })
    
  })
    
   

   
  observe({

    req(!is_empty(df()))
    print("reformatting data and creating date range widget")

    df = df()
    
    df_filtered <- df %>%
      filter(createdAt > input$dateRange[1] & createdAt < input$dateRange[2]) %>%
      group_by(clinician, status) %>%
      summarise(count = n()) %>%
      pivot_wider(names_from = status, values_from = count, values_fill = 0) %>%
      arrange(desc(COMPLETED)) %>%
      ungroup()
    
        
    output$outputTable <- renderDT({
      df_filtered %>%
        DT::datatable(
          
          caption = paste("Period from ", input$dateRange[1], " to ", input$dateRange[2]),
          
          extensions = 'Buttons',
          
          options = list(
            paging = TRUE,
            searching = TRUE,
            fixedColumns = FALSE,
            autoWidth = TRUE,
            ordering = TRUE,
            dom = 'tB',
            buttons = c('copy', 'csv', 'excel')
          ),
          filter = "bottom",
          class = "display"
        )
        
         }) 
    
    print("plots and tables rendered") 
    
  }) %>% bindEvent(input$dateRange)
  
  
}

## APP ## --------------------------------------------------------------------

shinyApp(ui = ui, server = server)