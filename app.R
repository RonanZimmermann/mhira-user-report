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

source("settings.R") # Please modify the settings file for changes to settings

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
                      title = tags$a(href = 'http://mhira-project.org',
                                     tags$img(src = 'mhira_logo.png', height = '50', width = '150'),
                                     'MHIRA')
                    ),
                    
                    # SIDEBAR ------------------------------------------------------------------
                    dashboardSidebar(
                      width = 250,
                      collapsed = TRUE,
                      tags$script(inactivity), # For timeout
                      
                      tags$script(HTML( # This javascript code gets data from localStorage of the browser
                        "$(document).on('shiny:connected', function() {
            const LS = window.localStorage.getItem('auth_app_token');
            Shiny.setInputValue('accessToken', LS);
            const CL = window.localStorage.getItem('currentLang');
            Shiny.setInputValue('currentLang', CL);
            });"
                      )),
            h1("Progress report")
            
                    ),
            
            # BODY -------------------------------------------------------------------
            dashboardBody(
              
              #  includeCSS("www/myCSS.css"),
              fluidRow(
                h1("Instruments used over time"),
                plotOutput("progress_plot"),
                br(),
                h1("Usage of questionnaires"),
                plotOutput("instrument_plot"),
                br(),
                h1("Indicators"),
                DTOutput("table", width = "30%")
                
                
                
                
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
    
    assessments(assessments)
    
    print("data has been obtained from API")
    
  }) %>%  bindEvent(input$accessToken)
  
  
  # MAKE PLOTS
  
  observe({
    req(!is_empty(assessments()))
    print("rendering plots")
    
    assessments = assessments()
    cols = c("#4DAF4A", "#E41A1C", "#377EB8", "#FF7F00", "#984EA3")
    
    df = assessments %>% 
      mutate(
        createdAt = lubridate::as_datetime(createdAt),
        updatedAt = lubridate::as_datetime(updatedAt),
        dateTime = createdAt,
        status = factor(status, levels = c("COMPLETED", "CANCELLED", "OPEN_FOR_COMPLETION","PARTIALLY_COMPLETED", "EXPIRED")))  %>%
      arrange(dateTime) %>%
      mutate(questInAssessment = map(assessments$questionnaires, nrow) %>% unlist) %>%
      group_by(status) %>% 
      mutate(questCount = cumsum(questInAssessment)) %>%
      ungroup
    
    progress = ggplot(df, aes(x = dateTime, y = questCount, group = status, linetype = status, colour = status)) + 
      geom_line(lwd = 1.3) + 
      geom_point(size = 2) + 
      ylab("cummulative number of questionnaires") + 
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) +
      theme_light() 
    
    questCount = df %>% 
      unnest(questionnaires) %>% 
      group_by(status) %>%
      count(abbreviation) %>%
      ungroup %>% 
      arrange(desc(status), abbreviation,  n) %>%
      group_by(abbreviation) %>%
      mutate(pos = cumsum(n) - 0.5 * n) 
    
    instrument = ggplot(questCount, aes(x = abbreviation, y = n, colour = status, fill = status)) +
      geom_bar(stat = "identity", alpha = 0.3) + 
      geom_text(aes(y = pos, label = n, group = status)) +
      scale_colour_manual(values = cols) +
      scale_fill_manual(values = cols) +
      ylab("count") + 
      xlab("instrument") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) 
    
    table = df %>%summarise(
      patients_with_assessments = length(patientId %>% unique()),
      completed_assessments = length(status[ status == "COMPLETED"]),
      open_assessments = length(status[ status == "OPEN_FOR_COMPLETION"]),
      partial_assessments = length(status[ status == "PARTIALLY_COMPLETED"]),
      expired_assessments = length(status[ status == "EXPIRED"]),
      planned_assessments = length(status[ status == "PLANNED"]),
      cancelled_assessments = length(status[ status == "CANCELLED"])) %>%
      pivot_longer(cols = everything(), names_to = "Object", values_to = "Count") 
    
    
    
    output$progress_plot = progress %>% renderPlot()
    output$instrument_plot = instrument %>% renderPlot()
    output$table = table %>% renderDT()
    
    print("plots and tables rendered") 
    
  }) 
  
  
}

## APP ## --------------------------------------------------------------------

shinyApp(ui = ui, server = server)