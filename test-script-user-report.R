### THIS SCRIPT ALLOWS YOU TO SIMULATE THE DATA FLOW OF THE SHINY APP
### IT IS USEFUL FOR TESTING NEW ELEMENTS

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

setwd("/home/mhira/shiny_apps/mhira-user-report/") # make sure you are in the app folder, else source files will not be found

if(!file.exists("settings.R")){
  source("settings-default.R")} else {
    source("settings.R")} # To customise settings, please create settings.R

# LOAD GRAPHQL ----------------------------------------------------------------

source("graphql_functions/getAssessments.R")

# LOAD UTILITY ----------------------------------------------------------------

# Load functions
source("graphql_functions/getToken.R")
source("graphql_functions/getAssessments.R")

# Load data
token = getToken(Username = "shiny", Password = "Password@1", url = url)
assessments = getAssessments(token = token, url = url)


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
  mutate(questCount = cumsum(questInAssessment))


df %>%
  filter(createdAt > "2022-07-19" & createdAt < "2022-07-28") %>%
  group_by(clinician, status) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = status, values_from = count, values_fill = 0) %>%
  arrange(desc(COMPLETED))

