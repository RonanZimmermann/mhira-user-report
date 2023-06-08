getAssessments = function(PAGING, FLT, SORT, token, url){
  print("getAssessments")

# Packages 


# Load functions ----
source("graphql_functions/GQL2.R")
source("graphql_functions/generateGetAssessmentsQuery.R")

# GraphQL request to get assessments ----
assessments_query = generateGetAssessmentsQuery()
res_assessments = GQL2(assessments_query, .token = token, .url = url)
res_assessments <- fromJSON(res_assessments)
nextPage = res_assessments$data$assessments$pageInfo$hasNextPage 
endCursor = res_assessments$data$assessments$pageInfo$endCursor
assessments = res_assessments$data$assessments$edges$node %>% unnest(questionnaireAssessment)

while(nextPage){
  assessments_query = generateGetAssessmentsQuery(PAGING = paste('first: 50, after: "', endCursor, '"', sep = ""))
  res_assessments = GQL2(assessments_query, .token = token, .url = url)
  res_assessments <- fromJSON(res_assessments)
  nextPage = res_assessments$data$assessments$pageInfo$hasNextPage 
  endCursor = res_assessments$data$assessments$pageInfo$endCursor
  assessments = assessments %>% bind_rows(res_assessments$data$assessments$edges$node %>% unnest(questionnaireAssessment))
  
}


# assessments = assessments[assessments$status == "COMPLETED",] # currently not working because field is exported from postgres
print("Assessments received")
return(assessments)

}
