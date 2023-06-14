
generateGetAssessmentsQuery = function(PAGING, FLT, SORT){

GQL = 'query {
    assessments(paging: {PAGING}, filter: {FLT}, sorting: {SORT}) {
      pageInfo{
        hasNextPage
        hasPreviousPage
        startCursor
        endCursor
      }
    	edges {
        node {
          id
          patientId
          clinician{
            firstName
            lastName
          }
          submissionDate
          createdAt
          updatedAt
          deletedAt
          questionnaireAssessment{
            status
            questionnaires(populate: true){
              abbreviation
            }
            
          }
        }
      }
    }
  }'

if(missing(FLT)){FLT = ""}
GQL = sub("FLT",FLT, GQL)   

if(missing(SORT)){SORT = "field: id, direction: ASC"}
GQL = sub("SORT", SORT, GQL)

if(missing(PAGING)){PAGING = "first: 50"}
GQL = sub("PAGING", PAGING, GQL)

return(GQL)  
  
}



# Could add ----
#clinician {
  #       id
  #       username
  #       active
  #       firstName
  #       middleName
  #       lastName
  #       email
  #       phone
  #       workID
  #       address
  #       gender
  #       birthDate
  #       nationality
  #       createdAt
  #       updatedAt
  #     }
  #     patient {
  #       id
  #       active
  #       medicalRecordNo
  #       firstName
  #       middleName
  #       lastName
  #       phone
  #       email
  #       address
  #       gender
  #       birthDate
  #       birthCountryCode
  #       nationality
  #       createdAt
  #       updatedAt
  #     }
  #   }
  # }
  # pageInfo {
  #   startCursor
  #   endCursor
  #   hasNextPage
  #   hasPreviousPage
  # }
