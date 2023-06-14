## Default settings file    ----------------------------------------------------

# Please make a copy of this file and name it settings.R
# If this is not done, the app will automatically create a settings.R file 
# with the defaults listed below on initiation. 
# The app cannot run without these setting variables.
# Please make sure that all settings of the default file are in the 'settings.R'
# file.

# Settings might not be refreshed until something in the app.R file changes.

## Settings    -----------------------------------------------------------------

# Url of graphql API of MIHRA (container-name and port)
url = "mhira-backend:3000/graphql" 

# App closes after x seconds of inactivity
timeoutSeconds = 10*60  

# in case no language is found from local storage(browser)
defaultLang = "en"

# Show this patient information. Remove those that should not be shown.
selectPatientInfo = c(
  'medicalRecordNo',
  'initials',
  #'firstName',
  #'middleName',
  #'lastName',
  #'birthDate',
  'age',
  'gender')

