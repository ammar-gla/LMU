#_______________________________________________________________________________
###  INITIALISIATION AND PATHS ----
#_______________________________________________________________________________

# Put in your Nomis API key (NB: THE BELOW IS AMMAR'S KEY!!)
Sys.setenv(NOMIS_API_KEY = "0x01a88c6659d20042f087de2e585cdf3a07708983")

# Set paths
INPUT <- paste0(here::here(),"/INPUT/")
INTERMEDIATE <- paste0(here::here(),"/INTERMEDIATE/")
IMAGES <- paste0(here::here(),"/IMAGES/MAIN/")
FORMATTING <- paste0(here::here(),"/FORMATTING/")
SCRIPTS <- paste0(here::here(),"/SCRIPTS/")
SUBSCRIPTS <- paste0(SCRIPTS,"/SUBSCRIPTS/")