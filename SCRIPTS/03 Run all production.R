#_______________________________________________________________________________
###  RUN ALL SCRIPTS ----
#_______________________________________________________________________________
  
  # Put in your Nomis API key (NB: THE BELOW IS AMMAR'S KEY!!)
  Sys.setenv(NOMIS_API_KEY = "0x01a88c6659d20042f087de2e585cdf3a07708983")

  # HERE package needed for dynamic pathfinding
  library("here") 
  
  # Set paths
  INPUT <- paste0(here::here(),"/INPUT/")
  INTERMEDIATE <- paste0(here::here(),"/INTERMEDIATE/")
  IMAGES <- paste0(here::here(),"/OUTPUT/IMAGES/MAIN/")
  HTML_OUT <- paste0(here::here(),"/OUTPUT/HTML/")
  FORMATTING <- paste0(here::here(),"/FORMATTING/")
  SCRIPTS <- paste0(here::here(),"/SCRIPTS/")
  SUBSCRIPTS <- paste0(SCRIPTS,"/SUBSCRIPTS/")

#...............................................................................
#### Run LMU scripts ----
#...............................................................................

  # Data packages
  source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))

  # Inputs such as borough codes in Nomis
  source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))
  
  # Run the subscripts necessary for markdown
  source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))
  source(paste0(SUBSCRIPTS,"GLAE_paye_dataload",".r"))
  source(paste0(SUBSCRIPTS,"GLAE_cpih_dataload",".r")) # Needs to run after PAYE
  source(paste0(SUBSCRIPTS,"GLAE_wfj_dataload",".r"))
  
  # Misc datasets - LFS, CC and geographical
  source(here::here("SCRIPTS",paste0("01a LMU dataload.r")))
  
  # Produce LMU markdown
  rmarkdown::render(paste0(SCRIPTS,"01b LMU markdown content.Rmd"),
                    output_file = paste0(HTML_OUT,"LMU ", format(Sys.Date(),"%B %Y"), 
                                        ".html"))
  
#...............................................................................
#### Run CCLB scripts  ----
#...............................................................................
  
  
  # Run CCLB
  rm(list = ls()) #Empty environment
  
  # HERE package needed for dynamic pathfinding
  library("here") 
  
  # Set paths
  INPUT <- paste0(here::here(),"/INPUT/")
  INTERMEDIATE <- paste0(here::here(),"/INTERMEDIATE/")
  IMAGES <- paste0(here::here(),"/OUTPUT/IMAGES/MAIN/")
  HTML_OUT <- paste0(here::here(),"/OUTPUT/HTML/")
  FORMATTING <- paste0(here::here(),"/FORMATTING/")
  SCRIPTS <- paste0(here::here(),"/SCRIPTS/")
  SUBSCRIPTS <- paste0(SCRIPTS,"/SUBSCRIPTS/")
  
  # Data packages
  source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))
  
  # Inputs such as paths and borough codes in Nomis
  source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))
  
  # Run the subscripts necessary for markdown
  source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))
  
  # Load CC data and geo data
  source(paste0(SUBSCRIPTS,"GLAE_CCLB_dataload",".r"))

  rmarkdown::render(here::here("SCRIPTS",paste0("02 CCLB markdown.Rmd")),
                    output_file = paste0(HTML_OUT,"CCLB ", format(Sys.Date(),"%B %Y"), 
                                         ".html"))
  