#_______________________________________________________________________________
###  RUN ALL SCRIPTS ----
#_______________________________________________________________________________
  
  # Put in your Nomis API key (NB: THE BELOW IS AMMAR'S KEY!!)
  Sys.setenv(NOMIS_API_KEY = "0x01a88c6659d20042f087de2e585cdf3a07708983")

  # ACTION: set whether to re-download all datasets, even if already exists
  redownload_all <- FALSE

  # HERE package needed for dynamic pathfinding
  library("here") 
  

#...............................................................................
#### Run LMU scripts ----
#...............................................................................

  # Create paths as strings
  source(here("SCRIPTS","SUBSCRIPTS","GLAE_paths.r"))
  
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
  source(here::here("SCRIPTS",paste0("01a_LMU_dataload.r")))
  
  # Produce LMU markdown
  rmarkdown::render(paste0(SCRIPTS,"01b_LMU_markdown_content.Rmd"),
                    output_file = paste0(HTML_OUT,"LMU ", format(Sys.Date(),"%B %Y"), 
                                        ".html"))
  
#...............................................................................
#### Run CCLB scripts  ----
#...............................................................................
  
  
  # Run CCLB -TODO: remove need for clearing environment
  rm(list = ls()) #Empty environment
  
  # ACTION: set whether to re-download all datasets, even if already exists
  redownload_all <- FALSE
  
  # HERE package needed for dynamic pathfinding
  library("here") 
  
  # Create paths as strings
  source(here("SCRIPTS","SUBSCRIPTS","GLAE_paths.r"))
  
  # Data packages
  source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))
  
  # Inputs such as paths and borough codes in Nomis
  source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))
  
  # Run the subscripts necessary for markdown
  source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))
  
  # Load CC data and geo data
  source(paste0(SUBSCRIPTS,"GLAE_CCLB_dataload",".r"))

  rmarkdown::render(here::here("SCRIPTS",paste0("02_CCLB_markdown.Rmd")),
                    output_file = paste0(HTML_OUT,"CCLB ", format(Sys.Date(),"%B %Y"), 
                                         ".html"))
  