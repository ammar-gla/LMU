
  #################################################################
  ###  Run scripts
  #################################################################
  
  # Needed to orientate the script into folder structure
  library("here")
  
  # Run the data and markdown scripts. Today's date is used by default to ensure version control.
  source(here::here("SCRIPTS",paste0("01a LMU dataload (m) ",format(Sys.Date(),"%y-%m-%d"),".r")))
  rmarkdown::render(here::here("SCRIPTS",paste0("01b LMU markdown content ",format(Sys.Date(),"%y-%m-%d"),".Rmd")),
                    output_file = paste0("LMU ", format(Sys.Date(),"%B %Y"), 
                                        ".html"))
  
  # Run CCLB
  rm(list = ls()) #Empty environment
  rmarkdown::render(here::here("SCRIPTS",paste0("02 CCLB markdown (m) ",format(Sys.Date(),"%y-%m-%d"),".Rmd")),
                    output_file = paste0("CCLB ", format(Sys.Date(),"%B %Y"), 
                                         ".html"))
  