  
  #_____________________________________________________________________________
  ###  Load packages and set paths ----
  #_____________________________________________________________________________
  
  ## Put in your Nomis API key (NB: THE BELOW IS AMMAR'S KEY!!)
  Sys.setenv(NOMIS_API_KEY = "0x01a88c6659d20042f087de2e585cdf3a07708983")
  
  library("here") # To set project folder dynamically
  library("remotes") # Makes it possible to load github package where necesary
  library("knitr") # Needed to knit markdown
  library("tidyverse") # Whole tidyverse package
  library("lubridate") # Tideyverse date manipulations
  library("scales") # For scales in ggplots
  library("ggplot2") # Used for making main charts
  library("ggthemes") # Additional themes for ggplot [not sure if used]
  library("nomisr") # Downloading data from Nomis
  library("devtools") # Allows downloading ggla packages
  library("gglaplot") # GLA plotting functions
  library("data.table") # Data table utility
  library("janitor") # Cleaning up date files
  library("ggrepel") # Used to repel overlapping text in charts [used in gglaplot?]
  library("plotly") # Interactive charts
  library("leaflet") # Interactive maps
  library("rgdal") # Needed for geospatial manipulations
  library("httr") # Quering websites to check if data exists
  library("flextable") # Summary headline stats table
  library("officer") # for fp_text
  library("extrafont") #Fonts for the flextable
  library("svDialogs") #for pop-ups
  
  # The below are needed to install GLA packages the first time it is runs
  #devtools::install_github("Greater-London-Authority/gglaplot")
  #devtools::install_github("Greater-London-Authority/ldndatar", auth_token = "96e66bb601f49f62f0bb9bdcb73a849ece358ad1")
  #remotes::install_github("wilkelab/ggtext")
  
  ### Paths
  
  INPUT <- paste0(here::here(),"/INPUT/")
  INTERMEDIATE <- paste0(here::here(),"/INTERMEDIATE/")
  IMAGES <- paste0(here::here(),"/IMAGES/MAIN/")
  FORMATTING <- paste0(here::here(),"/FORMATTING/")
  
  
  ### Geographic codes for UK, London and Boroughs.
  ### See "Borough codes.csv" in source folder for reference. 
  
  boroughs_group <- c(2013265927,2092957697,1811939540,1811939541,1811939542,1811939543,1811939544,1811939526,1811939527,1811939545,1811939546,1811939547,
             1811939548,1811939528,1811939529,1811939530,1811939549,1811939550,1811939551,1811939552,1811939531,1811939532,1811939553,1811939533,
             1811939534,1811939554,1811939535,1811939555,1811939556,1811939536,1811939557,1811939537,1811939558,1811939538,1811939539)
  
  #_____________________________________________________________________________
  ### Presets for presenting figures etc. ----
  #_____________________________________________________________________________
  
  signif_thous = 3 #How many significant figures to use when figures are in thousands
  signif_mil = 2 # For millions
  signif_perc = 2 #For percentages
  signif_pp = 3 #For percentage points
  
  digits_mil = 1
  digits_perc = 1
  
  #_____________________________________________________________________________
  ### Functions ----
  #_____________________________________________________________________________
  
  ### Define function to always present percentages with one decimal and figures without decimals
  perc_form = function(x, d=1) sprintf(paste0("%1.",d,"f"), x) 
  
  value_form = function(x,s=2,d= -1) format(signif(round(as.numeric(x), d),s), big.mark=",")
  
  
  ### Function to make conditional text based on values
  condi_text <- function(x,t=c("increase","noun","rise","growth","up","above")) { #Define names by the positive change
    if(t=="increase") {y = case_when(x<0 ~ "decreased",
                                     x>0 ~ "increased")
    return(y)}
    else if(t=="noun") {y = case_when(x<0 ~ "a decrease",
                                      x>0 ~ "an increase")
    return(y)}
    else if(t=="rise") {y = case_when(x<0 ~ "fell",
                                      x>0 ~ "rose")
    return(y)}
    else if(t=="growth") {y = case_when(x<0 ~ "shrank",
                                        x>0 ~ "grew")
    return(y)}
    else if(t=="up") {y = case_when(x<0 ~ "down",
                                    x>0 ~ "up")
    return(y)}
    else if(t=="above") {y = case_when(x<0 ~ "below",
                                       x>0 ~ "above")
    return(y)}
    else {return("[NB: wrong value for t]")}
  }
  
  ### Function to remove negative sign from character strings (needed as formatted numbers are not numerical)
  abs2 <- function(x) {
    y = gsub("-","",x)
    return(y)
  }
  
  ### Helper function to ensure legend labels are placed correctly
  reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
  }
  
  ### Helper function to re-introduce simplified modebar allowing chart download
  plotly_modebar <- function(plotly_plot) {
    plotly_plot <- plotly_plot %>% plotly::config(displayModeBar = TRUE) %>% # Allows menu bar such that image can be downloaded
      plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d","pan2d","autoScale2d","hoverClosestCartesian","hoverCompareCartesian","select2d","lasso2d")) %>% 
      plotly::config(displaylogo = FALSE)
    plotly_plot
  }

  ### To help pull data
  pull_dtpt <- function(dt=NULL,flt=NULL,vr=NULL) {
    dtpt <- dt %>% 
      filter(eval(rlang::parse_expr(flt))) %>% 
      pull(var=vr)
    
    return(dtpt)
  }
  # THe function above can be used as follows:
  # pull_dtpt(dt=paye_nat_long,flt="nationality == 'EU' & region== 'London' & date_day == max(date_day)",vr="employee_count")
  
  ### Function to save last chart displayed in PNG and SVG
  save_GLA_plot <- function(plot_name, w=8, h=8) {
    ggsave(paste0(IMAGES,Sys.Date(),"_",plot_name,".svg"), device = "svg", width = w, height = h, units = "in")
    ggsave(paste0(IMAGES,Sys.Date(),"_",plot_name,".png"), device = "png", width = w, height = h, units = "in")
  }
  
  ### Standard ggplotly settings
  # Note: the title and subtitle text need to be enclosed within a paste function within single quotes, such that functions can be evaluated
  
  LMU_plotly_settings <- function(ch_name, title_text=NULL,subtitle_text=NULL,hover_mode=c("x","closest","none","x unified","y unified"), ...) {
    
    hover_mode <- match.arg(hover_mode) # needs to be one or the other
    if (hover_mode=="none") hover_mode=FALSE
    
    if (hover_mode %in% c("x unified","y unified")) {
      
      gla_colours <- get(paste0("gla_",gla_theme_type)) # Colours are somehow overwritten for hover label, so reintroduce below
      
      ggplotly(ch_name,tooltip = "text") %>% 
        ggla_plotly_settings(...)    %>% 
        layout(title = list(text = (paste0("<b>",
                                           eval(parse(text=title_text)),
                                           "</b>","<br>",
                                           "<sup>",
                                           eval(parse(text=subtitle_text)),
                                           "</sup>","<br>")),
                            font = list(size = 22),
                            y = .95, xref = "plot"),
               xaxis = list(tickfont = list(size = 15)),
               yaxis = list(tickfont = list(size = 15)),
               legend = list(font = list(size = 15),title=list(text="")),
               hovermode = hover_mode,
               hoverlabel = list(bgcolor = gla_colours$headlines,
                                 bordercolor = gla_colours$headlines,
                                 font = list(family = "Arial",
                                             color = gla_colours$background))) %>% 
        plotly_modebar
    }
    else {
      ggplotly(ch_name,tooltip = "text") %>% 
        ggla_plotly_settings(...)    %>% 
        layout(title = list(text = (paste0("<b>",
                                           eval(parse(text=title_text)),
                                           "</b>","<br>",
                                           "<sup>",
                                           eval(parse(text=subtitle_text)),
                                           "</sup>","<br>")),
                            font = list(size = 22),
                            y = .95, xref = "plot"),
               xaxis = list(tickfont = list(size = 15)),
               yaxis = list(tickfont = list(size = 15)),
               legend = list(font = list(size = 15),title=list(text="")),
               hovermode = hover_mode) %>% 
        plotly_modebar
    }
    
  }
  
  # Round to nearest anything
  rounder <- function(x,y) {
    if(y >= 0) { x + (y - x %% y)}
    else { x - (x %% abs(y))}
  }
  
  #.............................................................................
  ### Define GLA line chart function ----
  #.............................................................................
  
  GLALineChart <- function( data_set = lfs_stats, ### This function has been written to work specifically with the NM_59_1 dataset
                            lfs_or_claims = "lfs",
                            x_var = date_day, ### Assumed that this will be a line chart with dates on the x-axis
                            min_x_var = as.Date("2015-01-01"), 
                            y_var = NULL, ### See document "[today's date]colnames.csv" for variables available for charting
                            geography = c("London", "United Kingdom"),
                            suffix = "%",
                            y_limits = c(0,100),
                            nudge_y = NULL,
                            title = NULL,
                            subtitle = paste0("Latest data for period ", new_release),
                            caption = "",
                            chart_name = NULL) {
    
    pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
    theme_set(theme_gla(gla_theme = "default"))
    
    ### Create dataset for charting
    
    if( lfs_or_claims == "lfs") {
      
      for_charting <- data_set %>% 
        filter( geography_name %in% geography & value_type_name == "Level" & sex_name == "Total" & measures == 20207 & {{x_var}}>=min_x_var)
    } else {
      for_charting <- data_set %>% 
        filter( geography_name %in% geography & sex_name == "Total" & !is.na({{y_var}}))
    }
    ### Set x-axis format
    
    ### Plot charts
    
    new_release <- for_charting$date_name[for_charting$date == max(for_charting$date)]
    
    glaplot <- for_charting %>%
      ggplot(mapping = aes(x = {{x_var}}, y = {{y_var}}, 
                           colour = geography_name, group = geography_name,
                           text = paste(
                             geography_name, "\n",
                             format({{x_var}},'%B %Y'), "\n",
                             "Rate: ", perc_form({{y_var}}),"%", "\n",
                             sep = ""))) +
      ggla_line(aes(size= geography_name)) +
      scale_size_manual(values = c(2 * mm_to_pt, 1 * mm_to_pt)) +
      scale_colour_manual(values = pal) +
      ggla_highlight(filter_type = "end") +
      ggla_highlight(mapping = aes(label = paste0({{y_var}}, "%")),
                     geom = GeomGLATextHighlight, filter_type = "end",  size = 4.5,
                     position = position_nudge(y = nudge_y),check_overlap = TRUE)+
      geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
                 linetype = "dotted",
                 size = 1 * mm_to_pt,
                 colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
      coord_cartesian(clip = 'off') +
      scale_y_continuous(expand = c(0, 0), labels = dollar_format(prefix = "", 
                                                                  suffix = suffix, 
                                                                  largest_with_cents = 1), 
                         limits = y_limits) +
      scale_x_date( date_breaks = "1 year",
                    date_labels = "%b %Y",
                    expand = expansion( mult = c(0.05,0.05))) +
      theme(plot.margin = unit(c(1,1,1,1), "cm"))+
      labs(title = title,
           subtitle = subtitle,
           caption = caption) +
      theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
    
    
    save_GLA_plot(plot_name = chart_name)
    
    return(glaplot)
    
  }

  #_____________________________________________________________________________
  ### Downloads and data processing ----
  #_____________________________________________________________________________
  
  #.............................................................................
  ### London borough geographical codes ----
  #.............................................................................
  London_LA_codes <-readxl::read_excel(path = paste0(INPUT,"London borough codes.xlsx"), sheet = "Codes") %>% clean_names()
  
  #.............................................................................
  ### Fundction for dowloading headline stats ----
  #.............................................................................
  
  HeadlineDownload <- function( data_series = "NM_59_1", 
                                time_period = c("1996-01","latest"), 
                                save_intermediate = TRUE,
                                geography = boroughs_group) {
    
    ### Download both absolute and percentage figures from NOMIS
    
    raw_nomis <- nomis_get_data( id = data_series, 
                                 geography = geography,
                                 time = time_period)
    
    ### Store downloads as dataframes
    
    lfs_stats <- raw_nomis %>% 
      mutate(DATE_DAY = as.Date(paste0(DATE, "-01"))) %>% 
      filter(MEASURES %in% c(20207,20100) & !is.na(OBS_VALUE)) %>%
      select( "DATE_DAY", "DATE_NAME", "SEX_NAME", "GEOGRAPHY_NAME", "ECONOMIC_ACTIVITY_NAME", "VALUE_TYPE_NAME", "OBS_VALUE","MEASURES","MEASURES_NAME") %>% 
      pivot_wider( names_from = ECONOMIC_ACTIVITY_NAME, values_from = OBS_VALUE) %>%  
      clean_names()
    
    ### Save intermediate dataframes if specified
    
    if( save_intermediate == TRUE) {
      lfs_stats %>% 
        fwrite(file = paste0(INTERMEDIATE,Sys.Date(),"_LFS_update.csv"))
      
    }
    
    data.table::fwrite( as.data.frame(colnames(lfs_stats)), file = paste0(INTERMEDIATE,"/",Sys.Date(),"_colnames.csv"))
    
    return( lfs_stats)
    
  }
  
  #.............................................................................
  ### Download LFS stats, automatically clean and save ----
  #.............................................................................
  
  lfs_stats <- HeadlineDownload( time_period = c("2010-01","latest"))
  
  #.............................................................................
  ### Claimant count data download ----
  #.............................................................................
  
  # Define function used
  
  ClaimantCountDownload <- function( data_series = "NM_162_1",
                                     time_period = c("1996-01","latest"),
                                     save_intermediate = TRUE,
                                     geography_v = boroughs_group,
                                     measures_v = 20100,
                                     line_or_bar = "line") {
    
    ### Download the claimant count data from NOMIS
    
    raw_nomis <- nomis_get_data(
      id = data_series, 
      geography = geography_v,
      measures = measures_v,
      time = time_period) 
    
    ### Clean data
    
    claimant_count_stats <- raw_nomis %>% 
      mutate( DATE_DAY = as.Date(paste0(DATE, "-01"))) %>% 
      filter( !is.na(OBS_VALUE) & MEASURES == measures_v) %>% 
      pivot_wider( names_from = MEASURE_NAME, values_from = OBS_VALUE) %>%
      clean_names() %>% 
      select( date_day, date_name, geography_name, gender_name, age_name, last_col(4):last_col()) %>% 
      rename( sex_name = gender_name) %>% 
      pivot_longer(cols=c(contains("claimant")),
                   names_to = "measure_name",
                   values_to = "measure_value",
                   values_drop_na=TRUE) # Re-reshape to get rid of NA and format as panel
    
    
    if( line_or_bar == "line")
    {
      claimant_count_stats <- claimant_count_stats %>%
        filter( age_name == "All categories: Age 16+")
    }
    
    
    ### Save intermediate dataframes if specified
    
    if( save_intermediate == TRUE) {
      fwrite(claimant_count_stats, file = paste0(INTERMEDIATE,Sys.Date(),"_",data_series,"_claimant.csv"))
    }
    
    return( claimant_count_stats) 
    
  }
  
  
  # ----- Download data and make wide and long version 
  
  claimant_count_stats_long <- ClaimantCountDownload( data_series = "NM_162_1", 
                                                      time_period = c("2015-01","latest"),
                                                      geography_v = c(2013265927,2092957697), 
                                                      save_intermediate = TRUE, 
                                                      line_or_bar = "bar")  %>% 
    group_by(geography_name,age_name,sex_name,measure_name) %>% 
    mutate(change_month = measure_value - lag(measure_value , n = 1),
           change_month_percent = 100 * (measure_value - lag(measure_value, n = 1))/lag(measure_value, n = 1),
           change_year = measure_value - lag(measure_value, n = 12),
           change_year_percent = 100*(measure_value - lag(measure_value, n = 12))/lag(measure_value, n = 12)) %>% 
    ungroup
  
  claimant_count_stats_wide <- claimant_count_stats_long %>% # Wide used for some charts
    select(-c(contains("change"))) %>% 
    pivot_wider(names_from = "measure_name", values_from = "measure_value")
  
  # raw_claim <- download.file( url = "https://www.nomisweb.co.uk/api/v01/dataset/NM_162_1.data.csv?geography=1807745125...1807745129,1807745111,1807745112,1807745130...1807745133,1807745113...1807745115,1807745134...1807745137,1807745116,1807745117,1807745138,1807745118,1807745119,1807745139,1807745120,1807745140,1807745141,1807745121,1807745142,1807745122,1807745143,1807745123,1807745124,2092957697,2013265927&date=latestMINUS22-latest&gender=0...2&age=0...2,10,11,3,12...16,4,17...20&measure=1...4&measures=20100&signature=NPK-ffca5e5a461529986a4082:0x6a7726c4d1c34de0ce2aca1289257fd9ef1cc613"), mode = "wb")
  
  # claimant_count_stats_o <-read.csv(paste0(INTERMEDIATE,"2021-11-16_NM_162_1_claimant.csv")) %>% 
  #     arrange(date, geography_name,sex_name,age_name)
  
  # claimant_count_stats <-read.csv(paste0(INPUT,"2021-12-10_claims_NM_162_1.csv")) %>% 
  #   mutate( DATE = as.Date(paste0(DATE, "-01"))) %>% 
  #   filter( !is.na(OBS_VALUE) & MEASURES == 20100) %>% 
  #   pivot_wider( names_from = MEASURE_NAME, values_from = OBS_VALUE) %>%
  #   clean_names() %>% 
  #   select( date, date_name, geography_name, gender_name, age_name, last_col(4):last_col()) %>% 
  #   rename( sex_name = gender_name) %>% 
  #   arrange(date, geography_name,sex_name,age_name)
  
  #.............................................................................
  ### Download PAYE data, clean and save ----
  #.............................................................................
  
  # Check if there is a file this month, otherwise take previous month
  
  paye_online_path_now <- paste0("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted/current/",
                                 paste0("rtisa",tolower(format(Sys.Date(),'%b%Y'))),
                                 ".xlsx")
  
  paye_online_path_prev <-  paste0("https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted/current/",
                                   paste0("rtisa",tolower(format(floor_date(Sys.Date(),"month")-months(1),'%b%Y'))),
                                   ".xlsx")
  
  ## Function to assert URL exists
  urlFileExist <- function(url){
    HTTP_STATUS_OK <- 200
    hd <- httr::HEAD(url)
    status <- hd$all_headers[[1]]$status
    list(exists = status == HTTP_STATUS_OK)
  }
  
  # Actual control
  if (urlFileExist(paye_online_path_now)==TRUE) {
    paye_online_path <- paye_online_path_now
  }  else {
    paye_online_path <- paye_online_path_prev
  }
  
  #Download latest file
  raw_paye <- download.file( url = paye_online_path,
                             destfile = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"),
                             mode = "wb")
  
  paye_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "7. Employees (NUTS1)", skip = 6) %>% 
    select( Date, London, UK) %>%
    clean_names() %>% 
    mutate( date_day = dmy(paste0("01",date)),
            lon_d_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                           date_day > as.Date("2020-02-01") ~ london - london[ date_day == "2020-02-01"]),
            lon_p_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                           date_day > as.Date("2020-02-01") ~ 100*(london - london[ date_day == "2020-02-01"])/london[ date_day == "2020-02-01"]),
            lon_d_change_mom = case_when(date_day - min(date_day) < 365 ~ NaN,
                                         date_day - min(date_day) > 365 ~ 
                                           (london - lag(london, n = 1))),
            lon_p_change_mom = case_when(date_day - min(date_day) < 365 ~ NaN,
                                         date_day - min(date_day) > 365 ~ 
                                           100*(london - lag(london, n = 1))/lag(london, n = 1)),
            uk_d_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                          date_day > as.Date("2020-02-01") ~ uk - uk[ date_day == "2020-02-01"]),
            uk_p_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                          date_day > as.Date("2020-02-01") ~ 100*(uk - uk[ date_day == "2020-02-01"])/uk[ date_day == "2020-02-01"]),
            lon_d_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                          date_day - min(date_day) > 365 ~ 
                                            (london - lag(london, n = 12))),
            lon_p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                                     date_day - min(date_day) > 365 ~ 
                                                       100*(london - lag(london, n = 12))/lag(london, n = 12)),
            uk_p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                                    date_day - min(date_day) > 365 ~ 
                                                      100*(uk - lag(uk, n = 12))/lag(uk, n = 12)))
  
  paye_nuts2_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "11. Employees (NUTS2)", skip = 6) %>% 
    mutate( date_day = dmy(paste0("01",Date)))  %>% 
    merge(paye_stats,by="date_day")   %>% 
    rename(London=london) %>% 
    select( Date, date_day, "Inner London - West",	"Inner London - East",	"Outer London - East and North East",	"Outer London - South",	"Outer London - West and North West", "London","UK") %>%
    pivot_longer(cols="Inner London - West":UK,names_to = "area", values_to = "emp_level") %>% 
    group_by(area) %>% 
    mutate( 
      p_change_feb20 = case_when( date_day <= as.Date("2020-02-01") ~ NaN,
                                           date_day > as.Date("2020-02-01") ~ 100*(emp_level - emp_level[ date_day == "2020-02-01"])/(emp_level[ date_day == "2020-02-01"])),
      p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                date_day - min(date_day) > 365 ~ 100*(emp_level - lag(emp_level, n= 12))/lag(emp_level, n=12))) %>% 
    clean_names()
  
  #--- Retrieve stats on employee monthly pay
  
  paye_pay_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "8. Median pay (NUTS1)", skip = 6) %>% 
    mutate(date_day=my(Date)) %>% 
    select(date_day, London, UK) %>%
    pivot_longer(cols = c("UK","London"),names_to = "geography_name",values_to = "median_pay_nominal") 
  
  paye_overall_last_month <- format(max(paye_stats$date_day),"%B %Y")
  
  ## -------------------------------------------------------------------------##
  # PAYE data has unique quarterly data on following months:
  ## - (a) By Age*NUTS1: 1,4,7,10
  ## - (b) By Industry*NUTS1: 2,5,8,11
  ## - (c) By LA: 3,6,9,12
  
  # These need to be updated as such:
  ## If division remainder is 0, then (c); if 1 then (a), if 2 then (b)
  ## If it is LMU release day, ensure data is updated
  
  current_month <- lubridate::month(Sys.Date())
  #month_remainder <- current_month %% 3
  release_dates <- c("2022-01-18","2022-02-15","2022-03-15","2022-04-12","2022-05-17","2022-06-14","2022-07-19","2022-08-16","2022-09-13","2022-10-11")
  release_dates_form <- as.Date(release_dates) # returns R dates for the dates above

  # Loop through dates from oldest to newest and replace datasets when appropriate
  for (i in 1:length(release_dates_form)) {
    if (release_dates_form[i] <= Sys.Date()) { #If we passed the day, assign dataset name
      
      month_remainder <- lubridate::month(release_dates_form[i]) %% 3 # Check which dataset is updated
      
      if (month_remainder == 0) {
        latest_paye_locauth_name <- paste0(release_dates_form[i],"_raw_paye.xlsx")
      } else if (month_remainder==1) {
        latest_paye_nuts1age_name <- paste0(release_dates_form[i],"_raw_paye.xlsx")
      } else if (month_remainder==2) {
        latest_paye_nuts1ind_name <- paste0(release_dates_form[i],"_raw_paye.xlsx")
      }
    }
  }
  
  # 
  # if (Sys.Date() %in% lapply(release_dates,as.Date)) {
  #   
  #   if (month_remainder == 0) {
  #     latest_paye_locauth_name <- "2022-03-15_raw_paye.xlsx"
  #   } else if (month_remainder==1) {
  #     dlg_message("Remember to update the following: PAYE Age*NUTS1 data")$res
  #   } else if (month_remainder==2) {
  #     dlg_message("Remember to update the following: PAYE Industry*NUTS1 data")$res
  #   }
  #   
  # }
  #### (a) Each quarter, process data on PAYE employee age on regional level
  #### NB: MUST BE UPDATED MANUALLY TO NEWER DATA  
  
  
  #latest_paye_nuts1age_name <- "2022-01-18_raw_paye.xlsx"
  
  paye_age_stats_uk <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1age_name), sheet = "28. Employees (Age)", skip = 5) %>%
    mutate(date_day = dmy(paste0("01",Date))) %>% 
    select( Date, date_day, "0-17","18-24","25-34","35-49","50-64","65+") %>% 
    pivot_longer(cols="0-17":"65+",names_to = "age_group", values_to = "emp_level") %>% 
    group_by(age_group) %>% 
    mutate( 
      area = "United Kingdom",
      p_change_feb20 = case_when( date_day <= as.Date("2020-02-01") ~ NaN,
                                           date_day > as.Date("2020-02-01") ~ 100*(emp_level - emp_level[ date_day == "2020-02-01"])/(emp_level[ date_day == "2020-02-01"])),
      p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                date_day - min(date_day) >= 365 ~ 100*(emp_level - lag(emp_level, n= 12))/lag(emp_level, n=12)),
      age_group = paste0("Aged: ", age_group)) %>% 
    clean_names() 
  
  
  paye_nuts1age_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1age_name), sheet = "32. Employees (NUTS1Age)", skip = 6) %>%
    mutate(date_day = dmy(paste0("01",Date))) %>% 
    select( Date, date_day, "London: 0-17","London: 18-24","London: 25-34","London: 35-49","London: 50-64","London: 65+") %>% 
    pivot_longer(cols="London: 0-17":"London: 65+",names_to = "age_group", values_to = "emp_level") %>% 
    group_by(age_group) %>% 
    mutate( 
      age_group = gsub("London: ","",x=age_group),
      area = "London",
      p_change_feb20 = case_when( date_day <= as.Date("2020-02-01") ~ NaN,
                                           date_day > as.Date("2020-02-01") ~ 100*(emp_level - emp_level[ date_day == "2020-02-01"])/(emp_level[ date_day == "2020-02-01"])),
      p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                date_day - min(date_day) >= 365 ~ 100*(emp_level - lag(emp_level, n= 12))/lag(emp_level, n=12)),
      age_group =paste0("Aged: ", age_group)) %>%
    clean_names()   %>%  rbind(paye_age_stats_uk) %>% 
    arrange(area,date_day)
  
  
  paye_age_last_month <- format(max(paye_nuts1age_stats$date_day),"%B %Y")
  
  
  
  #### (b) Each quarter, process data on PAYE employees by industry at region level. 
  #### NB: MUST BE UPDATED MANUALLY TO NEWER DATA
  
  #latest_paye_nuts1ind_name <- "2022-02-15_raw_paye.xlsx"
  
  paye_ind_stats_uk <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1ind_name), sheet = "23. Employees (Industry)", skip = 6) %>% 
    mutate(date_day = dmy(paste0("01",Date)),
           geography_name="United Kingdom") %>% 
    select(-c("UK"))  %>% 
    pivot_longer(cols="Agriculture, forestry and fishing":"Households and Extraterritorial",
                 names_to = "industry_name", values_to = "emp_level") %>% 
    clean_names() %>%
    rename(date_month=date)  
  
  
  paye_nuts1ind_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1ind_name), sheet = "36. Employees (NUTS1Sector)", skip = 6) %>% 
    mutate(date_day = dmy(paste0("01",Date)),
           geography_name="London") %>% 
    select(c(Date,date_day,geography_name,contains("London")))  %>%
    rename_with(~str_replace(.x,pattern="London: ",replacement=""))%>% 
    pivot_longer(cols="Agriculture, forestry and fishing":"Households and Extraterritorial",
                 names_to = "industry_name", values_to = "emp_level") %>% 
    clean_names() %>% 
    rename(date_month=date) %>% 
    rbind(paye_ind_stats_uk) %>% 
    group_by(geography_name,industry_name) %>% 
    mutate(
      p_change_feb20 = case_when( date_day <= as.Date("2020-02-01") ~ NaN,
                                           date_day > as.Date("2020-02-01") ~ 100*(emp_level - emp_level[date_day == "2020-02-01"])/(emp_level[ date_day == "2020-02-01"])),
      p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                date_day - min(date_day) >= 365 ~ 100*(emp_level - lag(emp_level, n= 12))/lag(emp_level, n=12)),
      industry_name_simple = case_when(
        industry_name == "Public administration and defence; social security" ~ "Public admin & defence",
        industry_name == "Health and social work" ~ "Health",
        industry_name == "Finance and insurance" ~ "Finance & insurance",
        industry_name == "Professional, scientific and technical" ~ "Professional services",
        industry_name == "Wholesale and retail; repair of motor vehicles" ~ "Retail",
        industry_name == "Other service activities" ~ "Other services",
        industry_name == "Transportation and storage" ~ "Transport & storage",
        industry_name == "Arts, entertainment and recreation" ~ "Arts & recreation",
        industry_name == "Information and communication" ~ "Information & communication",
        industry_name == "Administrative and support services" ~ "Administration",
        industry_name == "Accommodation and food service activities" ~ "Hospitality",
        industry_name == "Water supply, sewerage and waste" ~ "Water",
        TRUE ~ industry_name)) %>% 
    arrange(date_day,geography_name,industry_name_simple) %>% 
    relocate(date_month,date_day,geography_name,industry_name_simple) %>% 
    ungroup()
  
  paye_ind_last_month <- format(max(paye_nuts1ind_stats$date_day),"%B %Y")
  
  #### (c) Each quarter, process data on PAYE at borough level and merge with geographical codes. 
  #### NB: MUST BE UPDATED MANUALLY TO NEWER DATA
  
  #latest_paye_locauth_name <- "2022-03-15_raw_paye.xlsx"
  
  la_columns <- c("City of London", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey", "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea", "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", "Wandsworth", "Westminster")
  
  paye_la_emp_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_locauth_name), sheet = "19. Employees (LA)", skip = 7) %>% 
    mutate(date_day = dmy(paste0("01",Date))) %>% 
    select( Date, date_day, all_of(la_columns)) %>% 
    pivot_longer(cols="City of London":"Westminster",names_to = "area", values_to = "obs_value") %>% 
    mutate(obs_type="emp")
  
  paye_la_pay_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_locauth_name), sheet = "20. Median pay (LA)", skip = 7) %>% 
    mutate(date_day = dmy(paste0("01",Date))) %>% 
    select( Date, date_day, all_of(la_columns)) %>% 
    pivot_longer(cols="City of London":"Westminster",names_to = "area", values_to = "obs_value") %>% 
    mutate(obs_type="pay")
  
  paye_la_stats <-  paye_la_emp_stats %>% 
    rbind(paye_la_pay_stats) %>% 
    group_by(area,obs_type) %>% 
    mutate( 
      p_change_feb20 = case_when( date_day <= as.Date("2020-02-01") ~ NaN,
                                           date_day > as.Date("2020-02-01") ~ 100*(obs_value - obs_value[ date_day == "2020-02-01"])/(obs_value[ date_day == "2020-02-01"])),
      p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                date_day - min(date_day) >= 365 ~ 100*(obs_value - lag(obs_value, n= 12))/lag(obs_value, n=12))) %>% 
    clean_names() %>%  
    rename(date_month=date) %>% 
    merge(London_LA_codes,by='area') %>% 
    arrange(area,obs_type,date_day)
  
  paye_la_last_month <- format(max(paye_la_stats$date_day),"%B %Y")
  
  #.............................................................................
  ### Download Workforce Jobs data ----
  #.............................................................................
  
  
  wfj_stats <- nomis_get_data(id = "NM_130_1", time = c("2010-01", "latest"), geography = boroughs_group,
                              industry=c(37748736,150994945:150994964)) %>% 
    clean_names()   %>% 
    select(c("date","date_name","geography","geography_name","industry","industry_code","industry_name","item","item_name","measures","measures_name","obs_value","record_count")) %>% 
    rename(date_month=date) %>% 
    mutate(date_day = dmy(paste0("01",date_name)),
           quarter =  paste0("Q",quarter(date_day)),
           quarter_text = case_when(quarter=="Q1" ~ "first",
                                    quarter=="Q2" ~ "second",
                                    quarter=="Q3" ~ "third",
                                    quarter=="Q4" ~ "fourth")) %>% 
    group_by(industry_name, geography_name, item_name,measures_name) %>% #grouping makes calculating lag values easier
    mutate( 
      change_perc_since_dec19 = case_when( date_day <= as.Date("2019-12-01") | measures_name == "Percent" ~ NaN,
                                           date_day > as.Date("2019-12-01") ~ 100*(obs_value - obs_value[ date_day == "2019-12-01"])/(obs_value[ date_day == "2019-12-01"])),
      change_since_dec19 = case_when( date_day <= as.Date("2019-12-01") | measures_name == "Percent" ~ NaN,
                                      date_day > as.Date("2019-12-01") ~ (obs_value - obs_value[ date_day == "2019-12-01"])),
      change_perc_quar = case_when(  measures_name == "Percent" ~ NaN,
                                     TRUE ~ 100*(obs_value - lag(obs_value, n = 1))/(lag(obs_value, n = 1))),
      change_quar = case_when(  measures_name == "Percent" ~ NaN,
                                TRUE ~ (obs_value - lag(obs_value, n = 1))),
      industry_name = case_when(industry_name == "Total" ~ "Total",
                                TRUE ~  str_sub(industry_name,5,-1))) %>% #trims the name 
    mutate( industry_name_simple = case_when(
      industry_name == "Public administration and defence; compulsory social security" ~ "Public admin & defence",
      industry_name == "Human health and social work activities" ~ "Health",
      industry_name == "Financial and insurance activities" ~ "Finance & insurance",
      industry_name == "Professional, scientific and technical activities" ~ "Professional services",
      industry_name == "Wholesale and retail trade; repair of motor vehicles and motorcycles" ~ "Retail",
      industry_name == "Other service activities" ~ "Other services",
      industry_name == "Transportation and storage" ~ "Transport & storage",
      industry_name == "Arts, entertainment and recreation" ~ "Arts & recreation",
      industry_name == "Information and communication" ~ "Information & communication",
      industry_name == "Administrative and support service activities" ~ "Administration",
      industry_name == "Accommodation and food service activities" ~ "Hospitality",
      industry_name == "Water supply; sewerage, waste management and remediation activities" ~ "Water",
      TRUE ~ industry_name)) %>% 
    group_by( geography_name,date_day,item_name) %>% # To get ranks of industries within date-geography groups
    mutate(perc_change_dec19_within_date_rank = case_when( industry_name == "Total" ~ NA_integer_, 
                                                           TRUE  ~ dense_rank(desc(change_perc_since_dec19))), 
           perc_change_quart_within_date_rank = case_when( industry_name == "Total" ~ NA_integer_, 
                                                           TRUE  ~ dense_rank(desc(change_perc_quar)))) %>% 
    ungroup()%>%  #ungroup to ease future manipulations
    arrange(date_day,geography,item,measures,industry)
  
  #wfj_stats <- clean_names(wfj_stats,case="snake") #Converting column names to lower
  
  fwrite(wfj_stats, file = paste0(INTERMEDIATE,Sys.Date(),"_wfj.csv"), na = "NaN")
  
  #.............................................................................
  ### Download inflation data ----
  #.............................................................................
  
  raw_cpih <- download.file( url = "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23",
                             destfile = paste0(INTERMEDIATE,Sys.Date(),"_raw_cpih.csv"),
                             mode = "wb")
  raw_cpih_rate <- download.file( url = "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l55o/mm23",
                             destfile = paste0(INTERMEDIATE,Sys.Date(),"_raw_cpih_rate.csv"),
                             mode = "wb")
  
  cpih_rate <- read.csv(paste0(INTERMEDIATE,Sys.Date(),"_raw_cpih_rate.csv"), header=FALSE, stringsAsFactors=FALSE) %>% 
    rename(month_date=V1,cpih_yoy=V2) %>% 
    filter(grepl("\\d{4} \\w{3}",month_date))

  cpih_stats <- read.csv(paste0(INTERMEDIATE,Sys.Date(),"_raw_cpih.csv"), header=FALSE, stringsAsFactors=FALSE) %>% 
    rename(month_date=V1,cpih_index_2015=V2) %>% 
    filter(grepl("\\d{4} \\w{3}",month_date)) %>% 
    merge(cpih_rate,by="month_date") %>%   # Merge with annual rate
    mutate(date_day=ym(month_date),
           year=year(date_day),
           month = month(date_day)) %>% 
    arrange(date_day) %>% 
    mutate(cpih_yoy=as.numeric(cpih_yoy)/100,
           cpih_index_2015=as.numeric(cpih_index_2015),
           cpih_mom = cpih_index_2015/lag(cpih_index_2015,n=1)-1,
           cpih_mom_help=cpih_mom+1) %>% 
    arrange(date_day) %>% 
    mutate(cpih_annualised_3m = (cpih_mom_help*lag(cpih_mom_help,n=2)*lag(cpih_mom_help,n=3))^4-1) %>% 
    group_by(year) %>% # Calculate annualised inflation using all preceding months
    mutate(cpih_yearsofar_help = cumprod(cpih_mom_help),
           cpih_annualised = cpih_yearsofar_help^(12/month)-1) %>% 
    ungroup()
  # %>% 
  #   filter(date_day!="2022-04-01")
  
  remove(raw_cpih,raw_cpih_rate,cpih_rate)
  
  # Latest month for which CPIH data is available (is released after LMU)
  cpih_last_month <- cpih_stats %>% filter(date_day==max(date_day)) %>% pull(date_day)

  # Create predicted index for rest of year
  cpih_full_year <- tibble( # Create dataset with future months
    date_day = seq(as.Date(cpih_last_month),
                   as.Date(paste0(year(cpih_last_month),"-12-01")),
                   by="1 month")) %>% 
    mutate(year=year(date_day),
           month = month(date_day),
           forecast_month=1,
           months_in_future=interval(cpih_last_month, date_day) %/% months(1),
           forecast_description = "Annualised forecast CPIH") %>% 
    bind_rows(cpih_stats) %>% # Merge with main CPIH
    mutate(forecast_month = case_when(is.na(forecast_month) ~ 0,
                                      TRUE ~ forecast_month),
           forecast_description = case_when(is.na(forecast_description)==1 ~ "Actual CPIH",
                                            TRUE ~ forecast_description)) %>% #Needs to be created to sort "forecasted" current month after actual and fill down
    arrange(date_day,forecast_month) %>% 
    fill(c(cpih_annualised,cpih_index_2015), .direction="down") %>% 
    mutate(cpih_annualised_mom = (cpih_annualised+1)^(1/12),
      cpih_index_2015 = case_when( forecast_month==1 ~ cpih_index_2015*(cpih_annualised_mom^(months_in_future)), 
                                        TRUE ~ cpih_index_2015),
      cpih_index_feb20 = 100*cpih_index_2015/cpih_index_2015[date_day=="2020-02-01"])
  
  # Produce dataset with inflation-adjustment on pay
  cpih_pay_stats <- paye_pay_stats %>% 
    left_join(cpih_stats,by="date_day") %>% 
    mutate(median_pay_real=median_pay_nominal/cpih_index_2015*100)%>% 
    pivot_longer(cols=c(contains("_pay")),names_prefix="median_pay_",names_to = "pay_type",values_to="median_pay") %>% 
    mutate(geography_pay_type=paste0(geography_name,", ",pay_type)) %>% 
    group_by(geography_pay_type) %>% 
    arrange(geography_pay_type,date_day) %>% 
    mutate(p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    TRUE ~ (median_pay/lag(median_pay, n= 12)-1)),
           p_change_feb20 = case_when(date_day <= "2020-02-01" ~ (median_pay[date_day=="2020-02-01"]/median_pay-1), #If before Feb 20, then calculate change until then
                                      TRUE ~ (median_pay/median_pay[date_day=="2020-02-01"]-1)),
           pay_index_feb20 = 100*(median_pay/median_pay[date_day=="2020-02-01"]),
           months_from_feb20 = interval("2020-02-01",date_day) %/% months(1),
           p_change_mom = (median_pay/lag(median_pay, n= 1)-1),
           p_annualised_3m = ((1+p_change_mom)*(1+lag(p_change_mom,n=1))*(1+lag(p_change_mom,n=2)))^4-1) #Annualised change based on three rolling months

