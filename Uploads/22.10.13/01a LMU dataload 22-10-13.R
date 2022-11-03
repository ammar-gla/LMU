  
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
  library("sf") # for geometry
  
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
    if(y >= 0) {z = x + (y - x %% y)} #if rounding up
    else {z = x - (x %% abs(y))} #if rounding down
    
      z[dplyr::near(z,0)] <- 0 # due to floating point operators, ensure it is zero
    
    return(z)
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
                           text = paste0(
                             geography_name, "\n",
                             format({{x_var}},'%B %Y'), "\n",
                             "Rate: ", perc_form({{y_var}}),"%", "\n"))) +
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
  
  
  #.............................................................................
  #### Map production ----
  #.............................................................................
  
  # Helper function to create bins
  map_prep <- function(dataset,
                       dt_var=NULL,
                       pal_colour=NULL,
                       diverge_scale=FALSE, #whether negatives and positives needed
                       bin_rounding=NULL, #floors and ceilings rounded by this number
                       perc_txt="") {
    
    checkmate::assertNumber(bin_rounding)
    checkmate::assertCharacter(dt_var)
    
    
    # Create interval ranges for bins
    dataset_bins <- dataset %>%
      arrange(!!sym(dt_var)) %>% 
      mutate(value_floor = rounder(!!sym(dt_var),-bin_rounding),
             value_ceil = rounder(!!sym(dt_var),bin_rounding),
             value_interval = paste0(format(value_floor,big.mark=",",trim=TRUE),perc_txt,
                                     " to ",format(value_ceil,big.mark=",",trim=TRUE),perc_txt)) 
    
    # Helper data for bins
    bin_data <- dataset_bins %>% 
      pivot_longer(cols=c("value_ceil","value_floor"),values_to="bin_val")  %>% 
      arrange(bin_val) %>% 
      distinct(bin_val) %>% 
      mutate(bin_int = paste0(format(bin_val,big.mark=",",trim=TRUE),perc_txt,
                              " to ",format(lead(bin_val,n=1),big.mark=",",trim=TRUE),perc_txt),
             neg_int = bin_val<0) #used when diverging scales
    
    
    # Number of bin intervals and define palette - differs on single scale or diverging
    
    # Define bin cuts for interactive map (only palette needs to be defined as divergent)
    bin_cuts <- bin_data %>%  
      pull(bin_val)
    
    if (diverge_scale==TRUE) {
      
      checkmate::assert_character(pal_colour,len=2) #must be of length two
      
      # Check that not all values are either positive or negative
      checkmate::assert_true(all(c(!all(bin_cuts>=0),!all(bin_cuts<0)))) 
      
      # Define bin cuts for interactive map, both negative and positive numbers
      # bin_cuts_neg <- bin_data %>%  
      #   filter(neg_int==TRUE) %>% 
      #   pull(bin_val)
      # 
      # bin_cuts_pos <- bin_data %>%  
      #   filter(neg_int==FALSE) %>% 
      #   pull(bin_val)
      
      # Define categorical bin  intervals for ggplot, both negative and positive numbers
      bin_ints_neg <- bin_data %>%
        filter(!grepl("NA",bin_int) & neg_int==TRUE) %>% # remove any with NA in interval
        arrange(bin_val) %>% 
        pull(bin_int)
      
      bin_ints_pos <- bin_data %>%
        filter(!grepl("NA",bin_int) & neg_int==FALSE) %>% # remove any with NA in interval
        arrange(bin_val) %>% 
        pull(bin_int)
      
      bin_ints <- c(bin_ints_neg,bin_ints_pos) #combined for easy use in ggplot
      
      # Length of bin vectors separately
      bin_ints_n <- c(length(bin_ints_neg),length(bin_ints_pos))
      
      # Create palette for diverging scale
      map_palette <- c(setNames((gla_pal(palette_type = "quantitative", 
                                         main_colours = pal_colour[1],
                                         n = bin_ints_n[1])),
                                nm=bin_ints_neg),
                       setNames(rev(gla_pal(palette_type = "quantitative", #reverse GLA pal order due to "bug"
                                            main_colours = pal_colour[2],
                                            n = bin_ints_n[2])),
                                nm=bin_ints_pos))
    }
    else {
      checkmate::assert_character(pal_colour,len=1) #must be of length one
      
      # Define bin cuts for interactive map
      # bin_cuts <- bin_data %>%  
      #   pull(bin_val)
      
      # Define categorical bin  intervals for ggplot
      bin_ints <- bin_data %>%
        filter(!grepl("NA",bin_int)) %>% # remove any with NA in interval
        arrange(bin_val) %>% 
        pull(bin_int)
      
      bin_ints_n <- length(bin_ints)
      
      # Create palette for uniform scale
      map_palette <- setNames(rev(gla_pal(palette_type = "quantitative", #reverse GLA pal order due to "bug"
                                          main_colours = pal_colour,
                                          n = bin_ints_n)),
                              nm=bin_ints)
    }
    
    
    # Create list of outputs to use in mapping functions
    map_list <- list(bin_cuts=bin_cuts,bin_ints=bin_ints,map_palette=map_palette,dataset_bins=dataset_bins)
    
    return(map_list)
    
  }
  
  ## Produce the interactive map ***
  leaflet_map_output <- function(dataset,
                                 dt_var,
                                 perc_bin=TRUE,
                                 bin_rounding=NULL,
                                 diverge_scale=FALSE, #whether negatives and positives needed
                                 pal_colour,
                                 title)
  {
    
    # If bins have percanteges, include in strings
    if (perc_bin==TRUE) perc_txt="%"
    else perc_txt=""
    
    
    # Automatic binning
    map_list <- map_prep(dataset=dataset,
                         dt_var=dt_var,
                         pal_colour=pal_colour,
                         perc_txt=perc_txt,
                         diverge_scale=diverge_scale,
                         bin_rounding=bin_rounding)
    
    # Pull necessary objects from prep function
    map_bins <- map_list$bin_cuts
    palette <- map_list$map_palette
    
    
    # Create the palette
    pal_func <- colorBin(palette, domain = dataset[[dt_var]], bins = map_bins)
    
    # Make tool tip
    labels <- sprintf("<strong>%s</strong><br/>%s",dataset$geography_name, dataset$tooltip) %>%
      lapply(htmltools::HTML)
    
    # Make map
    map_output1 <- leaflet(dataset) %>%
      addPolygons(fillColor = ~pal_func(dataset[[dt_var]]),
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  fillOpacity = 0.8,
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto"))    %>%
      addLegend(pal = pal_func, 
                values = dataset[[dt_var]], 
                opacity = 0.8,
                title = title,
                position = "bottomright", 
                labFormat = labelFormat(between = paste0(perc_txt," to "), suffix=perc_txt))
    
    
    # Find default bounds
    lng_b <- map_output1[["x"]][["limits"]][["lng"]]
    lat_b <- map_output1[["x"]][["limits"]][["lat"]]
    bounds_txt <- paste0("function(btn, map){ map.flyToBounds([[",lat_b[1],",",lng_b[1],"],[",lat_b[2],",",lng_b[2],"]]);}")
    
    map_output <- map_output1 %>% 
      addEasyButton(easyButton(
        icon="fa-globe", title="Default zoom",
        onClick=JS(bounds_txt)))
    
    return(map_output)
  }
  
  ## Produce the static map ***
  
  ggplot_map_output <- function(dataset,
                                dt_var,
                                bin_rounding=bin_rounding,
                                perc_bin=TRUE,
                                diverge_scale=FALSE, #whether negatives and positives needed
                                pal_colour=pal_colour,
                                title,
                                chart_name,
                                caption_txt)
  {
    
    # If bins have percanteges, include in strings
    if (perc_bin==TRUE) perc_txt="%"
    else perc_txt=""
    
    # Automatic binning
    map_list <- map_prep(dataset=dataset,
                         dt_var=dt_var,
                         pal_colour=pal_colour,
                         diverge_scale=diverge_scale,
                         perc_txt=perc_txt,
                         bin_rounding=bin_rounding)
    
    # Pull necessary objects from prep function
    map_bins <- map_list$bin_ints
    palette <- map_list$map_palette
    dataset_bins <- map_list$dataset_bins
    
    gg_map_chart <- dataset_bins %>%
      ggplot(aes(geometry = geometry,
                 fill = factor(value_interval,levels=map_bins))) +
      ggla_sf() +
      scale_fill_manual(values = palette) +
      labs(
        title = paste0(gsub("<br>","\n",title)),
        subtitle = paste0(""),
        caption = paste0(gsub("<br>","\n",caption_txt)))
    
    save_GLA_plot(plot_name = chart_name,h=8,w=12)
    
    return(gg_map_chart)
  }
  
  #_____________________________________________________________________________
  ### Downloads and data processing ----
  #_____________________________________________________________________________
  
  #.............................................................................
  ### London borough geographical codes ----
  #.............................................................................
  London_LA_codes <-readxl::read_excel(path = paste0(INPUT,"London borough codes.xlsx"), sheet = "Codes") %>% 
    clean_names() %>% 
    rename(geography_name=area)
  
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
  ClaimantCountDownload <- function( sa_nsa = NULL,
                                     time_period = c("1996-01","latest"),
                                     save_intermediate = TRUE,
                                     geography_v = boroughs_group,
                                     measures_v = 20100) {
    
    checkmate::assert_choice(sa_nsa, choices = c("sa","nsa"))
    
    ## Select data series depending on SA NSA
    if (sa_nsa=="sa") {
      data_series <- "NM_11_1"
      
      ### Download the claimant count data from NOMIS
      raw_nomis <- nomis_get_data(
        id = data_series, 
        geography = geography_v,
        measures = measures_v,
        time = time_period) 
      
      ### Clean data
      cc_stats <- raw_nomis %>% 
        mutate(DATE_DAY = as.Date(paste0(DATE, "-01"))) %>% 
        filter(!is.na(OBS_VALUE) & MEASURES %in% measures_v) %>% 
        clean_names() %>% 
        select( date_day, date_name, geography_name, sex_name, obs_value, measures_name) %>% #notice the different var names to NSA
        rename(measure_value=obs_value)
    }
    else {
      data_series <- "NM_162_1"
      
      ### Download the claimant count data from NOMIS
      raw_nomis <- nomis_get_data(
        id = data_series, 
        geography = geography_v,
        measures = measures_v,
        time = time_period) 
      
      ### Clean data
      cc_stats <- raw_nomis %>% 
        mutate( DATE_DAY = as.Date(paste0(DATE, "-01"))) %>% 
        filter(!is.na(OBS_VALUE) & MEASURES %in% measures_v) %>% 
        clean_names() %>% 
        select( date_day, date_name, geography_name, gender_name, age_name, obs_value, measure_name) %>% 
        rename( sex_name = gender_name,measure_value=obs_value)
      
    }
    
    ### Save intermediate dataframes if specified
    if( save_intermediate == TRUE) {
      fwrite(cc_stats, file = paste0(INTERMEDIATE,Sys.Date(),"_",data_series,"_claimant.csv"))
    }
    
    return(cc_stats) 
    
  }
  
  
  # ----- Download data and make wide and long version 
  
  claimant_count_stats_long <- ClaimantCountDownload( sa_nsa = "nsa", 
                                                      time_period = c("2015-01","latest"),
                                                      geography_v = c(2013265927,2092957697), 
                                                      save_intermediate = TRUE)  %>% 
    group_by(geography_name,age_name,sex_name,measure_name) %>% 
    mutate(change_month = measure_value - lag(measure_value , n = 1),
           change_month_percent = 100 * (measure_value - lag(measure_value, n = 1))/lag(measure_value, n = 1),
           change_year = measure_value - lag(measure_value, n = 12),
           change_year_percent = 100*(measure_value - lag(measure_value, n = 12))/lag(measure_value, n = 12)) %>% 
    ungroup
 
  ## Simple rates for SA claimant count
  claimant_count_stats_sa <- ClaimantCountDownload( sa_nsa="sa", 
                                                    time_period = c("2015-01","latest"),
                                                    measures_v = c(20203,20100),
                                                    geography_v = c(2013265927,2092957697), 
                                                    save_intermediate = TRUE)  %>% 
    pivot_wider(names_from = "measures_name", values_from = "measure_value") %>% 
    clean_names() %>% 
    rename(level=value) %>% 
    group_by(geography_name,sex_name) %>% 
    mutate(d_change_level_yoy = level - lag(level , n = 12),
           p_change_level_yoy = 100 * (level / lag(level , n = 12) - 1)) %>% 
    ungroup()
  
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
  
  #### Download latest PAYE file ----
  raw_paye <- download.file( url = paye_online_path,
                             destfile = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"),
                             mode = "wb")
  
  
  paye_emp_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "7. Employees (NUTS1)", skip = 6) %>% 
    mutate(date_day=my(Date),
           measure_name = "emps") %>% 
    select(date_day, London, UK,measure_name) 
  
  paye_pay_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "8. Median pay (NUTS1)", skip = 6) %>% 
    mutate(date_day=my(Date),
           measure_name = "median_pay") %>% 
    select(date_day, London, UK,measure_name) 
  
  paye_stats <- paye_emp_stats %>% 
    rbind(paye_pay_stats) %>% 
    pivot_longer(cols = c("UK","London"),names_to = "geography_name",values_to = "measure_value") %>% 
    group_by(geography_name,measure_name) %>% 
    mutate(d_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                      date_day > as.Date("2020-02-01") ~ measure_value - measure_value[date_day == "2020-02-01"]),
           p_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                      date_day > as.Date("2020-02-01") ~ (measure_value - measure_value[date_day == "2020-02-01"])/measure_value[date_day == "2020-02-01"]),
           d_change_mom = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) > 365 ~ 
                                      (measure_value - lag(measure_value, n = 1))),
           p_change_mom = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) > 365 ~ 
                                      (measure_value - lag(measure_value, n = 1))/lag(measure_value, n = 1)),
           d_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) > 365 ~ 
                                      (measure_value - lag(measure_value, n = 12))),
           p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) > 365 ~ 
                                      (measure_value - lag(measure_value, n = 12))/lag(measure_value, n = 12))) %>% 
    ungroup()
  
  
  # For industry/age groupings, create dataset representing total
  paye_pay_aux <- paye_pay_stats %>% 
    pivot_longer(cols = c("UK","London"),names_to = "geography_name",values_to = "measure_value") %>% 
    rename(Total = measure_value) 
  
  remove(paye_emp_stats) #do not remove paye_pay_stats as needed for CPIH below
  
  paye_overall_last_month <- format(max(paye_stats$date_day),"%B %Y")
  
  #### PAYE NUTS2 ----
  paye_nuts2_emp_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "11. Employees (NUTS2)", skip = 6) %>% 
    mutate( date_day = dmy(paste0("01",Date)),
            measure_name="emps")   %>% 
    select(date_day,measure_name, "Inner London - West",	"Inner London - East",	"Outer London - East and North East",	"Outer London - South",	"Outer London - West and North West")
  
  paye_nuts2_pay_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,Sys.Date(),"_raw_paye.xlsx"), sheet = "12. Median pay (NUTS2)", skip = 6) %>% 
    mutate( date_day = dmy(paste0("01",Date)),
            measure_name="median_pay")  %>%
    select(date_day,measure_name, "Inner London - West",	"Inner London - East",	"Outer London - East and North East",	"Outer London - South",	"Outer London - West and North West")
  
  
  paye_nuts2_stats <- paye_nuts2_emp_stats %>% 
    rbind(paye_nuts2_pay_stats)%>% 
    pivot_longer(cols="Inner London - West":"Outer London - West and North West",names_to = "geography_name", values_to = "measure_value") %>% 
    group_by(geography_name,measure_name) %>% 
    mutate( d_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                       date_day > as.Date("2020-02-01") ~ measure_value - measure_value[date_day == "2020-02-01"]),
            p_change_feb20 = case_when(date_day <= as.Date("2020-02-01") ~ NaN,
                                       date_day > as.Date("2020-02-01") ~ (measure_value - measure_value[date_day == "2020-02-01"])/measure_value[date_day == "2020-02-01"]),
            d_change_mom = case_when(date_day - min(date_day) < 365 ~ NaN,
                                     date_day - min(date_day) > 365 ~ 
                                       (measure_value - lag(measure_value, n = 1))),
            p_change_mom = case_when(date_day - min(date_day) < 365 ~ NaN,
                                     date_day - min(date_day) > 365 ~ 
                                       (measure_value - lag(measure_value, n = 1))/lag(measure_value, n = 1)),
            d_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                     date_day - min(date_day) > 365 ~ 
                                       (measure_value - lag(measure_value, n = 12))),
            p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                     date_day - min(date_day) > 365 ~ 
                                       (measure_value - lag(measure_value, n = 12))/lag(measure_value, n = 12))) %>% 
    ungroup() %>% 
    rbind(paye_stats)
  
  remove(paye_nuts2_emp_stats,paye_nuts2_pay_stats)
  
  ## -------------------------------------------------------------------------##
  #### PAYE monthly data ----
  # PAYE data has unique quarterly data on following months:
  ## - (a) By Age*NUTS1: 1,4,7,10
  ## - (b) By Industry*NUTS1: 2,5,8,11
  ## - (c) By LA: 3,6,9,12
  
  # These need to be updated as such:
  ## If division remainder is 0, then (c); if 1 then (a), if 2 then (b)
  ## If it is LMU release day, ensure data is updated
  
  current_month <- lubridate::month(Sys.Date())
  #month_remainder <- current_month %% 3
  release_dates <- c("2022-01-18","2022-02-15","2022-03-15","2022-04-12","2022-05-17",
                     "2022-06-14","2022-07-19","2022-08-16","2022-09-13","2022-10-11",
                     "2022-11-15","2022-12-13","2023-01-17","2023-02-14","2023-03-14",
                     "2023-04-18","2023-05-16","2023-06-13","2023-07-11","2023-08-15",
                     "2023-09-12","2023-10-17","2023-11-14","2023-12-12")
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
  
  #### PAYE by age ----
  #### (a) Each quarter, process data on PAYE employee age on regional level
  
  
  paye_age_emp_uk <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1age_name), sheet = "28. Employees (Age)", skip = 5) %>%
    mutate(date_day = dmy(paste0("01",Date)),
           measure_name = "emps",
           geography_name = "UK",
           Total = rowSums(across(c("0-17","18-24","25-34","35-49","50-64","65+")))) %>% 
    select(date_day,geography_name, measure_name, "0-17","18-24","25-34","35-49","50-64","65+",Total) 
  
  paye_age_pay_uk <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1age_name), sheet = "29. Median pay (Age)", skip = 5) %>%
    mutate(date_day = dmy(paste0("01",Date)),
           measure_name = "median_pay",
           geography_name = "UK") %>% 
    left_join(paye_pay_aux,by=c("date_day","geography_name","measure_name")) %>% #merge with total average median pay
    select(date_day,geography_name, measure_name, "0-17","18-24","25-34","35-49","50-64","65+",Total) 
  
  paye_nuts1age_emp <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1age_name), sheet = "32. Employees (NUTS1Age)", skip = 6) %>%
    rename_with(~str_replace(.x,pattern="London: ",replacement="")) %>% 
    mutate(date_day = dmy(paste0("01",Date)),
           measure_name = "emps",
           geography_name = "London",
           Total = rowSums(across(c("0-17","18-24","25-34","35-49","50-64","65+")))) %>% 
    select(date_day,geography_name, measure_name, "0-17","18-24","25-34","35-49","50-64","65+",Total)
  
  paye_nuts1age_pay <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1age_name), sheet = "33. Median pay (NUTS1Age)", skip = 6) %>%
    rename_with(~str_replace(.x,pattern="London: ",replacement="")) %>% 
    mutate(date_day = dmy(paste0("01",Date)),
           measure_name = "median_pay",
           geography_name = "London") %>% 
    left_join(paye_pay_aux,by=c("date_day","geography_name","measure_name")) %>% #merge with total average median pay
    select(date_day,geography_name, measure_name, "0-17","18-24","25-34","35-49","50-64","65+",Total) 
  
  
  paye_nuts1age_stats <- paye_nuts1age_emp %>%
    rbind(paye_nuts1age_pay,paye_age_emp_uk,paye_age_pay_uk) %>% 
    pivot_longer(cols="0-17":"Total",names_to = "age_group", values_to = "measure_value") %>% 
    group_by(age_group,geography_name,measure_name) %>% 
    mutate(p_change_feb20 = case_when( date_day <= as.Date("2020-02-01") ~ NaN,
                                       date_day > as.Date("2020-02-01") ~ (measure_value - measure_value[ date_day == "2020-02-01"])/(measure_value[ date_day == "2020-02-01"])),
           p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    date_day - min(date_day) >= 365 ~ (measure_value - lag(measure_value, n= 12))/lag(measure_value, n=12)),
           age_group = case_when( age_group=="Total" ~ age_group,
                                  TRUE ~ paste0("Aged: ", age_group))) %>%
    clean_names()   %>% 
    arrange(date_day,geography_name,age_group,measure_name) %>% 
    ungroup()
  
  remove(paye_nuts1age_emp,paye_nuts1age_pay,paye_age_emp_uk,paye_age_pay_uk)
  
  paye_age_last_month <- format(max(paye_nuts1age_stats$date_day),"%B %Y")
  
  
  
  #### PAYE by industry ----
  #### (b) Each quarter, process data on PAYE employees by industry at region level. 
  
  paye_ind_emp_uk <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1ind_name), sheet = "23. Employees (Industry)", skip = 6) %>% 
    mutate(date_day = dmy(paste0("01",Date)),
           geography_name="UK",
           measure_name = "emps",
           Total = rowSums(across(c("Agriculture, forestry and fishing":"Households and Extraterritorial")))) %>% 
    select(-c("UK"))
  
  paye_ind_pay_uk <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1ind_name), sheet = "24. Median pay (Industry)", skip = 6) %>% 
    mutate(date_day = dmy(paste0("01",Date)),
           geography_name="UK",
           measure_name = "median_pay") %>% 
    left_join(paye_pay_aux,by=c("date_day","geography_name","measure_name")) %>% #merge with total average median pay
    select(-c("UK"))  
  
  
  paye_nuts1ind_emp <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1ind_name), sheet = "36. Employees (NUTS1Sector)", skip = 6) %>% 
    mutate(date_day = dmy(paste0("01",Date)),
           geography_name="London",
           measure_name = "emps",
           Total = rowSums(across(c("London: Agriculture, forestry and fishing":"London: Households and Extraterritorial")))) %>% 
    select(c(Date,date_day,geography_name,contains("London"),measure_name,Total))  %>%
    rename_with(~str_replace(.x,pattern="London: ",replacement=""))
  
  paye_nuts1ind_pay <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_nuts1ind_name), sheet = "37. Median pay (NUTS1Sector)", skip = 6) %>% 
    mutate(date_day = dmy(paste0("01",Date)),
           geography_name="London",
           measure_name = "median_pay") %>% 
    select(c(Date,date_day,geography_name,contains("London"),measure_name))  %>%
    left_join(paye_pay_aux,by=c("date_day","geography_name","measure_name")) %>% #merge with total average median pay
    rename_with(~str_replace(.x,pattern="London: ",replacement=""))
  
  paye_nuts1ind_stats <- paye_nuts1ind_emp %>% 
    rbind(paye_nuts1ind_pay,paye_ind_emp_uk,paye_ind_pay_uk) %>% 
    pivot_longer(cols=c("Agriculture, forestry and fishing":"Households and Extraterritorial","Total"),
                 names_to = "industry_name", values_to = "measure_value") %>% 
    clean_names() %>% 
    rename(date_month=date) %>% 
    group_by(geography_name,industry_name,measure_name) %>% 
    mutate(
      p_change_feb20 = case_when( date_day <= as.Date("2020-02-01") ~ NaN,
                                  date_day > as.Date("2020-02-01") ~ (measure_value - measure_value[date_day == "2020-02-01"])/(measure_value[ date_day == "2020-02-01"])),
      p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                               date_day - min(date_day) >= 365 ~ (measure_value - lag(measure_value, n= 12))/lag(measure_value, n=12)),
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
    relocate(date_month,date_day,measure_name,geography_name,industry_name_simple) %>% 
    ungroup()
  
  remove(paye_nuts1ind_emp,paye_nuts1ind_pay,paye_ind_emp_uk,paye_ind_pay_uk)
  
  paye_ind_last_month <- format(max(paye_nuts1ind_stats$date_day),"%B %Y")
  
  #### PAYE by LA ----
  #### (c) Each quarter, process data on PAYE at borough level and merge with geographical codes. 
  
  la_columns <- c("City of London", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey", "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea", "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", "Wandsworth", "Westminster")
  
  paye_la_emp_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_locauth_name), sheet = "19. Employees (LA)", skip = 7) %>% 
    mutate(date_day = dmy(paste0("01",Date))) %>% 
    select( Date, date_day, all_of(la_columns)) %>% 
    pivot_longer(cols="City of London":"Westminster",names_to = "geography_name", values_to = "measure_value") %>% 
    mutate(measure_name="emps")
  
  paye_la_pay_stats <- readxl::read_excel(path = paste0(INTERMEDIATE,latest_paye_locauth_name), sheet = "20. Median pay (LA)", skip = 7) %>% 
    mutate(date_day = dmy(paste0("01",Date))) %>% 
    select( Date, date_day, all_of(la_columns)) %>% 
    pivot_longer(cols="City of London":"Westminster",names_to = "geography_name", values_to = "measure_value") %>% 
    mutate(measure_name="median_pay")
  
  paye_la_stats <-  paye_la_emp_stats %>% 
    rbind(paye_la_pay_stats) %>% 
    group_by(geography_name,measure_name) %>% 
    mutate( 
      p_change_feb20 = case_when( date_day <= as.Date("2020-02-01") ~ NaN,
                                  date_day > as.Date("2020-02-01") ~ 100*(measure_value - measure_value[ date_day == "2020-02-01"])/(measure_value[ date_day == "2020-02-01"])),
      p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                               date_day - min(date_day) >= 365 ~ 100*(measure_value - lag(measure_value, n= 12))/lag(measure_value, n=12))) %>% 
    clean_names() %>%  
    rename(date_month=date) %>% 
    merge(London_LA_codes,by='geography_name') %>% 
    arrange(geography_name,measure_name,date_day)
  
  paye_la_last_month <- format(max(paye_la_stats$date_day),"%B %Y")
  
  #.............................................................................
  ### Download Workforce Jobs data ----
  #.............................................................................
  
  
  wfj_stats <- nomis_get_data(id = "NM_130_1", time = c("2010-01", "latest"), geography = boroughs_group,
                              industry=c(37748736,150994945:150994964)) %>% 
    clean_names() %>% 
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
           cpih_mom_help=cpih_mom+1,
           cpih_3m_ave = (cpih_index_2015+lag(cpih_index_2015,n=1)+lag(cpih_index_2015,n=2))/3,
           cpih_3o3 = cpih_3m_ave/lag(cpih_3m_ave,n=3)-1,
           cpih_annualised_3m = (cpih_mom_help*lag(cpih_mom_help,n=2)*lag(cpih_mom_help,n=3))^4-1) %>% 
    group_by(year) %>% # Calculate annualised inflation using all preceding months
    mutate(cpih_yearsofar_help = cumprod(cpih_mom_help),
           cpih_annualised = cpih_yearsofar_help^(12/month)-1) %>% 
    ungroup()
  
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
    pivot_longer(cols = c(London,UK),names_to = "geography_name",values_to="median_pay_nominal") %>% 
    left_join(cpih_stats,by="date_day") %>% 
    mutate(median_pay_real=median_pay_nominal/cpih_index_2015*100) %>% 
    pivot_longer(cols=c(contains("_pay")),names_prefix="median_pay_",names_to = "pay_type",values_to="median_pay") %>% 
    mutate(geography_pay_type=paste0(geography_name,", ",pay_type)) %>% 
    group_by(geography_pay_type) %>% 
    arrange(geography_pay_type,date_day) %>% 
    mutate(p_change_yoy = case_when(date_day - min(date_day) < 365 ~ NaN,
                                    TRUE ~ (median_pay/lag(median_pay, n= 12)-1)),
           p_change_feb20 = case_when(date_day <= "2020-02-01" ~ (median_pay[date_day=="2020-02-01"]/median_pay-1), #If before Feb 20, then calculate change until then
                                      TRUE ~ (median_pay/median_pay[date_day=="2020-02-01"]-1)),
           pay_index_feb20 = 100*(median_pay/median_pay[date_day=="2020-02-01"]),
           pay_index_jan19 = 100*(median_pay/median_pay[date_day=="2019-01-01"]),
           months_from_feb20 = interval("2020-02-01",date_day) %/% months(1),
           p_change_mom = (median_pay/lag(median_pay, n= 1)-1),
           median_pay_3m_ave = (median_pay+lag(median_pay, n= 1)+lag(median_pay, n= 2))/3,
           p_change_3o3 = (median_pay_3m_ave/lag(median_pay_3m_ave, n= 3)-1),
           date_3m = paste0(format(lag(date_day,n=2),"%b %y"),"-",format(date_day,"%b %y")),
           p_annualised_3m = ((1+p_change_mom)*(1+lag(p_change_mom,n=1))*(1+lag(p_change_mom,n=2)))^4-1) %>%  #Annualised change based on three rolling months
    ungroup()
  
  #.............................................................................
  ### Geographical data ----
  #.............................................................................
  
  # Read in data and transform to long-lat format geometry
  paye_la_stats_geo <- read_sf(here("INPUT","statistical-gis-boundaries-london","ESRI","London_Borough_Excluding_MHW.shp")) %>% 
    clean_names() %>% 
    select(name,geometry) %>% 
    rename(geography_name=name) %>%  
    left_join(paye_la_stats,by="geography_name") %>% 
    mutate(tooltip= paste("Change: ", perc_form(p_change_feb20),"%", "\n", sep = "")) %>% 
    filter(date_day==max(date_day)) %>% 
    st_transform(4326)
  