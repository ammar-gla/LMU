#_______________________________________________________________________________
# CREATE ALL STANDARD FUNCTIONS USED FOR GLAE MICRO OUTPUTS ----
#_______________________________________________________________________________


# Define function to always present percentages with one decimal and figures without decimals
perc_form = function(x, d=1) sprintf(paste0("%1.",d,"f"), x) 

value_form = function(x,s=2,d= -1) format(signif(round(as.numeric(x), d),s), big.mark=",")


# Function to make conditional text based on values
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

# Function to remove negative sign from character strings (needed as formatted numbers are not numerical)
abs2 <- function(x) {
  y = gsub("-","",x)
  return(y)
}

# Helper function to ensure legend labels are placed correctly
reverse_legend_labels <- function(plotly_plot) {
  n_labels <- length(plotly_plot$x$data)
  plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
  plotly_plot
}

# Helper function to re-introduce simplified modebar allowing chart download
plotly_modebar <- function(plotly_plot) {
  plotly_plot <- plotly_plot %>% plotly::config(displayModeBar = TRUE) %>% # Allows menu bar such that image can be downloaded
    plotly::config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d","zoom2d","pan2d","autoScale2d","hoverClosestCartesian","hoverCompareCartesian","select2d","lasso2d")) %>% 
    plotly::config(displaylogo = FALSE)
  plotly_plot
}

# To help pull data
pull_dtpt <- function(dt=NULL,flt=NULL,vr=NULL) {
  dtpt <- dt %>% 
    filter(eval(rlang::parse_expr(flt))) %>% 
    pull(var=vr)
  
  return(dtpt)
}
# THe function above can be used as follows:
# pull_dtpt(dt=paye_nat_long,flt="nationality == 'EU' & region== 'London' & date_day == max(date_day)",vr="employee_count")

# Function to save last chart displayed in PNG and SVG
save_GLA_plot <- function(plot_name, w=8, h=8) {
  ggsave(paste0(IMAGES,Sys.Date(),"_",plot_name,".svg"), device = "svg", width = w, height = h, units = "in")
  ggsave(paste0(IMAGES,Sys.Date(),"_",plot_name,".png"), device = "png", width = w, height = h, units = "in")
}

# Standard ggplotly settings
## Note: the title and subtitle text need to be enclosed within a paste function within single quotes, such that functions can be evaluated

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


# Function to assert URL exists
urlFileExist <- function(url){
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status == HTTP_STATUS_OK)
}

#.............................................................................
### Charting functions ----
#.............................................................................

LFS_line_chart <- function( data_set = lfs_stats, ### This function has been written to work specifically with the NM_59_1 dataset
                            lfs_or_claims = "lfs",
                            x_var = date_day, ### Assumed that this will be a line chart with dates on the x-axis
                            min_x_var = as.Date("2015-01-01"), 
                            y_var = NULL, ### See document "[today's date]colnames.csv" for variables available for charting
                            geography = c("London", "United Kingdom"),
                            suffix = "%",
                            y_limits = c(0,100),
                            nudge_y = NULL,
                            title = NULL,
                            subtitle = NULL,
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
  
  new_release <- format(max(for_charting$date_day),"%B %Y")
  if (is.null(subtitle)) subtitle = paste0("Latest data for period ", new_release)
  
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
    # ggla_highlight(mapping = aes(label = paste0({{y_var}}, "%")),
    #                geom = GeomGLATextHighlight, filter_type = "end",  size = 4.5,
    #                position = position_nudge(y = nudge_y),check_overlap = TRUE)+
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

GLAbarchart <- function( data_set = lfs_regions_charting,
                         x_var = geography_name, ### Assumed that this will be a line chart with dates on the x-axis
                         y_var = NULL, ### See document "[today's date]colnames.csv" for variables available for charting
                         y_limits = c(0,100),
                         nudge_y = NULL,
                         title = NULL,
                         subtitle = NULL,
                         caption = "Source: HM Revenue and Customs - Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live.",
                         chart_name = NULL) 
{
  
  new_release <- format(max(data_set$date_day),"%B %Y")
  if (is.null(subtitle)) subtitle = paste0("Latest data for period ", new_release)
  
  # Set palette to highlight London among regions
  regions_nm <- c("London",setdiff(sort(unique(data_set$geography_name)),"London"))
  regions_n <- length(regions_nm)
  pal <- setNames(gla_pal(palette_type = "highlight",n=c(1,regions_n-1)),regions_nm)
  
  # CHart
  lfs_barchart <- data_set %>% 
    mutate(chart_ranking = case_when(geography_name=="United Kingdom" ~ 0,
                                     TRUE ~ rank({{y_var}}))) %>% 
    ggplot(mapping = aes(x = reorder({{x_var}},chart_ranking), 
                         y = {{y_var}}/100, # Divide to fit into percentage axis
                         fill=geography_name,
                         text = paste0(geography_name, "\n",
                                       "Rate: ", perc_form({{y_var}}),"%", "\n"))) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.4)+
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_fill_manual(values = pal) +
    theme_set(theme_gla(gla_theme = "default")) +
    scale_y_continuous(labels = percent_format(accuracy=1),
                       limits=y_limits) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
    theme(axis.text.x = element_text( hjust=1, vjust=0.5)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title = title,
         subtitle = subtitle,
         caption = caption) +
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)),
          legend.position = "none")
  
  save_GLA_plot(plot_name = chart_name)
  
  return(lfs_barchart)
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

#...............................................................................
# Data downloading ----
#...............................................................................

# Determine what most recent release date is for labour market (lm) dataand download relevant dataset
lm_release_date_checker <- function(release_dates_lm_vec = release_dates_lm,
                                    release_dates_cpih_vec = release_dates_cpih,
                                    data_type="lm") # Account for quarterly WFJ releases or CPIH, LM is default
{
  
  # Data type has to be among specific options
  data_types_options <- c("lm","wfj","cpih")
  checkmate::assert_choice(data_type, choices = data_types_options)
  
  # WFJ and LM use same dates, while CPIH use different dates
  if (data_type %in% c("lm","wfj")) release_dates_vec <- release_dates_lm_vec
  if (data_type=="cpih") release_dates_vec <- release_dates_cpih_vec
  
  # Save the relevant most recent date
  for (i in 1:length(release_dates_vec)) {
    
    if (data_type %in% c("lm","cpih")) { #simply take latest date
      
      if (release_dates_vec[i] <= Sys.Date()) last_release_date <-  release_dates_vec[i]
    }    else if (data_type=="wfj") { #only keep if WFJ released on date
      
      if (release_dates_vec[i] <= Sys.Date() & month(release_dates_vec[i]) %in% release_months_wfj) last_release_date <-  release_dates_vec[i]
    }
  }
  
  return(last_release_date)
}

#...............................................................................
# LFS download
#...............................................................................
# Download LM data from Nomis
HeadlineDownload <- function(data_series = "NM_59_1", 
                             time_period = c("1996-01","latest"), 
                             save_intermediate = TRUE,
                             data_name = NULL, # used for differentiating datasets
                             release_date = NULL, #should be formatted as 'yy_mm_dd' 
                             econ_activity=c(0,3,7,9), #main ones used
                             sex=c(5,6,7), #all sexes
                             geography = NULL,
                             force_download=FALSE) { # In case new datasets are needed
  
  # If release date is left as null, run the function to check against most recent release date
  if (is.null(release_date)) {
    release_date <- lm_release_date_checker()
  }
  
  release_date_form <- format(release_date,"%y_%m_%d")
  
  # Check if most recent data is already downloaded. If not, download it now
  data_file_name_path_csv <- paste0(OTHERDATA,release_date_form,"_LFS_",data_name,".csv")
  data_file_name_path_rds <- paste0(RDATA,release_date_form,"_LFS_",data_name,".rds")
  
  if (file.exists(data_file_name_path_rds) & force_download==FALSE) { # Load existing file
    
    lfs_stats <- readRDS(file=data_file_name_path_rds)
  }
  
  else { # Download data
    
    ### Download both absolute and percentage figures from NOMIS
    raw_nomis <- nomis_get_data( id = data_series, 
                                 geography = geography,
                                 time = time_period,
                                 economic_activity=econ_activity,
                                 sex=sex)
    
    ### Store downloads as dataframes
    lfs_stats <- raw_nomis %>% 
      mutate(DATE_DAY = as.Date(paste0(DATE, "-01"))) %>% 
      filter(MEASURES %in% c(20207,20100) & !is.na(OBS_VALUE)) %>%
      select( "DATE_DAY", "DATE_NAME", "SEX_NAME", "GEOGRAPHY_NAME", "ECONOMIC_ACTIVITY_NAME", "VALUE_TYPE_NAME", "OBS_VALUE","MEASURES","MEASURES_NAME") %>% 
      pivot_wider( names_from = ECONOMIC_ACTIVITY_NAME, values_from = OBS_VALUE) %>%  
      clean_names()
    
    ### Save intermediate dataframes if specified
    if(save_intermediate == TRUE) {
      
      fwrite(lfs_stats, file = data_file_name_path_csv)
      saveRDS(lfs_stats,file = data_file_name_path_rds)
    }
    
  }
  
  return(lfs_stats)
}

#...............................................................................
# Claimant count data download
#...............................................................................

ClaimantCountDownload <- function(sa_nsa = NULL,
                                  time_period = c("1996-01","latest"),
                                  save_intermediate = TRUE,
                                  geography_v = c(london_geo_code,eng_geo_code,uk_geo_code,regions_geo_code,boroughs_group),
                                  measures_v = 20100,
                                  data_name = NULL, # used for differentiating datasets
                                  release_date = NULL, #should be formatted as 'yy_mm_dd' 
                                  force_download=FALSE,  # In case new datasets are needed
                                  ...) {
  
  checkmate::assert_choice(sa_nsa, choices = c("sa","nsa"))
  
  # If release date is left as null, run the function to check against most recent release date
  if (is.null(release_date)) {
    release_date <- lm_release_date_checker()
  }
  
  release_date_form <- format(release_date,"%y_%m_%d")
  
  # Check if most recent data is already downloaded. If not, download it now
  data_file_name_path_csv <- paste0(OTHERDATA,release_date_form,"_CC_",sa_nsa,"_",data_name,".csv")
  data_file_name_path_rds <- paste0(RDATA,release_date_form,"_CC_",sa_nsa,"_",data_name,".rds")
  
  if (file.exists(data_file_name_path_rds) & force_download==FALSE) { # Load existing file
    
    cc_stats <-readRDS(file=data_file_name_path_rds)
  }
  
  else { # Download data
    
    ## Select data series depending on SA NSA
    if (sa_nsa=="sa") {
      data_series <- "NM_11_1"
      
      ### Download the claimant count data from NOMIS
      raw_nomis <- nomis_get_data(
        id = data_series, 
        geography = geography_v,
        measures = measures_v,
        time = time_period,
        ...) 
      
      ### Clean data
      cc_stats <- raw_nomis %>% 
        mutate(DATE_DAY = as.Date(paste0(DATE, "-01"))) %>% 
        filter(OBS_STATUS!="Q") %>% # removes data points which do not have value, e.g. rate for X age group 
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
        time = time_period,
        ...) 
      
      ### Clean data
      cc_stats <- raw_nomis %>% 
        mutate( DATE_DAY = as.Date(paste0(DATE, "-01"))) %>% 
        filter(OBS_STATUS!="Q") %>% # removes data points which do not have value, e.g. rate for X age group 
        clean_names() %>% 
        select( date_day, date_name, geography_name, gender_name, age_name, obs_value, measure_name) %>% 
        rename( sex_name = gender_name,measure_value=obs_value)
      
    }
    
    ### Save intermediate dataframes if specified
    if( save_intermediate == TRUE) {
      fwrite(cc_stats, file = data_file_name_path_csv)
      saveRDS(cc_stats,file = data_file_name_path_rds)
    }
  }
  
  return(cc_stats) 
}

#...............................................................................
# PAYE data download
#...............................................................................
## This function checks whether new data has been released and downloads it.
## If it already exists as datafile in project, load it
## Checks for latest overall, and also for quarterly updates on specific topics

paye_rti_download <- function(release_dates_vec=release_dates_lm,
                              force_download=FALSE) {
  
  # Loop through dates from oldest to newest and replace datasets when appropriate
  ## Note: special instance of release date checker as it also saves previous months' datasets
  for (i in 1:length(release_dates_vec)) {
    if (release_dates_vec[i] <= Sys.Date()) { #If we passed the day, assign dataset name
      
      month_remainder <- lubridate::month(release_dates_vec[i]) %% 3 # Check which dataset is updated
      month_format <- format(release_dates_vec[i],"%b%Y")
      release_date_form <- format(release_dates_vec[i],"%y_%m_%d")
      
      latest_paye_overall_name <- tolower(paste0(release_date_form,"_raw_paye_rtisa",".xlsx")) # this will be overridden to contain latest only
      
      # Assign for each of the detailed dataasets
      ## Set name as appearing on ONS website and names we use, separately
      if (month_remainder == 0) {
        
        latest_paye_locauth_name <- tolower(paste0(release_date_form,"_raw_paye_rtisa",".xlsx"))
        latest_paye_locauth_url <- tolower(paste0("rtisa",month_format,".xlsx"))
        
      } else if (month_remainder==1) {
        
        latest_paye_nuts1age_name <- tolower(paste0(release_date_form,"_raw_paye_rtisa",".xlsx"))
        latest_paye_nuts1age_url <- tolower(paste0("rtisa",month_format,".xlsx"))
        
      } else if (month_remainder==2) {
        
        latest_paye_nuts1ind_name <- tolower(paste0(release_date_form,"_raw_paye_rtisa",".xlsx"))
        latest_paye_nuts1ind_url <- tolower(paste0("rtisa",month_format,".xlsx"))
        
      }
    }
  }
  
  # Create list with the names of the PAYE datasets as saved in project
  paye_data_paths_list <- list(overall=latest_paye_overall_name,
                               locauth=latest_paye_locauth_name,
                               nuts1age=latest_paye_nuts1age_name,
                               nuts1ind=latest_paye_nuts1ind_name)
  
  # Download the three most recent datasets
  ## URL on ONS website
  paye_online_path <- "https://www.ons.gov.uk/file?uri=/employmentandlabourmarket/peopleinwork/earningsandworkinghours/datasets/realtimeinformationstatisticsreferencetableseasonallyadjusted/current/"
  
  ## Loop through the dataset names, check each and save as appropriate
  for (dta_name in c("latest_paye_locauth","latest_paye_nuts1age","latest_paye_nuts1ind")) {
    
    # Retrieve the path name and file name
    dta_name_file <- eval(parse(text=paste0(dta_name,"_name")))
    dta_name_url <- eval(parse(text=paste0(dta_name,"_url")))
    download_path <- paste0(paye_online_path,dta_name_url)
    
    # If data does not already exists, or we want to force it to re-download
    
    if (file.exists(paste0(OTHERDATA,dta_name_file)) & force_download==FALSE) {
      # do nothing
    }
    else {
      # Check if link exists
      stopifnot("URL path for PAYE data does not exist"=urlFileExist(download_path)==TRUE)
      
      # Download
      download.file(url = download_path,
                    destfile = paste0(OTHERDATA,dta_name_file),
                    mode = "wb")
    }
  }
  
  # Return a list with the relevant datasets
  return(paye_data_paths_list)
}


#...............................................................................
# CPIH data download
#...............................................................................

cpih_download <- function(force_download=FALSE) {
  # Save files externally, using the most recent date
  cpih_last_release_date <- format(lm_release_date_checker(data_type="cpih"),"%y_%m_%d")
  
  cpih_index_file_name <- paste0(cpih_last_release_date,"_raw_cpih_index")
  cpih_rate_file_name <- paste0(cpih_last_release_date,"_raw_cpih_rate")
  
  # If the file exists, load it. Otherwise, download it and save - assume either both or neither exists
  if (file.exists(paste0(RDATA,cpih_index_file_name,".rds")) & redownload_all==FALSE) {
    
    cpih_index_raw <- readRDS(paste0(RDATA,cpih_index_file_name,".rds"))
    cpih_rate_raw <- readRDS(paste0(RDATA,cpih_rate_file_name,".rds"))
    
  } else {
    
    # Download both
    download.file(url = "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l522/mm23",
                  destfile = paste0(OTHERDATA,cpih_index_file_name,".csv"),
                  mode = "wb")
    download.file(url = "https://www.ons.gov.uk/generator?format=csv&uri=/economy/inflationandpriceindices/timeseries/l55o/mm23",
                  destfile = paste0(OTHERDATA,cpih_rate_file_name,".csv"),
                  mode = "wb")
    
    # Load them 
    cpih_index_raw <- read.csv(paste0(OTHERDATA,cpih_index_file_name,".csv"),
                               header=FALSE,
                               stringsAsFactors=FALSE)
    cpih_rate_raw <- read.csv(paste0(OTHERDATA,cpih_rate_file_name,".csv"),
                              header=FALSE,
                              stringsAsFactors=FALSE)
    
    # Save as Rdata
    saveRDS(cpih_index_raw, file = paste0(RDATA,cpih_index_file_name,".rds"))
    saveRDS(cpih_rate_raw, file = paste0(RDATA,cpih_rate_file_name,".rds"))
  }
  
  # Return list with datasets
  cpih_data_list <- list(cpih_index=cpih_index_raw,cpih_rate=cpih_rate_raw)
  
  return(cpih_data_list)
}

#...............................................................................
# WFJ data download
#...............................................................................

wfj_download <- function(force_download=FALSE) {
  
  # Save files externally, using the most recent date
  wfj_last_release_date <- format(lm_release_date_checker(data_type="wfj"),"%y_%m_%d")
  
  wfj_file_name <- paste0(wfj_last_release_date,"_raw_wfj")
  
  # If the file exists, load it. Otherwise, download it and save
  if (file.exists(paste0(RDATA,wfj_file_name,".rds")) & redownload_all==FALSE) {
    
    wfj_stats_raw <- readRDS(paste0(RDATA,wfj_file_name,".rds"))
    
  } else {
    
    wfj_stats_raw <- nomis_get_data(id = "NM_130_1", time = c("2010-01", "latest"), 
                                    geography = c(london_geo_code,uk_geo_code),
                                    industry=c(37748736,150994945:150994964))
    
    saveRDS(wfj_stats_raw, file = paste0(RDATA,wfj_file_name,".rds"))
    fwrite(wfj_stats_raw, file = paste0(OTHERDATA,wfj_file_name,".csv"), na = "NaN")
    
  }
  
  # Return dataset to environment
  return(wfj_stats_raw)
}