  
  #*****************************************************************************
  ### SET UP ----
  #*****************************************************************************
  
  ## Put in your Nomis API key (NB: THE BELOW IS AMMAR'S KEY!!)
  Sys.setenv(NOMIS_API_KEY = "0x01a88c6659d20042f087de2e585cdf3a07708983")

  # Needed to orientate the script into folder structure
  library("here")


  # Run the main data script
  source(here::here("SCRIPTS",paste0("01a LMU dataload (m) ","22-09-14",".r")))
  
  # Set specific folder for ad hoc output
  IMAGE_FOLDER <- paste0(here::here(),"/IMAGES/AD_HOC/")
  
  ### Geographic codes for UK, London and Boroughs.
  ### See "Borough codes.csv" in source folder for reference. 
  
  Group <- c(2013265927,2092957697,1811939540,1811939541,1811939542,1811939543,1811939544,1811939526,1811939527,1811939545,1811939546,1811939547,
             1811939548,1811939528,1811939529,1811939530,1811939549,1811939550,1811939551,1811939552,1811939531,1811939532,1811939553,1811939533,
             1811939534,1811939554,1811939535,1811939555,1811939556,1811939536,1811939557,1811939537,1811939558,1811939538,1811939539)
  
  #*****************************************************************************
  ### Presets for presenting figures etc. ----
  #*****************************************************************************
  
  signif_thous = 3 #How many significant figures to use when figures are in thousands
  signif_mil = 2 # For millions
  signif_perc = 2 #For percentages
  signif_pp = 3 #For percentage points
  
  digits_mil = 1
  digits_perc = 1
  
  #*****************************************************************************
  ### Functions ----
  #*****************************************************************************

  
  ### Function to save last chart displayed in PNG and SVG [TO REPLACE STANDARD SIZES]
  save_GLA_plot <- function(plot_name, w=6, h=8) {
    #ggsave(paste0(IMAGE_FOLDER,Sys.Date(),"_",plot_name,".svg"), device = "svg", width = w, height = h, units = "in")
    ggsave(paste0(IMAGE_FOLDER,Sys.Date(),"_",plot_name,".png"), device = "png", width = w, height = h, units = "in")
  }
  
  #*****************************************************************************
  # Data variables ----
  #*****************************************************************************
  
  # Paye
  paye_d_month <- value_form(paye_stats %>% filter( date_day == max(date_day)) %>% select(lon_d_change_mom),s= signif_thous)
  
  paye_p_month <- perc_form(paye_stats %>% filter( date_day == max(date_day)) %>% select(lon_p_change_mom),d = digits_perc)
  
  paye_newmonth <- paye_stats %>% filter( date_day == max(date_day)) %>% mutate(date_day=format(date_day,'%B %Y')) %>%  select(date_day)
  
  paye_lastmonth <- paye_stats  %>% mutate(lag_month = lag(date_day, n = 1)) %>% mutate(lag_month2 = format(lag_month, '%B')) %>% filter( date_day == max(date_day)) %>%  select(lag_month2)
  
  # LFS
  
  lfs_newmonth <- lfs_stats %>% mutate(month = format(date_day, '%B %Y')) %>% filter(sex_name == "Total"  & geography_name == "London" & value_type_name == "Level" & date_day == max(date_day) & measures == 20207) %>%  select(month)
  
  lfs_lastmonth <- lfs_stats  %>% mutate(lag_month = ymd(date_day) %m+% - months(2)) %>% mutate(lag_month2 = format(lag_month, '%B')) %>% filter(sex_name == "Total"  & geography_name == "London" & value_type_name == "Level" & date_day == max(date_day) & measures == 20207) %>%  select(lag_month2)
  
  
  # WFJ
  
  wfj_year <-  wfj_stats %>% filter(date_day == max(date_day)) %>% filter(row_number()==1) %>% mutate(year=year(date_day)) %>% select(year)
  
  wfj_newquarter <- wfj_stats %>% ungroup()  %>% filter(date_day == max(date_day)) %>% filter(row_number()==1) %>% select(quarter_text) 
  
  wfj_oldquarter <-  wfj_stats %>% filter(date_day == (ymd(max(date_day)) %m+% - months(3))) %>% filter(row_number()==1) %>% select(quarter_text)

  #*****************************************************************************
  # Paye main ----
  #*****************************************************************************

  # Plotting process:
  ## First set the relevant colour palette and them
  ## Produce the ggplot and save it on the shared drive
  ## Produce the interactive ggplotly chart for the markdown.
  
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  theme_set(theme_gla(gla_theme = "default"))
  
  
  paye_stats_chart <- paye_stats %>%
    pivot_longer(cols = contains("_change_"), names_to = "geography", values_to = "change" ) %>%
    mutate( geography_name = case_when( grepl("lon",geography) ~ "London",
                                        grepl("uk",geography) ~ "United Kingdom")) %>%
    mutate(var_type = sub(".*?_",'',geography)) %>% 
    filter( date_day >= "2016-01-01" & var_type == "p_change_yoy") %>%
    ggplot(mapping = aes(x = date_day, y = change,
                         colour = geography_name, group = geography_name,
                         text = paste(
                           geography_name, "\n",
                           format(date_day,'%B %Y'), "\n",
                           "Change: ", perc_form(change),"%", "\n",
                           sep = ""))) +
    ggla_line(aes(size= geography_name)) +
    geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
               linetype = "dotted",
               size = 1 * mm_to_pt,
               colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
    scale_size_manual(values = c(2 * mm_to_pt, 1 * mm_to_pt)) +
    scale_colour_manual(values = pal) +
    ggla_highlight(filter_type = "end") +
    ggla_highlight(mapping = aes(label = paste0(format(change, digits = 1),"%")),
                   geom = GeomGLATextHighlight, filter_type = "end",  size = 4.5,
                   position = position_nudge(y = c(+0.4,+0.4)))+
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(expand = c(0, 0), labels = dollar_format(prefix = "",
                                                                suffix = "%",
                                                                largest_with_cents = 1),
                       limits = c(-6,8)) +
    scale_x_date( date_breaks = "1 year",
                  date_labels = "%b %Y",
                  expand = expansion( mult = c(0.05,0.05))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title =  "Payrolled employees, change on previous year",
         subtitle = paste0("Latest data for ", paye_overall_last_month),
         caption = "\nSource: HM Revenue and Customs - Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live. March 2020 indicated by dotted line.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "paye")

  #*****************************************************************************
  # Paye change feb20 ----
  #*****************************************************************************
  
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  
  paye_stats_barchart_feb20 <- paye_nuts2_stats %>%
    filter( date_day == max(date_day)) %>%
    ungroup() %>% 
    mutate(chart_ranking = case_when( area == "UK" ~ 1,
                                      area == "London" ~ 2,
                                      TRUE ~ dense_rank(desc(p_change_feb20))+2)) %>% 
    ggplot(mapping = aes(x =  reorder(area, -chart_ranking), y = p_change_feb20,
                         colour = area, group = area, fill=area,
                         text = paste(
                           area, "\n",
                           "Change: ", perc_form(p_change_feb20),"%", "\n",
                           sep = ""))) +
    geom_bar(width=0.5, 
             stat="identity")+
    geom_text(aes(label=paste(format(round(p_change_feb20,1), digits = 2), "%"),
                  y = p_change_feb20-0.13*((p_change_feb20)/abs(p_change_feb20))),
              color=c("white","white","white","white","white","white","black"))+ #cannot use vjust with ggplotly
    scale_color_manual(values = c(pal[1],pal[1],pal[1],pal[1],pal[1],pal[1],pal[2]), aesthetics = "colour") +
    scale_fill_manual(values = c(pal[1],pal[1],pal[1],pal[1],pal[1],pal[1],pal[2]), aesthetics =  "fill") +
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(labels = dollar_format(prefix = "",
                                              suffix = "%",
                                              largest_with_cents = 1),
                       limits = c(-.5,3)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.text=element_text(size=12))+
    labs(title =  "Payrolled employees, change since February 2020 by NUTS2 region",
         subtitle = paste0("Latest data for ", paye_overall_last_month),
         caption = "\nSource: HM Revenue and Customs - Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live.") + 
    theme(legend.position = "none",
          plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))

  save_GLA_plot(plot_name = "paye_nuts2_feb20",w=8,h=8)

  #*****************************************************************************
  # Paye change year ----
  #*****************************************************************************
  
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  theme_set(theme_gla(gla_theme = "default"))
  
  paye_stats_barchart <- paye_nuts2_stats %>%
    filter(date_day == max(date_day)) %>%
    ungroup() %>% 
    mutate(chart_ranking = case_when( area == "UK" ~ 1,
                                      area == "London" ~ 2,
                                      TRUE ~ dense_rank(desc(p_change_yoy))+2)) %>% 
    ggplot(mapping = aes(x =  reorder(area, -chart_ranking), y = p_change_yoy,
                         colour = area, group = area, fill=area,
                         text = paste(
                           area, "\n",
                           "Change: ", perc_form(p_change_yoy),"%", "\n",
                           sep = ""))) +
    geom_text(aes(label=paste0(format(round(p_change_yoy,1), digits = 2), "%"), 
                  y = p_change_yoy-0.15*min(p_change_yoy)), 
              color=c("white","white","white","white","white","white","black"))+ #cannot use vjust with ggplotly
    geom_bar(stat="identity",width=0.5)+
    scale_color_manual(values = c(pal[1],pal[1],pal[1],pal[1],pal[1],pal[1],pal[2]), aesthetics = "colour") +
    scale_fill_manual(values = c(pal[1],pal[1],pal[1],pal[1],pal[1],pal[1],pal[2]), aesthetics = "fill") +
    scale_size_manual(values = c(4 * mm_to_pt, 2 * mm_to_pt)) +
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(labels = dollar_format(prefix = "",
                                              suffix = "%",
                                              largest_with_cents = 1),
                       limits = c(0,8)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    
    theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.text=element_text(size=12))+
    labs(title =  "Payrolled employees, change on previous year by NUTS2 region",
         subtitle = paste0("Latest data for ", paye_overall_last_month),
         caption = "\nSource: HM Revenue and Customs - Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live.") + 
    theme(legend.position = "none") +
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "paye_nuts2_yoy")

  #*****************************************************************************
  # Paye industry ----
  #*****************************************************************************
  
  # Find industries with lowest and highest growth in recent data
  
  sector_thresh_paye <- 25000
  
  lowest_growth_ind_paye <- paye_nuts1ind_stats %>% 
    filter(date_day == max(date_day) & geography_name == "London" & emp_level>=sector_thresh_paye & !is.na(p_change_feb20)) %>%
    slice_min(p_change_feb20,n=3) 
  
  highest_growth_ind_paye <- paye_nuts1ind_stats %>% 
    filter(date_day == max(date_day) & geography_name == "London" & emp_level>=sector_thresh_paye & !is.na(p_change_feb20)) %>%
    slice_max(p_change_feb20,n=3) 
  
  ## Find industry names and growth figures
  
  for (measure in c("lowest","highest")) {
    
    dat_temp <- eval(as.name(paste0(measure,"_growth_ind_paye")))
    
    for (num_la in 1:3) {
      
      growth_x_name <- dat_temp %>% filter(row_number()==num_la) %>% pull(industry_name_simple)
      
      growth_x_p <- perc_form(dat_temp %>% filter(row_number()==num_la) %>% pull(p_change_feb20),d = 1)
      
      assign(paste0(measure,"_growth_ind_paye_",num_la,"_name"),growth_x_name)
      assign(paste0(measure,"_growth_ind_paye_",num_la,"_p"),growth_x_p)
      remove(growth_x_name)
      remove(growth_x_p)
    }
    remove(dat_temp)
  }

 
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  theme_set(theme_gla(gla_theme = "default"))
  
  # Select industries large in London, here 25k
  lon_large_inds_paye <- paye_nuts1ind_stats %>%
    filter( emp_level >=sector_thresh_paye 
            & geography_name =="London" & date_day == max(date_day)) %>% 
    select(industry_name_simple)
  
  lon_large_inds_paye <- unlist(lon_large_inds_paye)
  
  paye_industry_bar <- paye_nuts1ind_stats %>%
    filter(date_day == max(date_day) & industry_name_simple %in% lon_large_inds_paye) %>% 
    group_by(geography_name) %>% #To ensure chart ranking is correct, make rank based on London and assign within sector
    mutate(chart_ranking = case_when(
      geography_name=="London" ~ dense_rank(desc(p_change_feb20)))) %>% 
    group_by(industry_name_simple) %>% 
    mutate(chart_ranking=min(chart_ranking,na.rm=TRUE)) %>% 
    ungroup() %>% 
    arrange(chart_ranking) %>% 
    ggplot(mapping = aes(x =  factor(reorder(industry_name_simple,-chart_ranking)), 
                         y = p_change_feb20, 
                         colour = factor(reorder(geography_name,desc(geography_name))) ,#since horizontal bar reverses orders, we need to reverse too
                         fill=factor(reorder(geography_name,desc(geography_name))),
                         width = 0.5,
                         text = paste(industry_name_simple, "\n",
                                      geography_name, "\n",
                                      "Change: ", perc_form(p_change_feb20),"%", "\n",
                                      sep = ""))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_color_manual(values = rev(pal), aesthetics = "colour")+
    scale_fill_manual(values = rev(pal), aesthetics = "fill")+
    theme_set(theme_gla(gla_theme = "default", y_label_length=100)) + #GLA theme and removes lines below y-axis labels
    scale_y_continuous(limits = c(-10, 16), labels = dollar_format(prefix = "", 
                                                                   suffix = "%", 
                                                                   largest_with_cents = 1)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    scale_x_discrete(expand = c(0,0)) +
    theme(panel.grid.major.y = element_blank(), #removed y grid
          panel.grid.minor.y = element_blank(), # removes small y grid
          panel.grid.major.x = element_line( size=.5 ), # removes x grid
          axis.text.y = ggplot2::element_text( # ensures  y axis labels stay outside chart
            hjust = 0, vjust = 0.5,
            margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0,
                                     unit = "pt")),
          axis.ticks.length.y = ggplot2::unit(x = 0, units = "pt")) + #removes ticks from y axis
    coord_flip()+
    guides(colour=guide_legend(reverse=TRUE),
           fill=guide_legend(reverse=TRUE)) +
    labs(title = paste0("Payrolled employees, change since February 2020 by selected sector"),
         subtitle = paste0("Latest data for ", paye_ind_last_month),
         caption = "\nSource: HM Revenues and Customs - Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live. Chart excludes sectors with fewer than 25,000 payrolled employees in London.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "paye_industries",w=8,h=8)

  #*****************************************************************************
  # Paye age ----
  #*****************************************************************************
  
  #Max change in London
  paye_age_max_perc_lon <- perc_form(paye_nuts1age_stats %>% ungroup() %>% filter(date_day == max(date_day) & area == "London" & !(age_group %in% c("Aged: 0-17"))) %>% filter(p_change_feb20 == max(p_change_feb20) ) %>% pull(p_change_feb20),d = digits_perc)
  
  paye_age_max_age_lon <- as.character(paye_nuts1age_stats %>% ungroup() %>% filter(date_day == max(date_day) & area == "London" & !(age_group %in% c("Aged: 0-17"))) %>% filter(p_change_feb20 == max(p_change_feb20) ) %>% pull(age_group))
  
  paye_age_max_l_perc_uk <- perc_form(paye_nuts1age_stats %>% ungroup() %>% filter(date_day == max(date_day) & area == "United Kingdom" & !(age_group %in% c("Aged: 0-17"))) %>% filter(age_group == paye_age_max_age_lon ) %>% pull(p_change_feb20),d = digits_perc)
  
  #Min change in London
  paye_age_min_perc_lon <- perc_form(paye_nuts1age_stats %>% ungroup() %>% filter(date_day == max(date_day) & area == "London" & !(age_group %in% c("Aged: 0-17"))) %>% filter(p_change_feb20 == min(p_change_feb20) ) %>% pull(p_change_feb20),d = digits_perc)
  
  paye_age_min_age_lon <- as.character(paye_nuts1age_stats %>% ungroup() %>% filter(date_day == max(date_day) & area == "London" & !(age_group %in% c("Aged: 0-17"))) %>% filter(p_change_feb20 == min(p_change_feb20) ) %>% pull(age_group))
  
  paye_age_min_l_perc_uk <- perc_form(paye_nuts1age_stats %>% ungroup() %>% filter(date_day == max(date_day) & area == "United Kingdom" & !(age_group %in% c("Aged: 0-17"))) %>% filter(age_group == paye_age_min_age_lon ) %>% pull(p_change_feb20),d = digits_perc)
  
  
  #Specifically the 50-64 group
  paye_age_5064_perc_lon <- perc_form(paye_nuts1age_stats %>% ungroup() %>% filter(date_day == max(date_day) & area == "London" & age_group %in% c("Aged: 50-64")) %>% filter(p_change_feb20 == max(p_change_feb20) ) %>% pull(p_change_feb20),d = digits_perc)
  
  paye_age_5064_perc_uk <- perc_form(paye_nuts1age_stats %>% ungroup() %>% filter(date_day == max(date_day) & area == "United Kingdom" & age_group %in% c("Aged: 50-64")) %>% filter(p_change_feb20 == max(p_change_feb20) ) %>% pull(p_change_feb20),d = digits_perc)
  
  ### Change in PAYE employees by age group since Feb2020
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  theme_set(theme_gla(gla_theme = "default"))
  
  
  paye_age_bars <- paye_nuts1age_stats %>%
    group_by(area) %>% 
    filter( (date_day == max(date_day)) & 
              !(age_group %in% c("Aged: 0-17"))
            & area %in% c("London", "United Kingdom")) %>%  
    mutate(chart_ranking = rank((age_group))) %>% 
    ungroup() %>% 
    ggplot(mapping = aes(x = reorder(age_group,chart_ranking), 
                         y = p_change_feb20, 
                         colour = area , fill=area,
                         text = paste(age_group, "\n",
                                      area, "\n",
                                      "Change: ", perc_form(p_change_feb20),"%", "\n",
                                      sep = ""))) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.4)+
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    theme_set(theme_gla(gla_theme = "default")) +
    scale_y_continuous(limits = c(-2, 9), labels = dollar_format(prefix = "", 
                                                                 suffix = "%", 
                                                                 largest_with_cents = 1)) +
    theme(axis.text.x = element_text( hjust=1, vjust=0.5)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title = "Payrolled employees, change since February 2020 by age group",
         subtitle = paste0("Latest data for ", paye_age_last_month),
         caption = "\nSource: HM Revenue and Customs - Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live.") +
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "paye_age_feb20")

  
  #*****************************************************************************
  # employment ----
  #*****************************************************************************
  emp_rate_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Level" & measures == 20207)  %>% pull(total_in_employment_aged_16_to_64),d = digits_perc)
  
  emp_pp_quart_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom"  & value_type_name == "Change on quarter" & measures == 20207)  %>% pull(total_in_employment_aged_16_to_64),d = digits_perc)
  
  emp_pp_year_uk <-perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom"  & value_type_name == "Change on year" & measures == 20207) %>% pull(total_in_employment_aged_16_to_64),d = digits_perc)
  

  ### The below should be the relevant chart
  employment_rate <- GLALineChart(data_set = lfs_stats, y_var = total_in_employment_aged_16_to_64, 
                                  title = "Employment rate (% of working age population)",
                                  caption = "\nSource: ONS Labour Force Survey.\n\nNote: The margin of error is +/- 1.3% for London and +/- 0.4% for the UK. \nMarch 2020 indicated by dotted line.",
                                  chart_name = "employment", y_limits = c(70,80), nudge_y = c(+0.4, -0.4)) 

  
  #*****************************************************************************
  # Unemp ----
  #*****************************************************************************
  
  unem_pp_quart_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Change on quarter" & measures == 20207) %>% pull(total_unemployed_aged_16_and_over),d = digits_perc)
  
  
  unem_pp_year_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Change on year" & measures == 20207)  %>% pull(total_unemployed_aged_16_and_over),d = digits_perc)
  


  unemployment_rate <- GLALineChart( data_set = lfs_stats, y_var = total_unemployed_aged_16_and_over, 
                                     title = "Unemployment rate (% of economically active)",
                                     caption = "\nSource: ONS Labour Force Survey.\n\nNote: The margin of error is +/- 0.8% for London and +/- 0.2% for the UK. \nMarch 2020 indicated by dotted line.",
                                     chart_name = "unemployment", y_limits = c(0,8), nudge_y = 0.4)

  #*****************************************************************************
  # Inactivity ----
  #*****************************************************************************
  
  inact_rate <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "London" & value_type_name == "Level" & measures == 20207)  %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_pp_quart <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "London" & value_type_name == "Change on quarter" & measures == 20207) %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_pp_year <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "London" & value_type_name == "Change on year" & measures == 20207)  %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_rate_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Level" & measures == 20207)  %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_pp_quart_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Change on quarter" & measures == 20207) %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_pp_year_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Change on year" & measures == 20207)  %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  

  econ_inactive <- GLALineChart( data_set = lfs_stats, y_var = total_economically_inactive_aged_16_to_64, 
                                 title =  "Economic inactivity (% of working age population)",
                                 caption = "\nSource: ONS Labour Force Survey.\n\nNote: The margin of error is not published for London, the UK margin is +/- 0.4%. \nMarch 2020 indicated by dotted line.",
                                 chart_name = "inact", y_limits = c(15,25), nudge_y = c(-0.5, +0.5))

  #*****************************************************************************
  # CC rate ----
  #*****************************************************************************
  
  
  claim_period <- claimant_count_stats_long %>%  filter(date_day == max(date_day)) %>% filter(row_number()==1) %>% mutate(date_day = format(date_day, '%B %Y'))  %>% pull(date_day)
  
  claim_level <- value_form(claimant_count_stats_long %>% filter( sex_name == "Total" & geography_name == "London" & date_day == max(date_day) & age_name == "All categories: Age 16+" & measure_name == "claimant_count") %>% pull(measure_value),s = signif_thous)
  
  claim_rate <- perc_form(claimant_count_stats_long %>% filter( sex_name == "Total" & geography_name == "London" & date_day == max(date_day) & age_name == "All categories: Age 16+" & measure_name == "claimants_as_a_proportion_of_residents_aged_16_64") %>% pull(measure_value),d = digits_perc)
  
  claim_d_year <- value_form(claimant_count_stats_long %>% filter( sex_name == "Total" & geography_name == "London" & date_day == max(date_day) & age_name == "All categories: Age 16+" & measure_name == "claimant_count") %>% pull(change_year),s = signif_thous)
  
  claim_p_year <- perc_form(claimant_count_stats_long %>% filter( sex_name == "Total" & geography_name == "London" & date_day == max(date_day) & age_name == "All categories: Age 16+" & measure_name == "claimant_count") %>% pull(change_year_percent),d = digits_perc)
  
  claim_d_year_uk <- value_form(claimant_count_stats_long %>% filter( sex_name == "Total" & geography_name == "United Kingdom" & date_day == max(date_day) & age_name == "All categories: Age 16+" & measure_name == "claimant_count") %>% pull(change_year),s = signif_thous)
  
  claim_p_year_uk <- perc_form(claimant_count_stats_long %>% filter( sex_name == "Total" & geography_name == "United Kingdom" & date_day == max(date_day) & age_name == "All categories: Age 16+" & measure_name == "claimant_count") %>% pull(change_year_percent),d = digits_perc)
  
  ### Create claimant count charts
  
  claimant_count_percent <- GLALineChart( data_set = claimant_count_stats_wide, y_var = claimants_as_a_proportion_of_residents_aged_16_64, x_var = date_day,
                                          suffix = "%", lfs_or_claims = "claims", title =  "Claimant count (as a % of residents aged 16 to 64)",
                                          caption = "\nSource: ONS Claimant Count by sex and age (NSA).\n\nNote: may include some employed claimants on low hours or earnings. \nMarch 2020 indicated by dotted line.",
                                          chart_name = "claimant_count_per", y_limits = c(0,10), nudge_y = 0.5)

  ### Manually plot total London claimants [IS NOT INCLUDED IN MARKDOWN]
  
  new_release_text <- claimant_count_stats_wide$date_name[claimant_count_stats_wide$date_day == max(claimant_count_stats_wide$date_day)]
  
  
  #*****************************************************************************
  # CC count ----
  #*****************************************************************************
  
  claimant_count_total <- claimant_count_stats_wide %>%
    filter( geography_name == "London" & sex_name == "Total" & !is.na(claimant_count) 
            & age_name == "All categories: Age 16+") %>% 
    ggplot(mapping = aes(x = date_day, y = claimant_count, 
                         colour = geography_name, group = geography_name)) +
    ggla_line(aes(size= geography_name)) +
    scale_size_manual(values = c(4 * mm_to_pt, 2 * mm_to_pt)) +
    scale_colour_manual(values = pal) +
    ggla_highlight(filter_type = "end") +
    ggla_highlight(mapping = aes(label = format(claimant_count, big.mark = ",")),
                   geom = GeomGLATextHighlight, filter_type = "end",  size = 4.5,
                   position = position_nudge(y = 20000),check_overlap = TRUE)+
    coord_cartesian(clip = 'off') +
    ggla_axisat0() +
    scale_y_continuous(expand = c(0, 0), labels = dollar_format(prefix = "", 
                                                                suffix = "", 
                                                                largest_with_cents = 1), 
                       limits = c(0,750000)) +
    scale_x_date( date_breaks = "2 months",
                  date_labels = "%b'%y",
                  expand = expansion( mult = c(0.1,0.02))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title =  "Claimant count in London",
         subtitle = paste0("Latest data for period ", new_release_text),
         caption = "\nSource: ONS Claimant Count by sex and age (NSA).\n\nNote: May include some employed claimants on low hours or earnings.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))

  save_GLA_plot(plot_name = "claimant_count")

  
  
  #*****************************************************************************
  # CC change year ----
  #*****************************************************************************
  
  ### Claimant count bar charts (edit variable names)
  year_ago_cc <- my(claim_period) - years(1)


  ### Increases in claimants by age
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  theme_set(theme_gla(gla_theme = "default"))
  
  claimant_count_bar <- claimant_count_stats_long %>%
    filter(measure_name=="claimant_count") %>% 
    group_by(sex_name, geography_name, date_day) %>% 
    mutate(chart_ranking = case_when( age_name == "All categories: Age 16+" ~ 1,
                                      TRUE ~ rank((age_name))+1)) %>% 
    ungroup() %>% 
    filter( sex_name == "Total" & (date_day == max(date_day)) & !(age_name %in% c("Aged 16-17", "Aged 18-21", "Aged 18-24", "Aged 50+" , "Age unknown (clerical claims)", "Aged 25-49", "Aged 65+"))) %>% 
    ggplot(mapping = aes(x = reorder(age_name,chart_ranking), 
                         y = change_year_percent, 
                         colour = geography_name , fill=geography_name,
                         text = paste(age_name, "\n",
                                      geography_name, "\n",
                                      "Change in count: ", perc_form(change_year_percent),"%", "\n",
                                      sep = ""))) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.4) +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    theme_set(theme_gla(gla_theme = "default")) +
    scale_y_continuous(limits = c(-50, 10), labels = dollar_format(prefix = "", 
                                                                   suffix = "%", 
                                                                   largest_with_cents = 1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
    theme(axis.text.x = element_text( hjust=1, vjust=0.5)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title = "Year-on-year percentage change in claimant count",
         subtitle = paste0("Latest data for period ", claim_period),
         caption = "\nSource: ONS Claimant Count by sex and age (NSA).\n\nNote: May include some employed claimants on low hours or earnings.") +
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "claimant_age_increase")
  
  #*****************************************************************************
  # WFJ count ----
  #*****************************************************************************

wfj_newmonth <- wfj_stats %>%  filter(date_day == max(date_day)) %>% filter(row_number()==1) %>% mutate(date_day = format(date_day, '%B %Y'))  %>% select(date_day)

wfj_lastmonth <- wfj_stats %>% filter(date_day == (ymd(max(date_day)) %m+% - months(3))) %>% filter(row_number()==1) %>% mutate(date_day = format(date_day, '%B %Y'))  %>% select(date_day)

wfj_level <- value_form(wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% mutate(obs_value=obs_value/1000000) %>% select(obs_value),s=signif_mil, d=digits_mil)


wfj_p_quart_uk <- perc_form(100*(wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == max(date_day) & geography_name == "United Kingdom" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value) - wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == (ymd(max(date_day)) %m+% - months(3)) & geography_name == "United Kingdom" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value))/(wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == (ymd(max(date_day)) %m+% - months(3)) & geography_name == "United Kingdom" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value)),d = digits_perc)


wfj_p_dec19_uk <- perc_form(100*(wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == max(date_day) & geography_name == "United Kingdom" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value) - wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == (ymd("2019-12-01")) & geography_name == "United Kingdom" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value))/(wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == (ymd("2019-12-01")) & geography_name == "United Kingdom" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value)),d = digits_perc)

wfj_emp_d_dec19 <- value_form(wfj_stats %>% filter(item_name == "employee jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value) - wfj_stats %>% filter(item_name == "employee jobs" & date_day == (ymd("2019-12-01")) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value),s = signif_thous)
  
wfj_emp_p_dec19 <- perc_form(100*(wfj_stats %>% filter(item_name == "employee jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value) - wfj_stats %>% filter(item_name == "employee jobs" & date_day == (ymd("2019-12-01")) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value))/(wfj_stats %>% filter(item_name == "employee jobs" & date_day == (ymd("2019-12-01")) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value)),d = digits_perc)

wfj_self_d_dec19 <- value_form(wfj_stats %>% filter(item_name == "self-employment jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value) - wfj_stats %>% filter(item_name == "self-employment jobs" & date_day == (ymd("2019-12-01")) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value),s = signif_thous)
  
wfj_self_p_dec19 <- perc_form(100*(wfj_stats %>% filter(item_name == "self-employment jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value) - wfj_stats %>% filter(item_name == "self-employment jobs" & date_day == (ymd("2019-12-01")) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value))/(wfj_stats %>% filter(item_name == "self-employment jobs" & date_day == (ymd("2019-12-01")) & geography_name == "London" & measures_name == "Value" & industry_name == "Total") %>% select(obs_value)),d = digits_perc)

  
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  theme_set(theme_gla(gla_theme = "default"))
  
  # Make the wfj stats wide to have employee and self-employee jobs in label.
  wfj_stats_wide <- wfj_stats %>% 
    filter( date_day >= "2010-03-01" & measures_name == "Value" & geography_name == "London" & industry_name == "Total" ) %>% 
    select(date_name,date_day,geography_name,industry_name,item_name,obs_value) %>% 
    pivot_wider(id_cols=c(date_name,date_day,geography_name,industry_name),names_from = item_name,values_from=obs_value) %>% 
    clean_names()
  
  wfj_time_chart <- wfj_stats_wide %>%
    ggplot(mapping = aes(x = date_day, y = total_workforce_jobs /1000,
                         colour = geography_name, group = geography_name,
                         text = paste( format(date_day,'%B %Y'), "\n",
                                       "Total jobs: ", value_form(total_workforce_jobs /1000,s=3),"k", "\n",
                                       "Employee jobs: ", value_form(employee_jobs /1000,s=3),"k", "\n",
                                       "Self-employed jobs: ", value_form(self_employment_jobs /1000,s=3),"k", "\n",
                                       sep = ""))) +
    ggla_line(aes(size= geography_name)) +
    scale_size_manual(values = c(2 * mm_to_pt, 1 * mm_to_pt)) +
    scale_colour_manual(values = pal) +
    ggla_highlight(filter_type = "end") +
    ggla_highlight(mapping = aes(label = paste0(format(total_workforce_jobs /1000, digits = 1, big.mark=","))),
                   geom = GeomGLATextHighlight, filter_type = "end",  size = 4.5,
                   position = position_nudge(y = c(200,0)))+
    geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
                 linetype = "dotted",
                 size = 1 * mm_to_pt,
                 colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start+
    coord_cartesian(clip = 'off') +
    scale_y_continuous(expand = c(0, 0), labels = dollar_format(prefix = "",
                                                                largest_with_cents = 1),
                       limits = c(4500,6500)) +
    scale_x_date( date_breaks = "18 months",
                  date_labels = "%b %Y",
                  expand = expansion( mult = c(0.05,0.05))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title =  "Total number of Workforce jobs in London",
         subtitle = paste0("Seasonally adjusted (thousands), latest data for ", wfj_newmonth),
         caption = "\nSource: ONS Workforce Jobs.\n\nNote: The margin of error for all jobs is +/- 0.9% for London and +/- 0.3% for the UK. \nMarch 2020 indicated by dotted line.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))

  save_GLA_plot(plot_name = "wfj_jobs")


  #*****************************************************************************
  # WFJ industries ----
  #*****************************************************************************
  
  
## Find industries with lowest and highest growth in recent data
  
  sector_thresh <- 25000
  
  lowest_growth <- wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & obs_value>=sector_thresh & !is.na(perc_change_dec19_within_date_rank)) %>% slice_max(perc_change_dec19_within_date_rank,n=3) 
  
  highest_growth <- wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & obs_value>=sector_thresh & !is.na(perc_change_dec19_within_date_rank)) %>% slice_min(perc_change_dec19_within_date_rank,n=3) 
  
  lowest_growth_q <- wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & obs_value>=sector_thresh & !is.na(perc_change_quart_within_date_rank)) %>% slice_max(perc_change_quart_within_date_rank,n=3) 
  
  highest_growth_q <- wfj_stats %>% filter(item_name == "total workforce jobs" & date_day == max(date_day) & geography_name == "London" & measures_name == "Value" & obs_value>=sector_thresh & !is.na(perc_change_quart_within_date_rank)) %>% slice_min(perc_change_quart_within_date_rank,n=3) 
  
  ## Find industry names and growth figures
  ## by year
  
  low_1_name <- lowest_growth %>% filter(row_number()==1) %>% select(industry_name_simple)
    
  low_1_d <- value_form(lowest_growth %>% filter(row_number()==1) %>% select(change_since_dec19),s = signif_thous)
    
  low_1_p <- perc_form(lowest_growth %>% filter(row_number()==1) %>% select(change_perc_since_dec19),d = digits_perc)
  
  low_2_name <- lowest_growth %>% filter(row_number()==2) %>% select(industry_name_simple)
    
  low_2_d <- value_form(lowest_growth %>% filter(row_number()==2) %>% select(change_since_dec19),s = signif_thous)
    
  low_2_p <- perc_form(lowest_growth %>% filter(row_number()==2) %>% select(change_perc_since_dec19),d = digits_perc)
  
  low_3_name <- lowest_growth %>% filter(row_number()==3) %>% select(industry_name_simple)
    
  low_3_d <- value_form(lowest_growth %>% filter(row_number()==3) %>% select(change_since_dec19),s = signif_thous)
    
  low_3_p <- perc_form(lowest_growth %>% filter(row_number()==3) %>% select(change_perc_since_dec19),d = digits_perc)
  
  
  high_1_name <- highest_growth %>% filter(row_number()==1) %>% select(industry_name_simple)
    
  high_1_d <- value_form(highest_growth %>% filter(row_number()==1) %>% select(change_since_dec19),s = signif_thous)
    
  high_1_p <- perc_form(highest_growth %>% filter(row_number()==1) %>% select(change_perc_since_dec19),d = digits_perc)
  
  high_2_name <- highest_growth %>% filter(row_number()==2) %>% select(industry_name_simple)
    
  high_2_d <- value_form(highest_growth %>% filter(row_number()==2) %>% select(change_since_dec19),s = signif_thous)
    
  high_2_p <- perc_form(highest_growth %>% filter(row_number()==2) %>% select(change_perc_since_dec19),d = digits_perc)
  
  high_3_name <- highest_growth %>% filter(row_number()==3) %>% select(industry_name_simple)
    
  high_3_d <- value_form(highest_growth %>% filter(row_number()==3) %>% select(change_since_dec19),s = signif_thous)
    
  high_3_p <- perc_form(highest_growth %>% filter(row_number()==3) %>% select(change_perc_since_dec19),d = digits_perc)

  
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  theme_set(theme_gla(gla_theme = "default"))
  
  # Select industries large in London, e.g. 20k+
  lon_large_inds <- wfj_stats %>%
    filter( item_name == "total workforce jobs" & date_day == max(date_day) & measures_name == "Value" & obs_value>=sector_thresh 
            & geography_name =="London") %>% 
    select(industry_name_simple)
  
  lon_large_inds <- unlist(lon_large_inds)
  
  wjf_industry_bar <- wfj_stats %>%
    filter( item_name == "total workforce jobs" & date_day == max(date_day) & measures_name == "Value" & industry_name_simple %in% lon_large_inds
            & geography_name %in% c("London", "United Kingdom")) %>% 
    group_by(geography_name) %>% #To ensure chart ranking is correct, make rank based on London and assign within sector
    mutate(chart_ranking = case_when( 
      industry_name_simple == "Total" & geography_name=="London" ~ 1,
      !(industry_name_simple == "Total") & geography_name=="London" ~ dense_rank(desc(change_perc_since_dec19))+1,
      TRUE ~ 99))  %>% 
    group_by(industry_name_simple) %>% 
    mutate(chart_ranking=min(chart_ranking)) %>% 
    ungroup() %>% 
    arrange(chart_ranking) %>% 
    ggplot(mapping = aes(x =  factor(reorder(industry_name_simple, -chart_ranking)), 
                         y = change_perc_since_dec19, 
                         colour = factor(reorder(geography_name,desc(geography_name))) ,#since horizontal bar reverses orders, we need to reverse too
                         fill=factor(reorder(geography_name,desc(geography_name))),
                         text = paste(industry_name_simple, "\n",
                                      geography_name, "\n",
                                      "Change: ", perc_form(change_perc_since_dec19),"%", "\n",
                                      sep = ""))) +
    geom_bar(stat = "identity", position = position_dodge(), width=0.5) +
    geom_hline(aes(yintercept=0), colour="gray45") +
    geom_vline(xintercept=c(length(lon_large_inds) - 0.5),
               colour="gray45",
               linetype = "dotted",
               size = 1 * mm_to_pt)+ #adds line below total
    scale_color_manual(values = rev(pal), aesthetics = "colour")+
    scale_fill_manual(values = rev(pal), aesthetics = "fill")+
    theme_set(theme_gla(gla_theme = "default", y_label_length=100)) + #GLA theme and removes lines below y-axis labels
    scale_y_continuous(limits = c(-18, 12), labels = dollar_format(prefix = "", 
                                                                   suffix = "%", 
                                                                   largest_with_cents = 1)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    scale_x_discrete(expand = c(0,0)) +
       theme(panel.grid.major.y = element_blank(), #removed y grid
        panel.grid.minor.y = element_blank(), # removes small y grid
        panel.grid.major.x = element_line( size=.5 ), # removes x grid
        axis.text.y = ggplot2::element_text( # ensures  y axis labels stay outside chart
              hjust = 0, vjust = 0.5,
              margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0,
                                       unit = "pt")),
        axis.ticks.length.y = ggplot2::unit(x = 0, units = "pt")) + #removes ticks from y axis
    coord_flip()+
    guides(colour=guide_legend(reverse=TRUE),
           fill=guide_legend(reverse=TRUE)) +
    labs(title = paste0("Percentage change in Workforce Jobs by industry group"),
         subtitle = paste0("Seasonally adjusted, between December 2019 and ", wfj_newmonth),
         caption = "\nSource: ONS Workforce Jobs.\n\nNote: The margin of error for all jobs is +/- 0.9% for London and +/- 0.3% for the UK.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "wfj_industries")
  
  # Real pay ----
  
  cpih_last_month_form <- format(cpih_last_month,"%B %Y")
  
  pay_v_cpih_data <- cpih_pay_stats %>%
    ungroup() %>%
    select(date_day,cpih_yoy,p_change_yoy,geography_name,pay_type) %>%
    mutate(cpih_yoy=-cpih_yoy) %>% #to model impact from inflation
    pivot_longer(cols=contains("yoy"),names_to = "measure_name",values_to="measure_value") %>%
    mutate(measure_desc=case_when(measure_name=="p_change_yoy" & pay_type=="nominal" ~ "Nominal pay",
                                  measure_name=="p_change_yoy" & pay_type=="real" ~ "Real pay",
                                  measure_name=="cpih_yoy" ~ "CPIH"),
           measure_rank=case_when(measure_name=="p_change_yoy"  ~ 1,
                                  measure_name=="cpih_yoy" ~ 2))
  
  measure_order <- c("Nominal pay","Real pay","CPIH") #Ensuring right order of stacked bars
  named_groups <- c("Nominal pay","Real pay","CPIH")
  pal <- gla_pal(palette_type = "highlight",n=c(2,1)) 
  pal_named <- setNames(object=pal,nm=named_groups)
  
  real_nom_chart <- pay_v_cpih_data %>%
    filter(date_day>="2020-01-01" & date_day<=cpih_last_month & geography_name=="London") %>% 
    arrange(date_day,pay_type,measure_rank) %>% 
    ggplot(aes())  +
    geom_bar(data=. %>% filter(pay_type=="nominal"),
             aes(x = date_day, y = measure_value ,
                 colour=factor(measure_desc,levels = measure_order),
                 fill=factor(measure_desc,levels = measure_order),
                 text = paste0(format(date_day,'%B %Y'), "\n",
                               measure_desc,", effect on real pay: ", perc_form(100*measure_value,d=1),"%")),
             stat='identity',
             position="stack",
             width = 10)  +
    ggla_line(data=. %>% filter(pay_type=="real" & measure_name=="p_change_yoy"),
              aes(x = date_day, y = measure_value,
                  colour=measure_desc,
                  fill=measure_desc),
              size=1 * mm_to_pt)  +
    geom_point(data=. %>% filter(pay_type=="real" & measure_name=="p_change_yoy"),
               aes(x = date_day, y = measure_value,
                   colour=measure_desc,
                   fill=measure_desc,
                   text = paste0(format(date_day,'%B %Y'), "\n",
                                 measure_desc,": ", perc_form(100*measure_value,d=1),"%")))+
    geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
               linetype = "dotted",
               size = 1 * mm_to_pt,
               colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
    scale_fill_manual(values=pal_named)+
    scale_colour_manual(values=pal_named)+
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(expand = c(0, 0), labels = percent_format(accuracy = 1),
                       limits=c(-.12,.15)) +
    scale_x_date( date_breaks = "1 year",
                  date_labels = "%b %Y",
                  expand = expansion( mult = c(0.05,0.05))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))+
    labs(title =  "Decomposition of real median pay in London, % annual change",
         subtitle = paste0("Effect from nominal pay change and CPIH inflation, to ",cpih_last_month_form))
         # caption = "\nSource: HM Revenue and Customs - Pay As You Earn Real Time Information, ONS.\n\nNote: March 2020 indicated by dotted line.\nInflation measure does not account for region-specific price changes. Sign of inflation rates \nhas been reversed (higher inflation rates are associated with lower real pay growth).")

  
  save_GLA_plot(plot_name = "real_nom_chart", w=9, h=6)
  
  # Real pay monthly change ----
  
  pay_v_cpih_data_mom <- cpih_pay_stats %>%
    ungroup() %>%
    select(date_day,cpih_mom,p_change_mom,geography_name,pay_type) %>%
    mutate(cpih_mom=-cpih_mom) %>% #to model impact from inflation
    pivot_longer(cols=contains("mom"),names_to = "measure_name",values_to="measure_value") %>%
    mutate(measure_desc=case_when(measure_name=="p_change_mom" & pay_type=="nominal" ~ "Nominal pay",
                                  measure_name=="p_change_mom" & pay_type=="real" ~ "Real pay",
                                  measure_name=="cpih_mom" ~ "CPIH"),
           measure_rank=case_when(measure_name=="p_change_mom"  ~ 1,
                                  measure_name=="cpih_mom" ~ 2))
  
  measure_order <- c("Nominal pay","Real pay","CPIH") #Ensuring right order of stacked bars
  named_groups <- c("Nominal pay","Real pay","CPIH")
  pal <- gla_pal(palette_type = "highlight",n=c(2,1)) 
  pal_named <- setNames(object=pal,nm=named_groups)
  
  real_nom_chart_mom <- pay_v_cpih_data_mom %>%
    filter(date_day>="2020-01-01" & date_day<=cpih_last_month & geography_name=="London") %>% 
    arrange(date_day,pay_type,measure_rank) %>% 
    ggplot(aes())  +
    geom_bar(data=. %>% filter(pay_type=="nominal"),
             aes(x = date_day, y = measure_value ,
                 colour=factor(measure_desc,levels = measure_order),
                 fill=factor(measure_desc,levels = measure_order),
                 text = paste0(format(date_day,'%B %Y'), "\n",
                               measure_desc,", effect on real pay: ", perc_form(100*measure_value,d=1),"%")),
             stat='identity',
             position="stack",
             width = 10)  +
    ggla_line(data=. %>% filter(pay_type=="real" & measure_name=="p_change_mom"),
              aes(x = date_day, y = measure_value,
                  colour=measure_desc,
                  fill=measure_desc),
              size=1 * mm_to_pt)  +
    geom_point(data=. %>% filter(pay_type=="real" & measure_name=="p_change_mom"),
               aes(x = date_day, y = measure_value,
                   colour=measure_desc,
                   fill=measure_desc,
                   text = paste0(format(date_day,'%B %Y'), "\n",
                                 measure_desc,": ", perc_form(100*measure_value,d=1),"%")))+
    geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
               linetype = "dotted",
               size = 1 * mm_to_pt,
               colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
    scale_fill_manual(values=pal_named)+
    scale_colour_manual(values=pal_named)+
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(expand = c(0, 0), labels = percent_format(accuracy = 1),
                       limits=c(-.05,.05)) +
    scale_x_date( date_breaks = "1 year",
                  date_labels = "%b %Y",
                  expand = expansion( mult = c(0.05,0.05))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))+
    labs(title =  "Decomposition of real median pay in London, % monthly change",
         subtitle = paste0("Effect from nominal pay change and CPIH inflation, to ",cpih_last_month_form),
         caption = "\nSource: HM Revenue and Customs - Pay As You Earn Real Time Information, ONS.\n\nNote: March 2020 indicated by dotted line.\nInflation measure does not account for region-specific price changes. Sign of inflation rates \nhas been reversed (higher inflation rates are associated with lower real pay growth).")
  
  
  save_GLA_plot(plot_name = "real_nom_chart_mom", w=11, h=6)
  
  # Real pay annualised ----
  
  pay_v_cpih_data_annualised <- cpih_pay_stats %>%
    ungroup() %>%
    select(date_day,cpih_annualised_3m,p_annualised_3m,geography_name,pay_type) %>%
    pivot_longer(cols=contains("annualised"),names_to = "measure_name",values_to="measure_value") %>%
    mutate(measure_desc=case_when(measure_name=="p_annualised_3m" & pay_type=="nominal" ~ "Nominal pay",
                                  measure_name=="p_annualised_3m" & pay_type=="real" ~ "Real pay",
                                  measure_name=="cpih_annualised_3m" ~ "CPIH"),
           measure_rank=case_when(measure_name=="p_annualised_3m"  ~ 1,
                                  measure_name=="cpih_annualised_3m" ~ 2))
  
  measure_order <- c("Real pay","Nominal pay","CPIH") #Ensuring right order of stacked bars
  named_groups <- c("Real pay","CPIH","Nominal pay")
  pal <- gla_pal(palette_type = "highlight",n=c(2,1)) 
  pal_named <- setNames(object=pal,nm=named_groups)
  
  real_nom_chart_annualised <- pay_v_cpih_data_annualised %>%
    filter(date_day>="2020-01-01" & date_day<=cpih_last_month & geography_name=="London") %>% 
    arrange(date_day,pay_type,measure_rank) %>% 
    ggplot(aes())  +
    geom_hline(aes(yintercept=0), colour="gray45") +
    geom_bar(data=. %>% filter(measure_name=="p_annualised_3m"),
             aes(x = date_day, y = measure_value ,
                 colour=factor(measure_desc,levels = measure_order),
                 fill=factor(measure_desc,levels = measure_order)),
             stat='identity',
             position="dodge",
             width = 10)  +
    ggla_line(data=. %>% filter(pay_type=="nominal" & measure_desc=="CPIH"),
              aes(x = date_day, y = measure_value,
                  colour=measure_desc,
                  fill=measure_desc),
              size=1 * mm_to_pt)  +
    geom_point(data=. %>% filter(pay_type=="nominal" & measure_desc=="CPIH"),
               aes(x = date_day, y = measure_value,
                   colour=measure_desc,
                   fill=measure_desc,
                   text = paste0(format(date_day,'%B %Y'), "\n",
                                 measure_desc,": ", perc_form(100*measure_value,d=1),"%")))+
    geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
               linetype = "dotted",
               size = 1 * mm_to_pt,
               colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
    scale_fill_manual(values=pal_named)+
    scale_colour_manual(values=pal_named)+
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(expand = c(0, 0), labels = percent_format(accuracy = 1),
                       limits=c(-.15,.25)) +
    scale_x_date( date_breaks = "1 year",
                  date_labels = "%b %Y",
                  expand = expansion( mult = c(0.05,0.05))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title =  "Median pay in London and CPIH",
         subtitle = paste0("Rolling 3-month average annualised % change, to ",cpih_last_month_form),
         caption = "\nSource: HM Revenue and Customs - Pay As You Earn Real Time Information, ONS.\n\nNote: March 2020 indicated by dotted line.\nInflation measure does not account for region-specific price changes.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "real_nom_chart_annualised", w=11, h=6)
    