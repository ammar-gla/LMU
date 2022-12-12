  
  #*****************************************************************************
  ### SET UP ----
  #*****************************************************************************

  # Set specific folder for ad hoc output
  IMAGE_FOLDER <- paste0(here::here(),"/OUTPUT/IMAGES/AD_HOC/")
  
  # Version of GLA theme for charting
  gla_theme_type <- "default"
  theme_set(theme_gla(gla_theme = gla_theme_type))
    
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

  paye_newmonth <- paye_stats %>% filter( date_day == max(date_day)) %>% mutate(date_day=format(date_day,'%B %Y')) %>%  select(date_day)
  
  paye_lastmonth <- paye_stats  %>% mutate(lag_month = lag(date_day, n = 1)) %>% mutate(lag_month2 = format(lag_month, '%B')) %>% filter( date_day == max(date_day)) %>%  select(lag_month2)
  
  # LFS
  
  lfs_newmonth <- lfs_stats %>% mutate(month = format(date_day, '%B %Y')) %>% filter(sex_name == "Total"  & geography_name == "London" & value_type_name == "Level" & date_day == max(date_day) & measures == 20207) %>%  select(month)
  
  lfs_lastmonth <- lfs_stats  %>% mutate(lag_month = ymd(date_day) %m+% - months(2)) %>% mutate(lag_month2 = format(lag_month, '%B')) %>% filter(sex_name == "Total"  & geography_name == "London" & value_type_name == "Level" & date_day == max(date_day) & measures == 20207) %>%  select(lag_month2)
  
  
  # WFJ
  
  wfj_year <-  wfj_stats %>% filter(date_day == max(date_day)) %>% filter(row_number()==1) %>% mutate(year=year(date_day)) %>% select(year)
  
  wfj_newquarter <- wfj_stats %>% ungroup()  %>% filter(date_day == max(date_day)) %>% filter(row_number()==1) %>% select(quarter_text) 
  
  wfj_oldquarter <-  wfj_stats %>% filter(date_day == (ymd(max(date_day)) %m+% - months(3))) %>% filter(row_number()==1) %>% select(quarter_text)

  
  # CPIH
  cpih_last_month_form <- format(cpih_last_month,"%B %Y")
  
  
  # Months since Feb 2020 used to calculate change in pay
  cpih_months_since_feb20 <- abs(interval(cpih_last_month, "2020-02-01") %/% months(1))
  
  #*****************************************************************************
  # Paye main ----
  #*****************************************************************************

  # Plotting process:
  ## First set the relevant colour palette and them
  ## Produce the ggplot and save it on the shared drive
  ## Produce the interactive ggplotly chart for the markdown.
  
  pal <- gla_pal(gla_theme = "default", palette_type = "categorical", n = 1)
  
  paye_emps_line <- paye_stats %>%
    filter( date_day >= "2016-01-01" & geography_name=="London" & measure_name=="emps")%>%
    ggplot(mapping = aes(x = date_day, y = measure_value,
                         colour = geography_name, group = geography_name,
                         text = paste0(
                           geography_name, "\n",
                           format(date_day,'%B %Y'), "\n",
                           "Level: ", value_form(measure_value,s=4), "\n"))) +
    ggla_line(aes(size= geography_name)) +
    geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
               linetype = "dotted",
               size = 1 * mm_to_pt,
               colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
    scale_size_manual(values = 2 * mm_to_pt) +
    scale_colour_manual(values = pal) +
    ggla_highlight(filter_type = "end") +
    # ggla_highlight(mapping = aes(label = paste0(value_form(measure_value))),
    #                geom = GeomGLATextHighlight, filter_type = "end",  size = 4.5,
    #                position = position_nudge(y = c(+0.4,+0.4)))+
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(expand = c(0, 0), labels = comma_format(),
                       limits=c(3.7e6,4.3e6)) +
    scale_x_date( date_breaks = "1 year",
                  date_labels = "%b %Y",
                  expand = expansion( mult = c(0.05,0.05))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title=element_text(vjust=3),
          plot.subtitle = element_text(vjust=3))+
    labs(title =  "Payrolled employees in London",
         subtitle = paste0("Latest data for ", paye_overall_last_month),
         caption = "\nSource: HM Revenue and Customs – Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live. March 2020 indicated by dotted line.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)),
          legend.position = "none")
  
  save_GLA_plot(plot_name = "paye_emps_line",h=6,w=8)

  #*****************************************************************************
  # Paye change feb20 ----
  #*****************************************************************************
  # Palette: five subregions, one London, one UK
  pal <- c(rep(gla_pal(gla_theme = "default", palette_type = "categorical", n = 1),5),
           gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1)))
  pal_text <- c(rep("white",6),"black")
  
  paye_emps_nuts2_feb20 <- paye_nuts2_stats %>%
    filter(date_day == max(date_day) & measure_name=="emps") %>%
    ungroup() %>% 
    mutate(chart_ranking = case_when( geography_name == "UK" ~ 1,
                                      geography_name == "London" ~ 2,
                                      TRUE ~ dense_rank(desc(p_change_feb20))+2)) %>% 
    ggplot(mapping = aes(x =  reorder(geography_name, -chart_ranking))) +
    geom_bar(aes(y = p_change_feb20,
                 group = geography_name,
                 fill=geography_name,
                 text = paste0(
                   geography_name, "\n",
                   "Change: ", perc_form(100*p_change_feb20),"%")), 
             stat="identity",
             width=0.5)+
    geom_text(aes(
      color=geography_name,
      label=paste0(format(round(100*p_change_feb20,1), digits = 2), "%"),
      y = p_change_feb20-0.0013*((p_change_feb20)/abs(p_change_feb20))))+ #cannot use vjust with ggplotly
    scale_color_manual(values = pal_text, aesthetics = "colour") +
    scale_fill_manual(values = pal, aesthetics =  "fill") +
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(labels = percent_format(accuracy=1),
                       limits = c(0,0.05)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.text=element_text(size=12))+
    labs(title =  "Payrolled employees, change since February 2020 by NUTS2 region",
         subtitle = paste0("Latest data for ", paye_overall_last_month),
         caption = "\nSource: HM Revenue and Customs – Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live.") + 
    theme(legend.position = "none",
          plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "paye_emps_nuts2_feb20")

  #*****************************************************************************
  # Paye change year ----
  #*****************************************************************************
  
  # Palette: five subregions, one London, one UK
  pal <- c(rep(gla_pal(gla_theme = "default", palette_type = "categorical", n = 1),5),
           gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1)))
  
  pal_text <- c(rep("white",6),"black")
  
  paye_emps_nuts2_yoy <- paye_nuts2_stats %>%
    filter(date_day == max(date_day) & measure_name=="emps") %>%
    ungroup() %>% 
    mutate(chart_ranking = case_when( geography_name == "UK" ~ 1,
                                      geography_name == "London" ~ 2,
                                      TRUE ~ dense_rank(desc(p_change_yoy))+2)) %>% 
    ggplot(mapping = aes(x =  reorder(geography_name, -chart_ranking))) +
    geom_text(aes(label=paste0(format(round(100*p_change_yoy,1), digits = 2), "%"), 
                  y = p_change_yoy-0.08*min(p_change_yoy),
                  color = geography_name))+ #cannot use vjust with ggplotly
    geom_bar(aes(y = p_change_yoy,
                 group = geography_name,
                 fill=geography_name,
                 text = paste0(
                   geography_name, "\n",
                   "Change: ", perc_form(100*p_change_yoy),"%", "\n")),
             stat="identity",
             width=0.5)+
    scale_color_manual(values = pal_text, aesthetics = "colour") +
    scale_fill_manual(values = pal, aesthetics = "fill") +
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(labels = percent_format(accuracy=1),
                       limits = c(0,0.06)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    
    theme(plot.margin = unit(c(1,1,1,1), "cm"),axis.text=element_text(size=12))+
    labs(title =  "Payrolled employees, change on previous year by NUTS2 region",
         subtitle = paste0("Latest data for ", paye_overall_last_month),
         caption = "\nSource: HM Revenue and Customs – Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live.") + 
    theme(legend.position = "none") +
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "paye_emps_nuts2_yoy")

  #*****************************************************************************
  # Paye pay trend ----
  #*****************************************************************************
  
  named_groups <- c("London, nominal","UK, nominal")
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  pal_named <- setNames(object=pal,nm=named_groups)
  size_named <- setNames(c(2 * mm_to_pt, 1 * mm_to_pt),named_groups)
  
  pay_trend_chart <- cpih_pay_stats %>%
    filter(date_day>="2015-01-01" & geography_pay_type %in% named_groups) %>% 
    ggplot(mapping = aes(x = date_day, y = median_pay,
                         colour = geography_pay_type,
                         size = geography_pay_type,
                         group = geography_pay_type,
                         text = paste0(
                           geography_name, "\n",
                           format(date_day,'%B %Y'), "\n",
                           "Median monthly pay: £", value_form(median_pay,s=4)))) +
    ggla_line(aes()) +
    geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
               linetype = "dotted",
               size = 1 * mm_to_pt,
               colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start
    scale_colour_manual(values = pal_named) +
    scale_size_manual(values = size_named) +
    ggla_highlight(filter_type = "end") +
    # ggla_highlight(mapping = aes(label = paste0(value_form(median_pay,s=4))),
    #                geom = GeomGLATextHighlight, filter_type = "end",  size = 4.5,
    #                position = position_nudge(y = c(+0.4,+0.4)))+
    coord_cartesian(clip = 'off') +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_y_continuous(expand = c(0, 0), labels = comma_format(prefix = "£"),
                       limits=c(1600,2600)) +
    scale_x_date( date_breaks = "1 year",
                  date_labels = "%b %Y",
                  expand = expansion( mult = c(0.05,0.05))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title =  "Median monthly employee pay",
         subtitle = paste0("Nominal pay to ",paye_overall_last_month," not adjusted for inflation"),
         caption = "\nSource: HM Revenue and Customs – Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live. March 2020 indicated by dotted line.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "pay_trend_chart",w=12,h=5)


  #*****************************************************************************
  # Paye cpih ----
  #*****************************************************************************

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
    labs(title =  "Decomposition of real median pay in London, % annual change",
         subtitle = paste0("Effect from nominal pay change and CPIH inflation, to ",cpih_last_month_form),
         caption = "\nSource: HM Revenue and Customs – Pay As You Earn Real Time Information, ONS.\n\nNote: March 2020 indicated by dotted line.\nInflation measure does not account for region-specific price changes. Sign of inflation rates has been \nreversed (higher inflation rates are associated with lower real pay growth).")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "real_nom_chart",w=12,h=6)

  #*****************************************************************************
  # Paye industry ----
  #*****************************************************************************
  
  # Find industries with lowest and highest growth in recent data
  
  paye_sector_thresh <- 25000
  
 
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  
  # Select industries large in London, here 25k
  lon_large_inds_paye <- paye_nuts1ind_stats %>%
    filter( measure_name=="emps" & measure_value >=paye_sector_thresh 
            & geography_name =="London" & date_day == max(date_day)) %>% 
    pull(industry_name_simple)
  
  paye_emps_ind_feb20 <- paye_nuts1ind_stats %>%
    filter(date_day == max(date_day) & industry_name_simple %in% lon_large_inds_paye & measure_name=="emps") %>% 
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
                         text = paste0(industry_name_simple, "\n",
                                      geography_name, "\n",
                                      "Change: ", perc_form(100*p_change_feb20),"%", "\n"))) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_color_manual(values = rev(pal), aesthetics = "colour")+
    scale_fill_manual(values = rev(pal), aesthetics = "fill")+
    theme_set(theme_gla(gla_theme = "default", y_label_length=100)) + #GLA theme and removes lines below y-axis labels
    scale_y_continuous(limits = c(-0.05, 0.1), labels = percent_format(accuracy=1)) +
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
         caption = "\nSource: HM Revenues and Customs – Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live. Chart excludes sectors with fewer than 25,000 payrolled employees in London.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "paye_emps_ind_feb20")

  #*****************************************************************************
  # Paye age ----
  #*****************************************************************************
  
  ### Change in PAYE employees by age group since Feb2020
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  
  
  paye_emps_age_feb20 <- paye_nuts1age_stats %>%
    filter((date_day == max(date_day)) & !(age_group %in% c("Aged: 0-17"))
            & geography_name %in% c("London", "UK") & measure_name=="emps") %>% 
    group_by(geography_name) %>%  
    mutate(chart_ranking = rank((age_group))) %>% 
    ungroup() %>% 
    ggplot(mapping = aes(x = reorder(age_group,chart_ranking), 
                         y = p_change_feb20, 
                         colour = geography_name , fill=geography_name,
                         text = paste0(age_group, "\n",
                                      geography_name, "\n",
                                      "Change: ", perc_form(100*p_change_feb20),"%", "\n"))) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.4)+
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    theme_set(theme_gla(gla_theme = "default")) +
    scale_y_continuous(limits = c(-0.02, 0.09), labels = percent_format(accuracy=1)) +
    theme(axis.text.x = element_text( hjust=1, vjust=0.5)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title = "Payrolled employees, change since February 2020 by age group",
         subtitle = paste0("Latest data for ", paye_age_last_month),
         caption = "\nSource: HM Revenue and Customs – Pay As You Earn Real Time Information.\n\nNote: Estimates are based on where employees live.") +
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "paye_emps_age_feb20")

  
  #*****************************************************************************
  # employment ----
  #*****************************************************************************
  emp_rate_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Level" & measures == 20207)  %>% pull(total_in_employment_aged_16_to_64),d = digits_perc)
  
  emp_pp_quart_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom"  & value_type_name == "Change on quarter" & measures == 20207)  %>% pull(total_in_employment_aged_16_to_64),d = digits_perc)
  
  emp_pp_year_uk <-perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom"  & value_type_name == "Change on year" & measures == 20207) %>% pull(total_in_employment_aged_16_to_64),d = digits_perc)
  

  # Tweaked date label to fit into narrow image
 employment_rate <- LFS_line_chart(data_set = lfs_stats, y_var = total_in_employment_aged_16_to_64, 
                           title = "Employment rate \n(% of working age population)",
                           caption = "\nSource: ONS Labour Force Survey.\n\nNote: The margin of error is +/- 1.6% for London and +/- 0.4% for the UK. \nMarch 2020 indicated by dotted line.",
                           chart_name = "employment", y_limits = c(70,80), nudge_y = c(-0.5, +0.5),
                           date_label="%Y") 

  
  #*****************************************************************************
  # Unemp ----
  #*****************************************************************************
  
  unem_pp_quart_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Change on quarter" & measures == 20207) %>% pull(total_unemployed_aged_16_and_over),d = digits_perc)
  
  
  unem_pp_year_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Change on year" & measures == 20207)  %>% pull(total_unemployed_aged_16_and_over),d = digits_perc)
  


  unemployment_rate <- LFS_line_chart( data_set = lfs_stats, y_var = total_unemployed_aged_16_and_over, 
                               title = "Unemployment rate \n(% of economically active)",
                               caption = "\nSource: ONS Labour Force Survey.\n\nNote: The margin of error is +/- 0.8% for London and +/- 0.2% for the UK. \nMarch 2020 indicated by dotted line.",
                               chart_name = "unemployment", y_limits = c(0,8), nudge_y = c(-0.2,-0.2),
                               date_label="%Y")

  #*****************************************************************************
  # Inactivity ----
  #*****************************************************************************
  
  inact_rate <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "London" & value_type_name == "Level" & measures == 20207)  %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_pp_quart <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "London" & value_type_name == "Change on quarter" & measures == 20207) %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_pp_year <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "London" & value_type_name == "Change on year" & measures == 20207)  %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_rate_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Level" & measures == 20207)  %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_pp_quart_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Change on quarter" & measures == 20207) %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  
  inact_pp_year_uk <- perc_form(lfs_stats %>% filter(sex_name == "Total" & date_day == max(date_day) & geography_name == "United Kingdom" & value_type_name == "Change on year" & measures == 20207)  %>% pull(total_economically_inactive_aged_16_to_64),d= digits_perc)
  

  econ_inactive <- LFS_line_chart( data_set = lfs_stats, y_var = total_economically_inactive_aged_16_to_64, 
                              title =  "Economic inactivity \n(% of working age population)",
                               caption = "\nSource: ONS Labour Force Survey.\n\nNote: The margin of error is not published for London, the UK margin is +/- 0.5%. \nMarch 2020 indicated by dotted line.",
                              chart_name = "inact", y_limits = c(15,25), nudge_y = c(+0.1, +0.6),
                              date_label="%Y")

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
  
  claimant_count_percent <- claimant_count_stats_sa %>%
  LFS_line_chart(y_var = proportion_of_resident_population_aged_16_64_estimates, x_var = date_day,
                                          suffix = "%", lfs_or_claims = "claims", title =  "Claimant count (as a % of residents aged 16 to 64)",
                                          caption = "\nSource: ONS Claimant Count by sex and age (seasonally adjusted).\n\nNote: may include some employed claimants on low hours or earnings. \nMarch 2020 indicated by dotted line.",
                                          chart_name = "claimant_count_per", y_limits = c(0,10), nudge_y = 0.5)
  
  
  #*****************************************************************************
  # CC change year ----
  #*****************************************************************************
  
  ### Increases in claimants by age (Not seasonally adjusted)
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  
  claimant_count_bar <- claimant_count_stats_long %>%
    filter(measure_name=="Claimant count") %>% 
    group_by(sex_name, geography_name, date_day) %>% 
    mutate(chart_ranking = case_when( age_name == "All categories: Age 16+" ~ 1,
                                      TRUE ~ rank((age_name))+1)) %>% 
    ungroup() %>% 
    filter( sex_name == "Total" & (date_day == max(date_day)) & !(age_name %in% c("Aged 16-17", "Aged 18-21", "Aged 18-24", "Aged 50+" , "Age unknown (clerical claims)", "Aged 25-49", "Aged 65+"))) %>% 
    ggplot(mapping = aes(x = reorder(age_name,chart_ranking), 
                         y = change_year_percent, 
                         colour = geography_name , fill=geography_name,
                         text = paste0(age_name, "\n",
                                      geography_name, "\n",
                                      "Change in count: ", perc_form(change_year_percent),"%", "\n"))) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.4) +
    geom_hline(aes(yintercept=0), colour="gray45") +
    scale_colour_manual(values = pal) +
    scale_fill_manual(values = pal) +
    theme_set(theme_gla(gla_theme = "default")) +
    scale_y_continuous(limits = c(-60,5), labels = dollar_format(prefix = "", 
                                                                   suffix = "%", 
                                                                   largest_with_cents = 1)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
    theme(axis.text.x = element_text( hjust=1, vjust=0.5)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"))+
    labs(title = "Year-on-year percentage change in claimant count",
         subtitle = paste0("Latest data for period ", claim_period),
         caption = "\nSource: ONS Claimant Count by sex and age (not seasonally adjusted).\n\nNote: May include some employed claimants on low hours or earnings.") +
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "claimant_age_increase")
  
  #*****************************************************************************
  # WFJ count ----
  #*****************************************************************************
  
  wfj_latest_month <- format(max(wfj_stats$date_day),'%B %Y')
  
  wfj_previous_month <- format(max(wfj_stats$date_day)- months(3),'%B %Y')
  
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  
  # Make the wfj stats wide to have employee and self-employee jobs in label.
  wfj_stats_wide <- wfj_stats %>% 
    filter( date_day >= "2010-03-01" & measures_name == "Value" & geography_name == "London" & industry_name == "Total" ) %>% 
    select(date_month,date_day,geography_name,industry_name,item_name,obs_value) %>% 
    pivot_wider(id_cols=c(date_month,date_day,geography_name,industry_name),names_from = item_name,values_from=obs_value) %>% 
    clean_names()
  
  wfj_time_chart <- wfj_stats_wide %>%
    ggplot(mapping = aes(x = date_day, y = total_workforce_jobs,
                         colour = geography_name, group = geography_name,
                         text = paste0( format(date_day,'%B %Y'), "\n",
                                        "Total jobs: ", value_form(total_workforce_jobs,s=3),"k", "\n",
                                        "Employee jobs: ", value_form(employee_jobs,s=3),"k", "\n",
                                        "Self-employed jobs: ", value_form(self_employment_jobs,s=3),"k", "\n"))) +
    ggla_line(aes(size= geography_name)) +
    scale_size_manual(values = c(2 * mm_to_pt, 1 * mm_to_pt)) +
    scale_colour_manual(values = pal) +
    ggla_highlight(filter_type = "end") +
    # ggla_highlight(mapping = aes(label = paste0(format(total_workforce_jobs /1000, digits = 1, big.mark=","))),
    #                geom = GeomGLATextHighlight, filter_type = "end",  size = 4.5,
    #                position = position_nudge(y = c(200,0)))+
    geom_vline(aes(xintercept = as.numeric(ymd("2020-03-01"))),
               linetype = "dotted",
               size = 1 * mm_to_pt,
               colour = rgb(166,166,166,maxColorValue = 255)) + # mark lockdowns start+
    coord_cartesian(clip = 'off') +
    scale_y_continuous(expand = c(0, 0), labels = comma_format(),
                       limits = c(4.5e6,6.5e6)) +
    scale_x_date( date_breaks = "1 year",
                  date_labels = "%Y",
                  expand = expansion( mult = c(0.05,0.05))) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
          plot.title=element_text(vjust=3),
          plot.subtitle = element_text(vjust=3),
          legend.position = "none")+
    labs(title =  "Total number of workforce jobs in London",
         subtitle = paste0("Seasonally adjusted, latest data for ", wfj_latest_month),
         caption = "\nSource: ONS Workforce Jobs.\n\nNote: The margin of error for all jobs is +/- 0.9% for London and +/- 0.3% for the UK. \nMarch 2020 indicated by dotted line.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "wfj_jobs",h=6,w=8)

  #*****************************************************************************
  # WFJ industries ----
  #*****************************************************************************
  
  
## Find industries with lowest and highest growth in recent data
  
  wfj_sector_thresh <- 25000
  
  pal <- gla_pal(gla_theme = "default", palette_type = "highlight", n = c(1, 1))
  
  # Select industries large in London, e.g. 20k+
  lon_large_inds <- wfj_stats %>%
    filter( item_name == "total workforce jobs" & date_day == max(date_day) & measures_name == "Value" & obs_value>=wfj_sector_thresh 
            & geography_name =="London") %>% 
    select(industry_name_simple)
  
  lon_large_inds <- unlist(lon_large_inds)
  
  wjf_industry_bar <- wfj_stats %>%
    filter( item_name == "total workforce jobs" & date_day == max(date_day) & measures_name == "Value" &
              industry_name_simple %in% lon_large_inds & geography_name %in% c("London", "United Kingdom")) %>% 
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
                         text = paste0(industry_name_simple, "\n",
                                      geography_name, "\n",
                                      "Change: ", perc_form(change_perc_since_dec19),"%", "\n"))) +
    geom_bar(stat = "identity", position = position_dodge(), width=0.5) +
    geom_hline(aes(yintercept=0), colour="gray45") +
    geom_vline(xintercept=c(length(lon_large_inds) - 0.5),
               colour="gray45",
               linetype = "dotted",
               size = 1 * mm_to_pt)+ #adds line below total
    scale_color_manual(values = rev(pal), aesthetics = "colour")+
    scale_fill_manual(values = rev(pal), aesthetics = "fill")+
    theme_set(theme_gla(gla_theme = "default", y_label_length=100)) + #GLA theme and removes lines below y-axis labels
    scale_y_continuous(limits = c(-14, 12), labels = dollar_format(prefix = "", 
                                                                   suffix = "%", 
                                                                   largest_with_cents = 1)) +
    theme(plot.margin = unit(c(1,1,1,1), "cm"),
          legend.position = "none")+
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
    labs(title = paste0("Percentage change in workforce jobs by industry group"),
         subtitle = paste0("Seasonally adjusted, between December 2019 and ", wfj_latest_month),
         caption = "\nSource: ONS Workforce Jobs.\n\nNote: The margin of error for all jobs is +/- 0.9% for London and +/- 0.3% for the UK.")+
    theme(plot.caption = element_text(color = rgb(166,166,166,maxColorValue = 255)))
  
  save_GLA_plot(plot_name = "wfj_industries")
  