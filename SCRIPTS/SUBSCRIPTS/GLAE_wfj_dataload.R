#_______________________________________________________________________________
# LOAD AND PROCESS WORKFORCE JOBS DATA ----
#_______________________________________________________________________________

#.............................................................................
### Download Workforce Jobs data ----
#.............................................................................

# Download, or load, dataset
wfj_stats_raw <- wfj_download(force_download = redownload_all)


#.............................................................................
### Process data and output ----
#.............................................................................

wfj_stats <- wfj_stats_raw %>% 
  clean_names() %>% 
  select(c("date","date_name","geography","geography_name","industry","industry_code","industry_name","item","item_name","measures","measures_name","obs_value","record_count")) %>% 
  rename(date_month=date)  %>% 
  mutate(date_day = dmy(paste0("01",date_name)),
         quarter =  paste0("Q",quarter(date_day)),
         quarter_text = case_when(quarter=="Q1" ~ "first",
                                  quarter=="Q2" ~ "second",
                                  quarter=="Q3" ~ "third",
                                  quarter=="Q4" ~ "fourth"),
         industry_name = case_when(industry_name == "Total" ~ "Total",
                                   TRUE ~  str_sub(industry_name,5,-1)),
         industry_name_simple = case_when(
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
           TRUE ~ industry_name))%>% 
  group_by(industry_name, geography_name, item_name,measures_name) %>% #grouping makes calculating lag values easier
  mutate( 
    change_perc_since_dec19 = case_when( date_day <= as.Date("2019-12-01") | measures_name == "Percent" ~ NaN,
                                         date_day > as.Date("2019-12-01") ~ 100*(obs_value - obs_value[ date_day == "2019-12-01"])/(obs_value[ date_day == "2019-12-01"])),
    change_since_dec19 = case_when( date_day <= as.Date("2019-12-01") | measures_name == "Percent" ~ NaN,
                                    date_day > as.Date("2019-12-01") ~ (obs_value - obs_value[ date_day == "2019-12-01"])),
    change_perc_quar = case_when(  measures_name == "Percent" ~ NaN,
                                   TRUE ~ 100*(obs_value - lag(obs_value, n = 1))/(lag(obs_value, n = 1))),
    change_quar = case_when(  measures_name == "Percent" ~ NaN,
                              TRUE ~ (obs_value - lag(obs_value, n = 1)))) %>% 
  group_by( geography_name,date_day,item_name) %>% # To get ranks of industries within date-geography groups
  mutate(change_p_dec19_within_date_rank = case_when( industry_name == "Total" ~ NA_integer_, 
                                                      TRUE  ~ dense_rank(desc(change_perc_since_dec19))), 
         change_p_quart_within_date_rank = case_when( industry_name == "Total" ~ NA_integer_, 
                                                      TRUE  ~ dense_rank(desc(change_perc_quar)))) %>% 
  ungroup()%>%  #ungroup to ease future manipulations
  arrange(date_day,geography,item,measures,industry)


# Helper dataset to reduce number of filters used in markdown
wfj_tot_value_stats <- wfj_stats %>% 
  filter(measures_name == "Value" & industry_name == "Total")


