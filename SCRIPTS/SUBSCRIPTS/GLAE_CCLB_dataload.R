
#_______________________________________________________________________________
# LOAD AND PROCESS CCLB DATA ----
#_______________________________________________________________________________


#.............................................................................
#### London sub-regional mapping ----
#.............................................................................

lon_subregion_map <-readxl::read_excel(path = here("INPUT","Sub_regional mapping.xlsx"), sheet = "Mapping") %>% clean_names() %>% rename(geography_name=london_borough)

#.............................................................................
#### Claimant count data
#.............................................................................

cc_stats_detailed_raw <- ClaimantCountDownload(sa_nsa="nsa", #use NSA throughout
                                               time_period = c("2020-01","latest"),
                                               save_intermediate = TRUE,
                                               geography=c(london_geo_code,uk_geo_code,boroughs_group),
                                               measures_v = 20100,
                                               force_download=redownload_all,
                                               data_name="detail") %>% 
  mutate(measure_name=snakecase::to_snake_case(measure_name),  #since it is easier to reference
         dataset_name="detailed") #to easily separate regions appearing twice

# Load regions data separately as not as much detail is needed
cc_stats_region_raw <- ClaimantCountDownload(sa_nsa="nsa",
                                             time_period = c("2019-01","latest"),
                                             save_intermediate = TRUE,
                                             geography = c(london_geo_code,regions_geo_code),
                                             measures_v = 20100,
                                             force_download=redownload_all,
                                             data_name = "region",
                                             sex=0,
                                             age=0) %>% 
  mutate(measure_name=snakecase::to_snake_case(measure_name), #since it is easier to reference
         dataset_name="region") #to easily separate regions appearing twice

# ----------- Create subregional data  -------------

cc_stats_subregion <- cc_stats_detailed_raw %>% 
  filter(!(geography_name %in% c("London", "United Kingdom"))) %>% 
  merge(lon_subregion_map,by="geography_name",all=TRUE)   %>%   #merge with sub-regions  
  pivot_wider(id_cols=c(geography_name:subregional_partnership,dataset_name),names_from = "measure_name",values_from="measure_value") %>% 
  mutate(population_res_16_64 = claimant_count/(claimants_as_a_proportion_of_residents_aged_16_64/100),
         population_res_active = claimant_count/(claimants_as_a_proportion_of_economically_active_residents_aged_16/100)) %>% 
  select(-c(contains("as_a_proportion"),"geography_name"))   %>%  
  group_by(date_day,date_name,sex_name,age_name,subregional_partnership,dataset_name) %>% 
  summarise(across(c(claimant_count,contains("population")),sum)) %>%   #now grouping at subregional level
  rename(geography_name = subregional_partnership) %>% 
  mutate(claimants_as_a_proportion_of_residents_aged_16_64 = 100 * claimant_count / population_res_16_64,
         claimants_as_a_proportion_of_economically_active_residents_aged_16 = 100 * claimant_count / population_res_active,
         claimants_as_a_proportion_of_the_workforce= NA) %>% #So that it has same columns as main dataset
  select(-c(contains("population"))) %>% # No need for it anymore
  pivot_longer(cols = contains("claimant"), names_to = "measure_name", values_to = "measure_value",values_drop_na = TRUE ) %>% 
  ungroup()

subregions_vector <- unique(na.omit(lon_subregion_map$subregional_partnership)) #extract subregions to make separate tables

# ----------- Create data for charts and tables  -------------

cc_changes_stats <- cc_stats_detailed_raw %>% 
  rbind(cc_stats_subregion,cc_stats_region_raw) %>% #Add in rows with subregional geographies
  arrange(date_day,geography_name,sex_name,age_name,measure_name,dataset_name) %>%
  group_by(geography_name, measure_name,sex_name,age_name,dataset_name) %>% 
  mutate(d_change_since_mar20 = case_when(date_day <= as.Date("2020-03-01") ~ NaN,
                                          date_day > as.Date("2020-03-01") ~ measure_value - measure_value[ date_day == "2020-03-01"]),
         p_change_since_mar20 = case_when(date_day <= as.Date("2020-03-01") ~ NaN,
                                          date_day > as.Date("2020-03-01") ~ 100*(measure_value - measure_value[ date_day == "2020-03-01"])/measure_value[ date_day == "2020-03-01"]),
         yoy_change = case_when(date_day - min(date_day) < 365 ~ NaN,
                                date_day - min(date_day) > 365 ~ 
                                  (measure_value - lag(measure_value, n = 12))),
         yoy_percentage_change = case_when(date_day - min(date_day) < 365 ~ NaN,
                                           date_day - min(date_day) > 365 ~ 
                                             100*(measure_value - lag(measure_value, n = 1))/lag(measure_value, n = 12)),
         mom_d_change =   (measure_value - lag(measure_value, n = 1)),
         mom_p_change = 100*(measure_value - lag(measure_value, n = 1))/lag(measure_value, n = 1),
         is_la = case_when(geography_name %in% c("London","United Kingdom",subregions_vector) | dataset_name=="region" ~ 0,
                           TRUE ~ 1)) %>%  
  ungroup()

# ----------- Create boroughs data for maps -------------

la_cc_map_data <- cc_changes_stats %>% 
  filter(is_la==1 & date_day==max(date_day) & sex_name=="Total" & age_name=="All categories: Age 16+") %>% 
  merge(London_LA_codes,by='geography_name') %>% 
  arrange(geography_name,date_day)

# ------------------ Separate frame for overall London timeseries------------- 

lon_cc_data <- cc_changes_stats   %>%
  filter(geography_name=="London" & dataset_name=="region" & age_name=="All categories: Age 16+" & sex_name=="Total")
