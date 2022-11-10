#_____________________________________________________________________________
### Downloads and data processing ----
#_____________________________________________________________________________


# Download datasets only if they do not already exist:
## labour market data is released on a single date each month, see object 'release_dates'
## if the release date has been passed, check if newest data exists.
## if it does not exist, download it. Otherwise, use the existing dataset.

#.............................................................................
#### Download LFS stats, automatically clean and save ----
#.............................................................................

lfs_stats <- HeadlineDownload(time_period = c("2010-01","latest"),
                              geography = c(london_geo_code,uk_geo_code,boroughs_group),
                              econ_activity=c(0,3,7,9), #main ones used for charts
                              sex=7,
                              data_name = "maindat",
                              force_download = redownload_all)

lfs_regions <- HeadlineDownload(time_period = c("latest","latest"), # Only need most recent changes
                                geography = c(london_geo_code,regions_geo_code,eng_geo_code,uk_geo_code,boroughs_group),
                                econ_activity=NULL, #should have all variables for use in table
                                sex=7,
                                data_name = "headline",
                                force_download = redownload_all)

#.............................................................................
#### Download CC stats, automatically clean and save ----
#............................................................................. 

claimant_count_stats_long <- ClaimantCountDownload( sa_nsa = "nsa", 
                                                    time_period = c("2015-01","latest"),
                                                    geography_v = c(2013265927,2092957697), 
                                                    save_intermediate = TRUE,
                                                    data_name = "summary")  %>% 
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
                                                  save_intermediate = TRUE,
                                                  data_name = "headline")  %>%
  pivot_wider(names_from = "measures_name", values_from = "measure_value") %>%
  clean_names() %>%
  rename(level=value) %>%
  group_by(geography_name,sex_name) %>%
  mutate(d_change_level_yoy = level - lag(level , n = 12),
         p_change_level_yoy = 100 * (level / lag(level , n = 12) - 1)) %>%
  ungroup()

#.............................................................................
#### Geographical data ----
#.............................................................................

# Read in data and transform to long-lat format geometry
paye_la_stats_geo <- read_sf(here("INPUT","statistical-gis-boundaries-london","ESRI","London_Borough_Excluding_MHW.shp")) %>% 
  clean_names() %>% 
  select(name,geometry) %>% 
  rename(geography_name=name) %>%  
  left_join(paye_la_stats,by="geography_name") %>% 
  mutate(tooltip= paste0("Change: ", perc_form(p_change_feb20),"%", "\n")) %>% 
  filter(date_day==max(date_day)) %>% 
  st_transform(4326)
