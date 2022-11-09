#_______________________________________________________________________________
# LOAD AND PROCESS ALL CPIH DATA ----
#_______________________________________________________________________________

#.............................................................................
### Check PAYE data to be loaded first ----
#.............................................................................

if(!exists("paye_pay_stats")) 
  stop("Load PAYE pay data before running this code!")


#.............................................................................
### Download inflation data ----
#.............................................................................

# Download datasets, or load them, within a list
cpih_raw_data_list <- cpih_download(force_download = redownload_all)

#.............................................................................
### Process inflation data ----
#.............................................................................

# Load raw datasets and clean up
cpih_rate <- cpih_raw_data_list[["cpih_rate"]] %>% 
  rename(month_date=V1,cpih_yoy=V2) %>% 
  filter(grepl("\\d{4} \\w{3}",month_date))

# Merge rate and index data
cpih_stats <- cpih_raw_data_list[["cpih_index"]] %>% 
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

remove(cpih_rate)

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

#.............................................................................
### Combine with PAYE pay data ----
#.............................................................................

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
