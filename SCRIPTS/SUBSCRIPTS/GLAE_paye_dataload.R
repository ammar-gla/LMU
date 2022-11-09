#_______________________________________________________________________________
# LOAD AND PROCESS ALL PAYE DATA ----
#_______________________________________________________________________________

# New method (Nov-2022) for downloading and loading PAYE data
# Check if we have passed data release date referenced in list
#- If date is passed:
#-- download the latest PAYE data from the current month
#-- depending on the month, designate the appropriate prior dataset for detailed data
#-If date is not passed:
#-- download previous month's PAYE data
#-- designate the appropriate prior datasets for detailed data

#...............................................................................
# Determine latest data ----
#...............................................................................

# Run function to retrieve list of relevant PAYE datasets
paye_data_list <- paye_rti_download(force_download=redownload_all)

#...............................................................................
# Generate overall latest data ----
#...............................................................................


paye_emp_stats <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["overall"]), sheet = "7. Employees (NUTS1)", skip = 6) %>% 
  mutate(date_day=my(Date),
         measure_name = "emps") %>% 
  select(date_day, London, UK,measure_name) 

paye_pay_stats <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["overall"]), sheet = "8. Median pay (NUTS1)", skip = 6) %>% 
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
paye_nuts2_emp_stats <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["overall"]), sheet = "11. Employees (NUTS2)", skip = 6) %>% 
  mutate( date_day = dmy(paste0("01",Date)),
          measure_name="emps")   %>% 
  select(date_day,measure_name, "Inner London - West",	"Inner London - East",	"Outer London - East and North East",	"Outer London - South",	"Outer London - West and North West")

paye_nuts2_pay_stats <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["overall"]), sheet = "12. Median pay (NUTS2)", skip = 6) %>% 
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

#...............................................................................
# PAYE monthly data ----
#...............................................................................


# PAYE data has unique quarterly data on following months:
## - (a) By Age*NUTS1: 1,4,7,10
## - (b) By Industry*NUTS1: 2,5,8,11
## - (c) By LA: 3,6,9,12

#...............................................................................
#### PAYE by age ----
#### (a) Each quarter, process data on PAYE employee age on regional level


paye_age_emp_uk <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["nuts1age"]), sheet = "28. Employees (Age)", skip = 5) %>%
  mutate(date_day = dmy(paste0("01",Date)),
         measure_name = "emps",
         geography_name = "UK",
         Total = rowSums(across(c("0-17","18-24","25-34","35-49","50-64","65+")))) %>% 
  select(date_day,geography_name, measure_name, "0-17","18-24","25-34","35-49","50-64","65+",Total) 

paye_age_pay_uk <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["nuts1age"]), sheet = "29. Median pay (Age)", skip = 5) %>%
  mutate(date_day = dmy(paste0("01",Date)),
         measure_name = "median_pay",
         geography_name = "UK") %>% 
  left_join(paye_pay_aux,by=c("date_day","geography_name","measure_name")) %>% #merge with total average median pay
  select(date_day,geography_name, measure_name, "0-17","18-24","25-34","35-49","50-64","65+",Total) 

paye_nuts1age_emp <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["nuts1age"]), sheet = "32. Employees (NUTS1Age)", skip = 6) %>%
  rename_with(~str_replace(.x,pattern="London: ",replacement="")) %>% 
  mutate(date_day = dmy(paste0("01",Date)),
         measure_name = "emps",
         geography_name = "London",
         Total = rowSums(across(c("0-17","18-24","25-34","35-49","50-64","65+")))) %>% 
  select(date_day,geography_name, measure_name, "0-17","18-24","25-34","35-49","50-64","65+",Total)

paye_nuts1age_pay <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["nuts1age"]), sheet = "33. Median pay (NUTS1Age)", skip = 6) %>%
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


#...............................................................................
#### PAYE by industry ----
#### (b) Each quarter, process data on PAYE employees by industry at region level. 

paye_ind_emp_uk <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["nuts1ind"]), sheet = "23. Employees (Industry)", skip = 6) %>% 
  mutate(date_day = dmy(paste0("01",Date)),
         geography_name="UK",
         measure_name = "emps",
         Total = rowSums(across(c("Agriculture, forestry and fishing":"Households and Extraterritorial")))) %>% 
  select(-c("UK"))

paye_ind_pay_uk <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["nuts1ind"]), sheet = "24. Median pay (Industry)", skip = 6) %>% 
  mutate(date_day = dmy(paste0("01",Date)),
         geography_name="UK",
         measure_name = "median_pay") %>% 
  left_join(paye_pay_aux,by=c("date_day","geography_name","measure_name")) %>% #merge with total average median pay
  select(-c("UK"))  


paye_nuts1ind_emp <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["nuts1ind"]), sheet = "36. Employees (NUTS1Sector)", skip = 6) %>% 
  mutate(date_day = dmy(paste0("01",Date)),
         geography_name="London",
         measure_name = "emps",
         Total = rowSums(across(c("London: Agriculture, forestry and fishing":"London: Households and Extraterritorial")))) %>% 
  select(c(Date,date_day,geography_name,contains("London"),measure_name,Total))  %>%
  rename_with(~str_replace(.x,pattern="London: ",replacement=""))

paye_nuts1ind_pay <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["nuts1ind"]), sheet = "37. Median pay (NUTS1Sector)", skip = 6) %>% 
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

#...............................................................................
#### PAYE by LA ----
#### (c) Each quarter, process data on PAYE at borough level and merge with geographical codes. 

la_columns <- c("City of London", "Barking and Dagenham", "Barnet", "Bexley", "Brent", "Bromley", "Camden", "Croydon", "Ealing", "Enfield", "Greenwich", "Hackney", "Hammersmith and Fulham", "Haringey", "Harrow", "Havering", "Hillingdon", "Hounslow", "Islington", "Kensington and Chelsea", "Kingston upon Thames", "Lambeth", "Lewisham", "Merton", "Newham", "Redbridge", "Richmond upon Thames", "Southwark", "Sutton", "Tower Hamlets", "Waltham Forest", "Wandsworth", "Westminster")

paye_la_emp_stats <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["locauth"]), sheet = "19. Employees (LA)", skip = 7) %>% 
  mutate(date_day = dmy(paste0("01",Date))) %>% 
  select( Date, date_day, all_of(la_columns)) %>% 
  pivot_longer(cols="City of London":"Westminster",names_to = "geography_name", values_to = "measure_value") %>% 
  mutate(measure_name="emps")

paye_la_pay_stats <- readxl::read_excel(path = paste0(OTHERDATA,paye_data_list["locauth"]), sheet = "20. Median pay (LA)", skip = 7) %>% 
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
