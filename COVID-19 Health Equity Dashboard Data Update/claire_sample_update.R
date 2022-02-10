

###################     SETTING WORKING DIRECTORY   ####################################

#get your working directory
getwd()

box1 = "/Users/air/OneDrive - Emory University/COVID19_data_shared" 
local = "/Users/air/Downloads"
onedrive="/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard"
path_c19dashboard_shared_folder="/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard"
#R.Version()


#######################################Install Packages############################################
# Package names
packages <- c("base","tidyverse", "tidyr", "plyr", "readr", "ggplot2", "tibble", "lubridate","cdlTools","openintro","gtools","rvest","dplyr","DescTools","readxl","stringr","naniar","zoo","mice","anytime")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

rm(packages,installed_packages)


#get nation, state and county level data from nytimes
nytimes_national_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv"
nytimes_states_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
nytimes_counties_url <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"


nyt_national <- read_csv(nytimes_national_url)%>% 
  dplyr::mutate(nation = 1)
nyt_states <- read_csv(nytimes_states_url) %>% 
  dplyr::mutate(state = fips %>% as.numeric(.)) %>% 
  mutate(fips = as.numeric(fips))
nyt_counties <- read_csv(nytimes_counties_url) %>% 
  dplyr::mutate(state = substr(fips,1,2) %>% as.numeric(.),
                county = substr(fips,3,5) %>% as.numeric(.)) %>% 
  mutate(fips = as.numeric(fips))

# Save RAW ----------
national_last_date = max(nyt_national$date)
write.csv(nyt_national,paste0(path_c19dashboard_shared_folder,"/Data/Raw/NYT Covid19 data/nytimes us_",national_last_date,".csv"),row.names = FALSE)

states_last_date = max(nyt_states$date)
write.csv(nyt_states,paste0(path_c19dashboard_shared_folder,"/Data/Raw/NYT Covid19 data/nytimes states_",states_last_date,".csv"),row.names = FALSE)

counties_last_date = max(nyt_counties$date)
write.csv(nyt_counties,paste0(path_c19dashboard_shared_folder,"/Data/Raw/NYT Covid19 data/nytimes counties_",counties_last_date,".csv"),row.names = FALSE)

# covid_all_orig <- read_csv(paste0(path_c19dashboard_old_folder,"/covid_all.csv"))

covid_all <- bind_rows(
  nyt_national ,
  nyt_states,
  nyt_counties 
  
) %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::filter(date == Sys.Date())

# daily cases data -----
library(zoo)

county_daily <- nyt_counties %>% 
  arrange(fips,date) %>% 
  group_by(state,county) %>% 
  mutate(
    dailydeaths = case_when(date == min(date) ~ deaths,
                            deaths < dplyr::lag(deaths) ~ 0,
                            TRUE ~ deaths - dplyr::lag(deaths)),
    dailycases = case_when(date == min(date) ~ cases,
                           cases < dplyr::lag(cases) ~ 0,
                           TRUE ~ cases - dplyr::lag(cases))
  ) %>% 
  ungroup() %>% 
  arrange(fips,date) %>% 
  group_by(state,county) %>% 
  mutate(mean7daydeaths = rollapply(deaths,7,mean,partial=TRUE,align="r"),
         mean7daycases = rollapply(cases,7,mean,partial=TRUE,align="r"),
  ) %>% 
  mutate(percent14dayCases = case_when(is.na(dplyr::lag(cases,14)) ~ NA_real_,
                                       TRUE ~ 100*(cases/dplyr::lag(cases,14) - 1)),
         percent14dayDailyCases = case_when(is.na(dplyr::lag(mean7daycases,14)) ~ NA_real_,
                                            TRUE ~ 100*(mean7daycases/dplyr::lag(mean7daycases,14) - 1)),
         
         percent14dayDeaths = case_when(is.na(dplyr::lag(deaths,14)) ~ NA_real_,
                                        TRUE ~ 100*(deaths/dplyr::lag(deaths,14) - 1)),
         percent14dayDailyDeaths = case_when(is.na(dplyr::lag(mean7daydeaths,14)) ~ NA_real_,
                                             TRUE ~ 100*(mean7daydeaths/dplyr::lag(mean7daydeaths,14) - 1))) %>% 
  ungroup()


# states_daily -----------

state_daily <- nyt_states %>% 
  arrange(fips,date) %>% 
  group_by(state) %>% 
  mutate(
    dailydeaths = case_when(date == min(date) ~ deaths,
                            deaths < dplyr::lag(deaths) ~ 0,
                            TRUE ~ deaths - dplyr::lag(deaths)),
    dailycases = case_when(date == min(date) ~ cases,
                           cases < dplyr::lag(cases) ~ 0,
                           TRUE ~ cases - dplyr::lag(cases))
  ) %>% 
  ungroup() %>% 
  arrange(fips,date) %>% 
  group_by(state) %>% 
  mutate(mean7daydeaths = rollapply(deaths,7,mean,partial=TRUE,align="r"),
         mean7daycases = rollapply(cases,7,mean,partial=TRUE,align="r"),
  ) %>% 
  mutate(percent14dayCases = case_when(is.na(dplyr::lag(cases,14)) ~ NA_real_,
                                       TRUE ~ 100*(cases/dplyr::lag(cases,14) - 1)),
         percent14dayDailyCases = case_when(is.na(dplyr::lag(mean7daycases,14)) ~ NA_real_,
                                            TRUE ~ 100*(mean7daycases/dplyr::lag(mean7daycases,14) - 1)),
         
         percent14dayDeaths = case_when(is.na(dplyr::lag(deaths,14)) ~ NA_real_,
                                        TRUE ~ 100*(deaths/dplyr::lag(deaths,14) - 1)),
         percent14dayDailyDeaths = case_when(is.na(dplyr::lag(mean7daydeaths,14)) ~ NA_real_,
                                             TRUE ~ 100*(mean7daydeaths/dplyr::lag(mean7daydeaths,14) - 1))) %>% 
  ungroup()

# us_daily -----------

us_daily <- nyt_national %>% 
  arrange(date) %>% 
  mutate(
    dailydeaths = case_when(date == min(date) ~ deaths,
                            deaths < dplyr::lag(deaths) ~ 0,
                            TRUE ~ deaths - dplyr::lag(deaths)),
    dailycases = case_when(date == min(date) ~ cases,
                           cases < dplyr::lag(cases) ~ 0,
                           TRUE ~ cases - dplyr::lag(cases))
  ) %>% 
  arrange(date) %>% 
  mutate(mean7daydeaths = rollapply(deaths,7,mean,partial=TRUE,align="r"),
         mean7daycases = rollapply(cases,7,mean,partial=TRUE,align="r"),
  ) %>% 
  mutate(percent14dayCases = case_when(is.na(dplyr::lag(cases,14)) ~ NA_real_,
                                       TRUE ~ 100*(cases/dplyr::lag(cases,14) - 1)),
         percent14dayDailyCases = case_when(is.na(dplyr::lag(mean7daycases,14)) ~ NA_real_,
                                            TRUE ~ 100*(mean7daycases/dplyr::lag(mean7daycases,14) - 1)),
         
         percent14dayDeaths = case_when(is.na(dplyr::lag(deaths,14)) ~ NA_real_,
                                        TRUE ~ 100*(deaths/dplyr::lag(deaths,14) - 1)),
         percent14dayDailyDeaths = case_when(is.na(dplyr::lag(mean7daydeaths,14)) ~ NA_real_,
                                             TRUE ~ 100*(mean7daydeaths/dplyr::lag(mean7daydeaths,14) - 1)))

# mean7day_current ----------

mean7day_current <- bind_rows(
  us_daily,
  state_daily,
  county_daily
)  %>% 
  dplyr::filter(date == Sys.Date()-1) %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::select(date,state,county,nation,
                dailydeaths,mean7daydeaths,
                dailycases,mean7daycases
  ) %>%
  
  # CHECK - What is this for? =============
mutate(zero = 0)

# covidtimeseries -----------
covidtimeseries <- bind_rows(
  us_daily,
  state_daily,
  county_daily
) %>% 
  dplyr::filter(date >= "2020-04-01") %>% 
  dplyr::filter(!state %in% c(66, 69, 72, 78)) %>% 
  dplyr::select(date,state,county,nation,
                deaths,cases,
                dailydeaths,mean7daydeaths,
                dailycases,mean7daycases
  )


# urbancodes3 ------------
urbancodes3 <- readxl::read_excel(paste0(path_c19dashboard_shared_folder,"/Data/Raw/CDC_Urban_Rural/Urban_Rural_2013_Classification.xlsx"),sheet="SAS") %>% 
  dplyr::rename(Urbanization_2013 = '2013 Urbanization',
                Urbanization_Code_2013 = '2013 Urbanization Code',
                county_code = 'County Code',
                county_name = 'County',
                deaths = 'Deaths',
                population = 'Population',
                crude_rate = 'Crude Rate') %>% 
  mutate(fips = sprintf("%05d",county_code),
         annualdeaths2018 = deaths,
         annualmortality2018 = crude_rate
  )%>% 
  dplyr::mutate(state = substr(fips,1,2) %>% as.numeric(.),
                county = substr(fips,3,5) %>% as.numeric(.)) %>% 
  mutate(fips = as.numeric(fips)) %>% 
  mutate(urbanrural = paste0(Urbanization_Code_2013,str_replace_all(Urbanization_2013," ",""))) %>% 
  mutate_at(vars(deaths,population,crude_rate),~as.numeric(.))




statepopulation3 <- read.csv(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Census 2019 Data/PopulationData_Census2019.csv")) %>% 
  dplyr::select(STATE,COUNTY,POPESTIMATE2019,DEATHS2019) %>% 
  dplyr::filter(COUNTY == 0) %>% 
  mutate(COUNTY = NA_real_,
         crude_rate = DEATHS2019/POPESTIMATE2019) %>% 
  dplyr::rename(state = STATE,
                county = COUNTY,
                deaths = DEATHS2019,
                population = POPESTIMATE2019)



# nationalpop2 -----------
nationalpop2 <- statepopulation3 %>% 
  summarize_at(vars(deaths,population),~sum(.,na.rm = TRUE)) %>% 
  mutate(state = NA,
         county = NA,
         nation = 1)

# population_all -----------

population_all <- bind_rows(nationalpop2,
                            statepopulation3,
                            urbancodes3) %>% 
  dplyr::rename(deaths_allcause = deaths) %>% 
  dplyr::select(nation,state,county,deaths_allcause,population)



population_all <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC_Urban_Rural/population_all.RDS"))
# covid_pop -------------
attach(covid_all)
attach(population_all)
covid_pop <- covid_all %>% 
  dplyr::filter(!is.na(nation)|!is.na(state)|!is.na(county)) %>% 
  full_join(population_all,
            by = c("nation","state","county")) %>% 
  full_join(mean7day_current %>% 
              dplyr::select(nation,state,county,
                            dailycases,dailydeaths,
                            mean7daycases,mean7daydeaths),
            by=c("nation","state","county")) %>% 
  mutate(
    covidmortality = (deaths/population)*(10^5),
    caserate = (cases/population)*(10^5),
    
    covidmortality7day = (mean7daydeaths/population)*(10^5),
    caserate7day = (mean7daycases/population)*(10^5)
    
  ) %>% 
  mutate_at(vars(cases,deaths,
                covidmortality,caserate,
                covidmortality7day,caserate7day),
         .funs=list(fig=~case_when(. == 0 | is.na(.) | . == Inf ~ -1,
                                             TRUE ~ .)),
         .names = "{fn}_{col}")

# data checking covid_pop --------

covid_pop %>% 
  dplyr::select(-date,-nation,-state,-fips,-county) %>% 
  summarize_all(.funs=function(x) mean(x %in% c(0,NA))) %>% 
  View()

# covidtimeseries_pop ---------------
covidtimeseries_pop <- covidtimeseries %>% 
  dplyr::filter(!is.na(nation)|!is.na(state)|!is.na(county)) %>% 
  left_join(population_all,
            by=c("nation","county","state")
) %>% 
  ungroup() %>% 
  mutate(
    covidmortality = (deaths/population)*(10^5),
    caserate = (cases/population)*(10^5),
    cfr = 100*(deaths/cases),
    covidmortality7day = (mean7daydeaths/population)*(10^5),
    caserate7day = (mean7daycases/population)*(10^5)
    
  ) %>% 
  mutate_at(vars(cases,deaths,cfr,
                 covidmortality,caserate,
                 covidmortality7day,caserate7day),
            .funs=list(fig=~case_when(. == 0 | is.na(.) | . == Inf ~ -1,
                                      TRUE ~ .)),
            .names = "{fn}_{col}") %>% 
  dplyr::filter(state != 0 | !(is.na(nation)&is.na(state)&is.na(county)))

#merge with hospitalization data
setwd(onedrive)
final_hosptest_ts <- read.csv("./Data/Processed/Hospitalizations and testing/series_hosptest_cleaned.csv")
our_ts <- covidtimeseries_pop
max(our_ts$date)
final_hosptest_ts$date<-anydate(final_hosptest_ts$date)
final_hosptest_ts<-final_hosptest_ts[,-c(6,8,9,10,11)]
final_hosptest_ts[,9]<-""
names(final_hosptest_ts)[9]<-"percent7dayhospDaily"
final_hosptest_ts<-final_hosptest_ts[
  with(final_hosptest_ts, order(statename, date)),
]
final_hosptest_ts$hospDaily[which(final_hosptest_ts$hospDaily  == -1)] = NA
final_hosptest_ts$percent7dayhospDaily=100*(final_hosptest_ts$hospDaily-lag(final_hosptest_ts$hospDaily,7))/lag(final_hosptest_ts$hospDaily,14)
variable<-c("percent7dayhospDaily")
final_hosptest_ts[, variable][is.na(final_hosptest_ts[, variable])] <- -999
final_hosptest_ts[, variable][is.infinite(final_hosptest_ts[, variable])] <- -999
final_hosptest_ts[, variable][is.nan(final_hosptest_ts[, variable])] <- -999
which(is.na(final_hosptest_ts$percent7dayhospDaily))
which(is.infinite(final_hosptest_ts$percent7dayhospDaily))
which(is.nan(final_hosptest_ts$percent7dayhospDaily))
variable1<-c("hospDaily")
final_hosptest_ts[, variable1][is.na(final_hosptest_ts[, variable1])] <- -1
final_hosptest_ts$hospAdmissionper100beds<-""

#merge with community profile report
df <- list.files(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports"))
df <- anydate(df)
df <- max(df,na.rm = T)
df

states_daterange_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/",df,"/states_date_range_clean.RDS"))
states_df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/",df,"/states_df_clean.RDS"))
counties_daterange_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/",df,"/counties_date_range_clean.RDS"))
counties_df_clean <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/Community Profile Reports/",df,"/counties_df_clean.RDS"))

# STATES: Example for indicators which belong to same header : "VIRAL (RT-PCR) LAB TESTING: LAST WEEK"

states_example_df <- states_df_clean %>% 
  dplyr::select(file_name,date_of_file,
                S01,S02,state,county,
                S21,S22,S52,S53,S62,S23
  ) %>% 
  left_join(states_daterange_clean %>% 
              dplyr::select(file_name),
            by = "file_name")



counties_example_df <-counties_df_clean%>%
  dplyr::select(file_name,date_of_file,
                V01,V02,state,county,
                V33,V34,V56,V57,V66,V35
  ) %>% 
  left_join(counties_daterange_clean %>% 
              dplyr::select(file_name),
            by = "file_name")


states_example_df<-states_example_df[,-c(1,4,6)]
states_example_df$S62<-states_example_df$S62*100
names(states_example_df)[1] <- "date"
names(states_example_df)[2] <- "statename"
names(states_example_df)[4] <- "percentPositive"
names(states_example_df)[5] <- "totaltests"
names(states_example_df)[6] <- "hospDaily"
names(states_example_df)[7] <- "percent7dayhospDaily"
names(states_example_df)[8] <- "hospAdmissionper100beds"
names(states_example_df)[9] <- "positivePer100K"

states_example_df<-subset(states_example_df,!statename%in%"Guam"& !statename%in%"United States Virgin Islands"& !statename%in%"Commonwealth of the Northern Mariana Islands"& !statename%in%"American Samoa" & !statename%in%"Puerto Rico")
states_example_df$date<-anydate(states_example_df$date)
states_example_df<-states_example_df%>%
  filter(date>="2021-03-08")
states_example_df<-states_example_df[
  with(states_example_df, order(statename, date)),
]
states_example_df$percentPositive<-states_example_df$percentPositive*100

counties_example_df<-counties_example_df[,-c(1,4)]
counties_example_df$V66<-counties_example_df$V66*100
counties_example_df$V33<-counties_example_df$V33*100
names(counties_example_df)[1] <- "date"
names(counties_example_df)[2] <- "countyname"
names(counties_example_df)[5] <- "percentPositive"
names(counties_example_df)[6] <- "totaltests"
names(counties_example_df)[7] <- "hospDaily"
names(counties_example_df)[8] <- "hospAdmissionper100beds"
names(counties_example_df)[9] <- "percent7dayhospDaily"
names(counties_example_df)[10] <- "positivePer100K"
counties_example_df$date<-anydate(counties_example_df$date)
counties_example_df<-counties_example_df%>%
  filter(date>="2021-03-08")
counties_example_df<-counties_example_df[
  with(counties_example_df, order(countyname, date)),
]
counties_example_df[is.na(counties_example_df)]<- -1


states_example_df[,10] <- ""
states_example_df[,11] <- ""
names(states_example_df)[10]<-"nation"
names(states_example_df)[11]<-"county"
final_hosptest_ts[,11] <- ""
names(final_hosptest_ts)[11]<-"positivePer100K"

final_hosptest_ts<-rbind(final_hosptest_ts,states_example_df)
final_hosptest_ts<-final_hosptest_ts[
  with(final_hosptest_ts, order(statename, date)),
]
final_hosptest_ts$percent7dayhospDaily<-round(final_hosptest_ts$percent7dayhospDaily,2)
final_hosptest_ts1<-subset(final_hosptest_ts,date<"2021-03-08")
final_hosptest_ts2<-subset(final_hosptest_ts,date>="2021-03-07")
final_hosptest_ts2$positivetoday=final_hosptest_ts2$totaltests*final_hosptest_ts2$percentPositive/100
detach("package:plyr", unload = TRUE)
final_hosptest_ts3<-final_hosptest_ts2%>%
  group_by(statename)%>%
  mutate(totaltests = cumsum(totaltests))
final_hosptest_ts3<-final_hosptest_ts3%>%
  group_by(statename)%>%
  mutate(positivetoday = cumsum(positivetoday))
final_hosptest_ts3$percentPositive=final_hosptest_ts3$positivetoday/final_hosptest_ts3$totaltests*100
final_hosptest_ts3<-final_hosptest_ts3[!(final_hosptest_ts3$date=="2021-03-07"),]
final_hosptest_ts3<-final_hosptest_ts3[,-c(12)]

final_hosptest_ts4<-rbind(final_hosptest_ts1,final_hosptest_ts3)
final_hosptest_ts4<-final_hosptest_ts4[
  with(final_hosptest_ts4, order(statename, date)),
]

final_hosptest_ts5<-final_hosptest_ts4 %>%
  group_by(statename) %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by="day"))%>%
  fill(`statename`)
final_hosptest_ts6<-final_hosptest_ts5%>%
  group_by(statename)%>%
  mutate(state=ifelse(is.na(state),lag(state),state))%>%
  mutate(percentPositive=ifelse(is.na(percentPositive),lag(percentPositive),percentPositive))%>%
  mutate(hospDaily=ifelse(is.na(hospDaily),lag(hospDaily),hospDaily))%>%
  mutate(totaltests=ifelse(is.na(totaltests),lag(totaltests),totaltests))%>%
  mutate(percent7dayhospDaily=ifelse(is.na(percent7dayhospDaily),lag(percent7dayhospDaily),percent7dayhospDaily))%>%
  mutate(hospAdmissionper100beds=ifelse(is.na(hospAdmissionper100beds),lag(hospAdmissionper100beds),hospAdmissionper100beds))%>%
  mutate(positivePer100K=ifelse(is.na(positivePer100K),lag(positivePer100K),positivePer100K))


counties_example_df1<-counties_example_df%>%
  group_by(countyname) %>%
  tidyr::complete(date = seq.Date(min(date), max(date), by="day"))%>%
  fill(`countyname`)

counties_example_df2<-counties_example_df1%>%
  group_by(countyname)%>%
  mutate(state=ifelse(is.na(state),lag(state),state))%>%
  mutate(county=ifelse(is.na(county),lag(county),county))%>%
  mutate(percentPositive=ifelse(is.na(percentPositive),lag(percentPositive),percentPositive))%>%
  mutate(hospDaily=ifelse(is.na(hospDaily),lag(hospDaily),hospDaily))%>%
  mutate(totaltests=ifelse(is.na(totaltests),lag(totaltests),totaltests))%>%
  mutate(percent7dayhospDaily=ifelse(is.na(percent7dayhospDaily),lag(percent7dayhospDaily),percent7dayhospDaily))%>%
  mutate(hospAdmissionper100beds=ifelse(is.na(hospAdmissionper100beds),lag(hospAdmissionper100beds),hospAdmissionper100beds))%>%
  mutate(positivePer100K=ifelse(is.na(positivePer100K),lag(positivePer100K),positivePer100K))


final_hosptest_ts6$county<-as.integer(final_hosptest_ts6$county)
final_hosptest_ts6$nation<-as.integer(final_hosptest_ts6$nation)
our_ts$date<-anydate(our_ts$date)
merged_covidtimeseries <- left_join(our_ts,final_hosptest_ts6)
counties_example_df2$hospAdmissionper100beds<-as.character(counties_example_df2$hospAdmissionper100beds)

variable<-c("percentPositive","hospDaily","totaltests","percent7dayhospDaily","hospAdmissionper100beds","positivePer100K")

merged_covidtimeseries[, variable][is.na(merged_covidtimeseries[, variable])] <- ""
counties_example_df2$percentPositive<-as.character(counties_example_df2$percentPositive)
counties_example_df2$hospDaily<-as.character(counties_example_df2$hospDaily)
counties_example_df2$totaltests<-as.character(counties_example_df2$totaltests)
counties_example_df2$percent7dayhospDaily<-as.character(counties_example_df2$percent7dayhospDaily)
counties_example_df2$hospAdmissionper100beds<-as.character(counties_example_df2$hospAdmissionper100beds)

counties_example_df2$positivePer100K<-as.character(counties_example_df2$positivePer100K)

merged_covidtimeseries$percentPositive<-as.character(merged_covidtimeseries$percentPositive)
merged_covidtimeseries$hospDaily<-as.character(merged_covidtimeseries$hospDaily)
merged_covidtimeseries$totaltests<-as.character(merged_covidtimeseries$totaltests)
merged_covidtimeseries$percent7dayhospDaily<-as.character(merged_covidtimeseries$percent7dayhospDaily)
merged_covidtimeseries$hospAdmissionper100beds<-as.character(merged_covidtimeseries$hospAdmissionper100beds)
merged_covidtimeseries$positivePer100K<-as.character(merged_covidtimeseries$positivePer100K)
str(merged_covidtimeseries)
str(counties_example_df2)
merged_covidtimeseries1 <- left_join(merged_covidtimeseries, counties_example_df2, by = c("date", "state", "county"))%>% 
  transmute(
    date = date,
    state = state,
    county = county,
    hospDaily = coalesce(hospDaily.y, hospDaily.x),
    totaltests = coalesce(totaltests.y, totaltests.x),
    percentPositive = coalesce(percentPositive.y, percentPositive.x),
    percent7dayhospDaily=coalesce(percent7dayhospDaily.y,percent7dayhospDaily.x),
    hospAdmissionper100beds=coalesce(hospAdmissionper100beds.y,hospAdmissionper100beds.x),
    positivePer100K=coalesce(positivePer100K.y,positivePer100K.x)
  )

covidtimeseries2 <- merged_covidtimeseries%>%
  dplyr::select(-c("percentPositive","hospDaily","totaltests","percent7dayhospDaily","hospAdmissionper100beds","positivePer100K"))
covidtimeseries2<-left_join(merged_covidtimeseries1,covidtimeseries2)

x<-covidtimeseries2[covidtimeseries2$nation==1,] %>% 
  distinct()
x<-x[-1,]
y<-subset(covidtimeseries2,is.na(nation))
covidtimeseries2<-rbind(y,x)

covidtimeseries2$nation[is.na(covidtimeseries2$nation)] <- ""
covidtimeseries2$nation[covidtimeseries2$nation == TRUE] <- "1"
covidtimeseries2$state[is.na(covidtimeseries2$state)] <- ""
covidtimeseries2$county[is.na(covidtimeseries2$county)] <- ""

covidtimeseries2$cases[is.na(covidtimeseries2$cases)] <- -1
covidtimeseries2$deaths[is.na(covidtimeseries2$deaths)] <- -1
covidtimeseries2$caserate[is.na(covidtimeseries2$caserate)] <- -1
covidtimeseries2$covidmortality[is.na(covidtimeseries2$covidmortality)] <- -1
covidtimeseries2$dailycases[is.na(covidtimeseries2$dailycases)] <- -1
covidtimeseries2$dailydeaths[is.na(covidtimeseries2$dailydeaths)] <- -1
covidtimeseries2$caserate7dayfig[is.na(covidtimeseries2$caserate7dayfig)] <- -1
covidtimeseries2$covidmortality7dayfig[is.na(covidtimeseries2$covidmortality7dayfig)] <- -1
covidtimeseries2$mean7daycases[is.na(covidtimeseries2$mean7daycases)] <- -1
covidtimeseries2$mean7daydeaths[is.na(covidtimeseries2$mean7daydeaths)] <- -1

covidtimeseries2$totaltests[is.na(covidtimeseries2$totaltests)] <- -1
covidtimeseries2$hospDaily[is.na(covidtimeseries2$hospDaily)] <- -1
covidtimeseries2$cfr[is.na(covidtimeseries2$cfr)] <- -1
covidtimeseries2$positivePer100K[is.na(covidtimeseries2$positivePer100K)] <- -1
covidtimeseries2$percentPositive[is.na(covidtimeseries2$percentPositive)] <- -1
covidtimeseries2$percent14dayDailyCases[is.na(covidtimeseries2$percent14dayDailyCases)] <- -999
covidtimeseries2$percent14dayDailyDeaths[is.na(covidtimeseries2$percent14dayDailyDeaths)] <- -999
covidtimeseries2$percent7dayhospDaily[is.na(covidtimeseries2$percent7dayhospDaily)] <- -999


###############  REGION DATA TO COVID TIME SERIES  #################

stateNames <- data.frame(stateNames)

setwd(onedrive)
regiondata <- read.csv("./Data/Processed/stateregiondata.csv")

stateregions <- merge(stateNames,regiondata,by.x=c("STATENAME","STATE"),by.y=c("statename","stateabb")) %>%
  arrange(STATEFP) %>%
  dplyr::rename(statename1=STATENAME,state=STATEFP) %>%
  dplyr::select(-STATE) %>% 
  mutate(state=sub("^(-?)0", "\\1", sprintf("%s", state)),
         state=as.integer(state))
library(plyr)
final_merged_covidtimeseries <- join(covidtimeseries2,stateregions,by="state") %>% dplyr::select(-statename1)
names(final_merged_covidtimeseries)
final_merged_covidtimeseries <- final_merged_covidtimeseries %>% 
  dplyr::select(date,nation,state,statename,county,countyname,region,division,Population,X_013_Urbanization,X_013_Urbanization_Code,urbanrural,everything())


names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_013_Urbanization"] <- "_013_Urbanization"
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_013_Urbanization_Code"] <- "_013_Urbanization_Code"
#final_merged_covidtimeseries[which(final_merged_covidtimeseries$statename %in% c("Alabama","Georgia")),]
################### National Level Hospitalization Data ###############

#national
final_merged_covidtimeseries1<-subset(final_merged_covidtimeseries,nation==1)
final_merged_covidtimeseries1<-final_merged_covidtimeseries1[!duplicated(final_merged_covidtimeseries1),]
#state level
final_merged_covidtimeseries2<-subset(final_merged_covidtimeseries,!statename=="")

detach("package:plyr", unload = TRUE)
final_merged_covidtimeseries2$percentPositive<-as.numeric(final_merged_covidtimeseries2$percentPositive)
final_merged_covidtimeseries2$hospDaily<-as.numeric(final_merged_covidtimeseries2$hospDaily)
final_merged_covidtimeseries2$totaltests<-as.numeric(final_merged_covidtimeseries2$totaltests)
final_merged_covidtimeseries2$percent7dayhospDaily<-as.numeric(final_merged_covidtimeseries2$percent7dayhospDaily)
final_merged_covidtimeseries2$hospAdmissionper100beds<-as.numeric(final_merged_covidtimeseries2$hospAdmissionper100beds)
final_merged_covidtimeseries2$positivePer100K<-as.numeric(final_merged_covidtimeseries2$positivePer100K)

final_merged_covidtimeseries3<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(percentPositive=mean(percentPositive,na.rm=TRUE))
final_merged_covidtimeseries3<-final_merged_covidtimeseries3[,c(1,15)]
final_merged_covidtimeseries3<-final_merged_covidtimeseries3[!duplicated(final_merged_covidtimeseries3), ]
final_merged_covidtimeseries1$percentPositive=final_merged_covidtimeseries3$percentPositive
final_merged_covidtimeseries4<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(hospDaily=sum(hospDaily,na.rm=TRUE))
final_merged_covidtimeseries4<-final_merged_covidtimeseries4[,c(1,13)]
final_merged_covidtimeseries4<-final_merged_covidtimeseries4[!duplicated(final_merged_covidtimeseries4), ]
final_merged_covidtimeseries1$hospDaily=final_merged_covidtimeseries4$hospDaily
final_merged_covidtimeseries5<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(totaltests=sum(totaltests,na.rm=TRUE))
final_merged_covidtimeseries5<-final_merged_covidtimeseries5[,c(1,14)]
final_merged_covidtimeseries5<-final_merged_covidtimeseries5[!duplicated(final_merged_covidtimeseries5), ]
final_merged_covidtimeseries1$totaltests=final_merged_covidtimeseries5$totaltests
final_merged_covidtimeseries2<-final_merged_covidtimeseries2%>%mutate(percent7dayhospDaily=ifelse(percent7dayhospDaily==-999.00,0,percent7dayhospDaily))
final_merged_covidtimeseries6<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(percent7dayhospDaily=mean(percent7dayhospDaily,na.rm=TRUE))
final_merged_covidtimeseries6<-final_merged_covidtimeseries6[,c(1,16)]
final_merged_covidtimeseries6<-final_merged_covidtimeseries6[!duplicated(final_merged_covidtimeseries6), ]
final_merged_covidtimeseries1$percent7dayhospDaily=final_merged_covidtimeseries6$percent7dayhospDaily
final_merged_covidtimeseries7<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(hospAdmissionper100beds=mean(hospAdmissionper100beds,na.rm=TRUE))
final_merged_covidtimeseries7<-final_merged_covidtimeseries7[,c(1,17)]
final_merged_covidtimeseries7<-final_merged_covidtimeseries7[!duplicated(final_merged_covidtimeseries7), ]
final_merged_covidtimeseries1$hospAdmissionper100beds=final_merged_covidtimeseries7$hospAdmissionper100beds
final_merged_covidtimeseries8<-final_merged_covidtimeseries2%>%group_by(date)%>%mutate(positivePer100K=mean(positivePer100K,na.rm=TRUE))
final_merged_covidtimeseries8<-final_merged_covidtimeseries8[,c(1,18)]
final_merged_covidtimeseries8<-final_merged_covidtimeseries8[!duplicated(final_merged_covidtimeseries8), ]
final_merged_covidtimeseries1$positivePer100K=final_merged_covidtimeseries8$positivePer100K


final_merged_covidtimeseries1$percentPositive[is.nan(final_merged_covidtimeseries1$percentPositive)]<--1
final_merged_covidtimeseries1$hospDaily[is.nan(final_merged_covidtimeseries1$hospDaily)]<--1
final_merged_covidtimeseries1$totaltests[is.nan(final_merged_covidtimeseries1$totaltests)]<--1
final_merged_covidtimeseries1$percent7dayhospDaily[is.nan(final_merged_covidtimeseries1$percent7dayhospDaily)]<--1
final_merged_covidtimeseries1$hospAdmissionper100beds[is.nan(final_merged_covidtimeseries1$hospAdmissionper100beds)]<--1
final_merged_covidtimeseries1$positivePer100K[is.nan(final_merged_covidtimeseries1$positivePer100K)]<--1

final_merged_covidtimeseries2<-subset(final_merged_covidtimeseries,nation=="")
final_merged_covidtimeseries<-rbind(final_merged_covidtimeseries2,final_merged_covidtimeseries1)
#x<-subset(final_merged_covidtimeseries,nation==1)

final_merged_covidtimeseries$percentPositive <- ifelse(final_merged_covidtimeseries$percentPositive == "", -1, final_merged_covidtimeseries$percentPositive)
final_merged_covidtimeseries$hospDaily<-ifelse(final_merged_covidtimeseries$hospDaily == "", -1, final_merged_covidtimeseries$hospDaily)
final_merged_covidtimeseries$totaltests<-ifelse(final_merged_covidtimeseries$totaltests == "", -1, final_merged_covidtimeseries$totaltests)
final_merged_covidtimeseries$percent7dayhospDaily<-ifelse(final_merged_covidtimeseries$percent7dayhospDaily == "", -1, final_merged_covidtimeseries$percent7dayhospDaily)
final_merged_covidtimeseries$hospAdmissionper100beds<-ifelse(final_merged_covidtimeseries$hospAdmissionper100beds == "", -1, final_merged_covidtimeseries$hospAdmissionper100beds)
final_merged_covidtimeseries$positivePer100K<-ifelse(final_merged_covidtimeseries$positivePer100K == "", -1, final_merged_covidtimeseries$positivePer100K)

final_merged_covidtimeseries<-subset(final_merged_covidtimeseries,!is.na(date))


county<-subset(final_merged_covidtimeseries,!countyname=="")
rest<-subset(final_merged_covidtimeseries,countyname=="")

#load all the packages again
library(base)
library(tidyverse)
library(tidyr)
library(plyr)
library(readr)
library(data.table)
library(ggplot2)
library(tibble)
library(lubridate)
library(cdlTools)
library(openintro)
library(gtools)
library(rvest)
library(dplyr)
library(DescTools)
library(readxl)
library(stringr)  
library(naniar)
library(zoo)
library(mice)



final_merged_covidtimeseries1<-county%>%group_by(countyname,date)%>%
  mutate(percentPositive1=lag(percentPositive))%>%
  mutate(hospDaily1=lag(hospDaily))%>%
  mutate(totaltests1=lag(totaltests))%>%
  mutate(percent7dayhospDaily1=lag(percent7dayhospDaily))%>%
  mutate(hospAdmissionper100beds1=lag(hospAdmissionper100beds))%>%
  mutate(positivePer100K1=lag(positivePer100K))

final_merged_covidtimeseries2<-rest%>%group_by(statename,date)%>%
  mutate(percentPositive1=lag(percentPositive))%>%
  mutate(hospDaily1=lag(hospDaily))%>%
  mutate(totaltests1=lag(totaltests))%>%
  mutate(percent7dayhospDaily1=lag(percent7dayhospDaily))%>%
  mutate(hospAdmissionper100beds1=lag(hospAdmissionper100beds))%>%
  mutate(positivePer100K1=lag(positivePer100K))

county$percentPositive[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$percentPositive1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$hospDaily[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$hospDaily1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$totaltests[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$totaltests1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$percent7dayhospDaily[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$percent7dayhospDaily1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$hospAdmissionper100beds[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$hospAdmissionper100beds1[final_merged_covidtimeseries1$date=="2021-04-03"]
county$positivePer100K[county$date=="2021-04-03"]<-final_merged_covidtimeseries1$positivePer100K1[final_merged_covidtimeseries1$date=="2021-04-03"]


rest$percentPositive[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$percentPositive1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$hospDaily[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$hospDaily1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$totaltests[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$totaltests1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$percent7dayhospDaily[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$percent7dayhospDaily1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$hospAdmissionper100beds[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$hospAdmissionper100beds1[final_merged_covidtimeseries2$date=="2021-04-03"]
rest$positivePer100K[rest$date=="2021-04-03"]<-final_merged_covidtimeseries2$positivePer100K1[final_merged_covidtimeseries2$date=="2021-04-03"]



final_merged_covidtimeseries_x<-rbind(rest,county)
nation<-subset(final_merged_covidtimeseries_x,nation==1)
not_nation<-subset(final_merged_covidtimeseries_x,nation=="")

not_nation<-not_nation[
  with(not_nation, order(state,county)),
]
final_merged_covidtimeseries_x<-rbind(not_nation,nation)


final<-function(data){
  final_merged_covidtimeseries_x<-data%>%
    dplyr::group_by(countyname)%>%
    dplyr::mutate(percentPositive=ifelse(percentPositive==-1,lag(percentPositive),percentPositive))%>%
    dplyr::mutate(hospDaily=ifelse(hospDaily==-1,lag(hospDaily),hospDaily))%>%
    dplyr::mutate(totaltests=ifelse(totaltests==-1,lag(totaltests),totaltests))%>%
    dplyr::mutate(percent7dayhospDaily=ifelse(percent7dayhospDaily==-1,lag(percent7dayhospDaily),percent7dayhospDaily))%>%
    dplyr::mutate(hospAdmissionper100beds=ifelse(hospAdmissionper100beds==-1,lag(hospAdmissionper100beds),hospAdmissionper100beds))%>%
    dplyr::mutate(positivePer100K=ifelse(positivePer100K==-1,lag(positivePer100K),positivePer100K))
  return(final_merged_covidtimeseries_x)
}


x<-Sys.Date()-1
cpr_date<-regmatches(df, gregexpr("[[:digit:]]+", df))
cpr_date<-ymd(cpr_date)  
y<-as.numeric(x-cpr_date)


m<-Reduce(function(x, ign) final(x), 1:y, init = final_merged_covidtimeseries_x, accumulate = TRUE)
trial<-m[y+1]
trial<-as.data.frame(trial)

#get national raw data
acs2018_5yr_county = haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/ACS 2018 5yr/acs2018_5yr_county.sas7bdat")) %>%
  dplyr::rename(fips = FIPS) %>% 
  mutate(popden = A00002_002,
         age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100,
         male      = A02001_002 / A00001_001 * 100,
         female    = A02001_003 / A00001_001 * 100,
         black     = A04001_004 / A00001_001 * 100,  
         hispanic  = A04001_010 / A00001_001 * 100,
         minority = (A00001_001-A04001_003)/ A00001_001 * 100,
         natives =   A03001_004/ A00001_001 * 100,
         groupquater = A19001_002/A00001_001 * 100,
         college =     A12001_005/A12001_001 * 100,
         hhincome = A14008_001,
         poverty = A13005_002 / A13005_001 * 100, 
         
         county=(substr(fips,3,5)) %>% as.numeric(),
         state=(substr(fips,1,2)) %>% as.numeric()) %>% 
  dplyr::select(county,state,fips,popden,
                age65over,male,female,
                black,hispanic,minority,natives,
                groupquater,college,hhincome,poverty)


# acs2018_5yr_state ------

acs2018_5yr_state = haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/ACS 2018 5yr/acs2018_5yr_state.sas7bdat")) %>%
  dplyr::rename(fips = FIPS) %>% 
  mutate(popden = A00002_002,
         age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100,
         male      = A02001_002 / A00001_001 * 100,
         female    = A02001_003 / A00001_001 * 100,
         black     = A04001_004 / A00001_001 * 100,  
         hispanic  = A04001_010 / A00001_001 * 100,
         minority = (A00001_001-A04001_003)/ A00001_001 * 100,
         natives =   A03001_004/ A00001_001 * 100,
         groupquater = A19001_002/A00001_001 * 100,
         college =     A12001_005/A12001_001 * 100,
         hhincome = A14008_001,
         poverty = A13005_002 / A13005_001 * 100, 
         
         county=NA_real_,
         state=(substr(fips,1,2)) %>% as.numeric()) %>% 
  dplyr::select(county,state,fips,popden,
                age65over,male,female,
                black,hispanic,minority,natives,
                groupquater,college,hhincome,poverty)

# acs2018_5yr_nation --------

acs2018_5yr_nation = haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/ACS 2018 5yr/acs2018_5yr_nation.sas7bdat")) %>%
  dplyr::rename(fips = FIPS) %>% 
  mutate(popden = A00002_002,
         age65over = (A01001_011 + A01001_012 + A01001_013) / A00001_001 * 100,
         male      = A02001_002 / A00001_001 * 100,
         female    = A02001_003 / A00001_001 * 100,
         black     = A04001_004 / A00001_001 * 100,  
         hispanic  = A04001_010 / A00001_001 * 100,
         minority = (A00001_001-A04001_003)/ A00001_001 * 100,
         natives =   A03001_004/ A00001_001 * 100,
         groupquater = A19001_002/A00001_001 * 100,
         college =     A12001_005/A12001_001 * 100,
         hhincome = A14008_001,
         poverty = A13005_002 / A13005_001 * 100, 
         
         county=NA_real_,
         state=NA_real_,
         nation = 1) %>% 
  dplyr::select(county,state,nation,fips,popden,
                age65over,male,female,
                black,hispanic,minority,natives,
                groupquater,college,hhincome,poverty)

# acs2018_5yr_all ----------

acs2018_5yr_all <- bind_rows(
  acs2018_5yr_nation,
  acs2018_5yr_state,
  acs2018_5yr_county
) %>% 
  dplyr::filter(!state %in% c(66,69,72,78))

SVI <- haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/CDC SVI/svi2018_us_county.sas7bdat")) %>% 
  mutate(fips = sprintf("%05d",FIPS)) %>% 
  mutate(county = substr(fips,3,5) %>% as.numeric(),
         nation = NA_real_) %>% 
  dplyr::rename(state = ST) %>% 
  dplyr::filter(!state %in% c(66,69,72,78)) %>% 
  dplyr::select(nation,state,county,fips,RPL_THEME1,RPL_THEME2,RPL_THEME3,RPL_THEME4)

onedrive
path_c19dashboard_old_folder <- "/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Raw"
diabetescounty0 <- haven::read_sas(paste0(path_c19dashboard_old_folder,"/CDC Diabetes Surveillance/county.sas7bdat")) %>% 
  
  dplyr::rename(state = Fips) %>% 
  mutate(fips = sprintf("%05d",CountyFIPS)) %>% 
  mutate(county = substr(fips,3,5) %>% as.numeric()) %>% 
  dplyr::select(-County,-State,-CountyFIPS,-fips) %>% 
  dplyr::filter(!state %in% c(66,69,72,78))


diabetesstate0 <- diabetescounty0 %>% 
  group_by(state) %>% 
  summarize_at(vars(diabetes,obesity),~mean(.,na.rm=TRUE)) %>% 
  ungroup() 

diabetesnation0 <- diabetescounty0 %>% 
  summarize_at(vars(diabetes,obesity),~mean(.,na.rm=TRUE)) %>% 
  mutate(nation = 1)

CDCdiabetes <- bind_rows(
  diabetesnation0,
  diabetesstate0,
  diabetescounty0
)


sahie_2018 <- haven::read_sas(paste0(path_c19dashboard_shared_folder,"/Data/Raw/SAHIE/sahie_2018_new.sas7bdat")) %>% 
  mutate(county = case_when(countyfips == 0 ~ NA_real_,
                            TRUE ~ countyfips),
         nation = NA_real_
  ) %>% 
  dplyr::rename(state = statefips) %>% 
  dplyr::select(nation,county,state,PCTUI,PCTIC)

# /*This file is not needed - state is covered in county;
# sahie_2018_state2 <- sahie_2018 %>% 
#   group_by(state) %>% 
#   summarize_at(vars(PCTUI,PCTIC),~mean(.,na.rm=TRUE)) %>% 
#   mutate(nation = 1,
#          county = NA_real_) %>% 
#   ungroup()



sahie_2018_nat2 <- sahie_2018 %>% 
  summarize_at(vars(PCTUI,PCTIC),~mean(.,na.rm=TRUE)) %>% 
  mutate(nation = 1,
         state = NA_real_,
         county = NA_real_)


sahie_2018_merge <- bind_rows(sahie_2018,sahie_2018_nat2)


acs2018_5yr_all <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/ACS 2018 5yr/acs2018_5yr_all.RDS"))
SVI <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC SVI/SVI.RDS"))
CDCdiabetes <- readRDS(paste0(path_c19dashboard_shared_folder,"/Data/Processed/CDC Diabetes Surveillance/CDCdiabetes.RDS"))
sahie_2018_merge <- readRDS("/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Processed/SAHIE/sahie_2018_merge.RDS")


mergedsocial <- acs2018_5yr_all %>% 
  full_join(SVI %>% dplyr::select(-fips),
            by = c("nation","state","county")) %>% 
  full_join(CDCdiabetes,
            by=c("nation","state","county")) %>% 
  full_join(sahie_2018_merge,
            by=c("nation","state","county")) %>% 
  dplyr::filter(!is.na(nation)|!is.na(state)|!is.na(county))


# mergedsocial is exported as nationalraw0.csv

# pending: Variable labelling  ---------

var_labels_mergedsocial <- read.table(file="/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Raw/ddaux01_variable labels.txt",sep = "=",
                                      col.names = c("variable","label")) %>% 
  mutate_all(~as.character(.) %>% trimws(.))




library(labelled)
var_label(mergedsocial) <- map(colnames(mergedsocial),
                               .f= function(x){
                                 x_label = var_labels_mergedsocial %>% 
                                   dplyr::filter(variable == x) %>% 
                                   dplyr::select(label) %>% 
                                   pull();
                                 x_label = ifelse(length(x_label) == 0,"",x_label);
                                 return(x_label)
                               }) %>% unlist()

our_static <-mergedsocial

final_hosptest_ts7<-final_hosptest_ts6%>%
  filter(date==max(final_hosptest_ts6$date)) 

our_static$date<-anydate(our_static$date)
final_hosptest_ts7$percentPositive<-as.character(final_hosptest_ts7$percentPositive)
final_hosptest_ts7$hospDaily<-as.character(final_hosptest_ts7$hospDaily)
final_hosptest_ts7$totaltests<-as.character(final_hosptest_ts7$totaltests)
final_hosptest_ts7$percent7dayhospDaily<-as.character(final_hosptest_ts7$percent7dayhospDaily)


merged_nationalraw <- left_join(our_static,final_hosptest_ts7)
merged_nationalraw$date<-anydate(merged_nationalraw$date)

merged_nationalraw$hospDaily<-as.character(merged_nationalraw$hospDaily)
merged_nationalraw$percentPositive<-as.character(merged_nationalraw$percentPositive)
merged_nationalraw$totaltests<-as.character(merged_nationalraw$totaltests)
merged_nationalraw$percent7dayhospDaily<-as.character(merged_nationalraw$percent7dayhospDaily)
merged_nationalraw$hospAdmissionper100beds<-as.character(merged_nationalraw$hospAdmissionper100beds)
merged_nationalraw$positivePer100K<-as.character(merged_nationalraw$positivePer100K)
variable<-c("percentPositive","hospDaily","totaltests","percent7dayhospDaily","hospAdmissionper100beds","positivePer100K")
counties_example_df3<-counties_example_df2%>%
  filter(date==max(counties_example_df2$date))
counties_example_df3<-counties_example_df3[,-c(2)]
merged_nationalraw1 <- left_join(merged_nationalraw, counties_example_df3, by = c("state", "county"))%>% 
  transmute(
    date = date,
    state = state,
    county = county,
    hospDaily = coalesce(hospDaily.y, hospDaily.x),
    totaltests = coalesce(totaltests.y, totaltests.x),
    percentPositive = coalesce(percentPositive.y, percentPositive.x),
    percent7dayhospDaily=coalesce(percent7dayhospDaily.y,percent7dayhospDaily.x),
    hospAdmissionper100beds=coalesce(hospAdmissionper100beds.y,hospAdmissionper100beds.x),
    positivePer100K=coalesce(positivePer100K.y,positivePer100K.x)
  )

merged_nationalraw2 <- merged_nationalraw%>%
  dplyr::select(-variable)

merged_nationalraw2 <- left_join(merged_nationalraw2, merged_nationalraw1, by = c("date", "state", "county"))


merged_nationalraw2[, variable == ""] <- NA
merged_nationalraw2[, variable][is.na(merged_nationalraw2[, variable])] <- -1



rm(our_static)


########## REGION DATA TO NATIONALRAW ############

stateNames <- data.frame(stateNames)


regiondata <- read.csv("/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Processed/stateregiondata.csv")


stateregions <- merge(stateNames,regiondata,by.x=c("STATENAME","STATE"),by.y=c("statename","stateabb")) %>%
  arrange(STATEFP) %>%
  dplyr::rename(statename1=STATENAME,state=STATEFP) %>%
  dplyr::select(-STATE) %>% 
  mutate(state=sub("^(-?)0", "\\1", sprintf("%s", state)),
         state=as.integer(state))

library(plyr)

merge <- join(merged_nationalraw2,stateregions,by="state") %>% dplyr::select(-statename1)

final_merged_nationalraw <- merge %>% 
  dplyr::select(date,nation,state,statename,county,countyname,region,Population,X_2013_Urbanization_Code,everything())

########## Merge CDC Chronic Condition Data to Nationalraw ############
setwd(box1)
Chronic_Conditions<-read_excel("./DataUpload/cdc_90519_DS1.xlsx")


Chronic_Conditions$CNTY_FIPS = str_remove(Chronic_Conditions$CNTY_FIPS, "^0+")
names(Chronic_Conditions)[names(Chronic_Conditions) == "CNTY_FIPS"] <- "county"
names(Chronic_Conditions)[names(Chronic_Conditions) == "STATE_FIPS"] <- "state"
Chronic_Conditions$county<-as.integer(Chronic_Conditions$county)

Chronic_Conditions$state = str_remove(Chronic_Conditions$state, "^0+")

final_merged_nationalraw<-join(final_merged_nationalraw,Chronic_Conditions)


names(final_merged_nationalraw)[names(final_merged_nationalraw) == "county_pop2018_18.and.older"] <- "countyPop201818andolder"

names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_prevalence"] <- "anyconditionPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_Lower 95% CI"] <- "anyconditionLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_Upper 95% CI"] <- "anyconditionUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_number"] <- "anyconditionNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "anycondition_prevalence"] <- "anyconditionPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_prevalence"] <- "obesityPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_Lower 95% CI"] <- "obesityLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_Upper 95% CI"] <- "obesityUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Obesity_number"] <- "obesityNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_prevalence"] <- "heartdiseasePrevalence"

names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_Lower 95% CI"] <- "heartdiseaseLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_Upper 95% CI"] <- "heartdiseaseUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "Heart disease_number"] <- "heartdiseaseNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_prevalence"] <- "copdPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_Lower 95% CI"] <- "copdLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_Upper 95% CI"] <- "copdUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "COPD_number"] <- "copdNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_prevalence"] <- "diabetesPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_Lower 95% CI"] <- "diabetesLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_Upper 95% CI"] <- "diabetesUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "diabetes_number"] <- "diabetesNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_prevalence"] <- "ckdPrevalence"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_Lower 95% CI"] <- "ckdLower95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_Upper 95% CI"] <- "ckdUpper95CI"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "CKD_number"] <- "ckdNumber"
names(final_merged_nationalraw)[names(final_merged_nationalraw) == "county_pop2018_18 and older"] <- "countyPop201818andolder"


final_merged_nationalraw<-final_merged_nationalraw %>%
  dplyr::select(- c(COUNTY_NAME, STATE_NAME, FIPS,Urban_rural_code,STAB,anyconditionLower95CI,anyconditionUpper95CI,obesityLower95CI,obesityUpper95CI,heartdiseaseLower95CI,heartdiseaseUpper95CI,copdLower95CI,copdUpper95CI,diabetesLower95CI,diabetesUpper95CI,ckdLower95CI,ckdUpper95CI,diabetes,obesity))


##################  RENAMING URBANIZATION COLUMNS BEFORE EXPORTING  ################  


rm(merged_nationalraw,black,age65over,diabetes,groupquater,hhincome,hispanic,male,merge,minority,natives,obesity,poverty,regiontoNR,stateNames,stateregions,regiondata,outcomes, ourraw)


################### ***UNLOCK TO EXPORT*** ###############
setwd(onedrive)
write.csv(final_merged_nationalraw,"./Data/Upload/nationalraw.csv", na="", row.names=F)

# reset directory
setwd(local)
write.csv(final_merged_nationalraw,"./nationalraw.csv", na="", row.names=F)





########## NATIONAL REPORT: Highest 10 BAR CHARTS ###################


## Highest 10 CASES CHANGE 

setwd(local)
highcases <- as_tibble(read.csv("./nationalraw.csv")) %>%
  filter(!countyname=="") %>% 
  mutate(caserate7day=round(caserate7day),date=as.Date(date,origin = "1899-12-30")) %>%
  dplyr::select(date,countyname,caserate7day)


Highest10cases <- highcases %>% 
  arrange(desc(caserate7day)) %>% 
  slice(1:10) %>% 
  mutate(variable = "caserate7day", rank = seq(1:10)) %>% 
  dplyr::rename(measure = caserate7day)



## Highest 10 DEATHS CHANGE 

setwd(local)
highdeaths <- as_tibble(read.csv("./nationalraw.csv")) %>%
  filter(!countyname=="") %>% 
  mutate(covidmortality7day=round(covidmortality7day),date=as.Date(date,origin = "1899-12-30")) %>%
  dplyr::select(date,countyname,covidmortality7day)


Highest10deaths <- highdeaths %>% 
  arrange(desc(covidmortality7day)) %>%
  slice(1:10) %>%
  mutate(variable = "covidmortality7day", rank = seq(1:10)) %>% 
  dplyr::rename(measure = covidmortality7day)


# MERGE THE DATASET
merge <- full_join(Highest10cases,Highest10deaths) %>% dplyr::select(date,countyname,variable,measure,rank)



################### ***UNLOCK TO EXPORT*** ###############

#write.csv(merge,"./DataUpload/Yubin/Highest10barchart.csv",row.names=FALSE,na="")
write.csv(merge,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/Highest10barchart.csv",row.names=FALSE,na="")

# KEEP THE GLOBAL ENVIRONMENT CLEAN
rm(highcases,highdeaths,Highest10cases,Highest10deaths,merge)



########### NATIONAL REPORT: TRENDLINE PLOTS ####################


# CASE RATE 

setwd(local)
raw <- read.csv("./nationalraw.csv") %>% 
  filter(!countyname=="") %>% 
  dplyr::select(countyname,percent14dayDailyCases) %>%
  arrange(desc(percent14dayDailyCases)) %>% slice(1:10) %>% 
  mutate(rank = seq(1:10)) %>% 
  dplyr::select(-percent14dayDailyCases)



data <- trial %>% 
  left_join(raw) %>% 
  filter(!rank=="") %>%
  dplyr::select(date,countyname,rank,caserate7day) %>%
  arrange(rank,desc(date)) %>%
  mutate(variable = "caserate7day") %>%
  dplyr::rename(measure = caserate7day)


# DEATH RATE 

setwd(local)
death <- read.csv("./nationalraw.csv") %>% 
  filter(!countyname=="") %>% 
  dplyr::select(countyname,percent14dayDailyDeaths) %>%
  arrange(desc(percent14dayDailyDeaths)) %>% slice(1:5) %>% 
  mutate(rank = seq(1:5)) %>% 
  dplyr::select(-percent14dayDailyDeaths)



deathraw <- trial %>% 
  left_join(death) %>% 
  filter(!rank=="") %>%
  dplyr::select(date,countyname,rank,covidmortality7day) %>%
  arrange(rank,desc(date)) %>%
  mutate(variable = "covidmortality7day") %>%
  dplyr::rename(measure = covidmortality7day)


# MERGE DATA 

merge <- full_join(data,deathraw) %>% dplyr::select(date,countyname,variable,measure,rank)


################### ***UNLOCK TO EXPORT*** ###############
setwd(onedrive)
write.csv(merge,"./Data/Upload/Highest10trendlines.csv",row.names=FALSE,na="")



######################  MERGING CVI SCORE  ######################

ccvi_states <- openxlsx::read.xlsx("/Users/air/Downloads/surgo_ccvi_with_raw_public_indicators/ccvi.xlsx",sheet=1) %>%
  dplyr::rename(statename=stateName,state=FIPS) %>%
  mutate(statename=str_to_title(statename), countyname="", countycode=NA)

statecode <- ccvi_states %>% dplyr::select(statename,state)

ccvi_county <- openxlsx::read.xlsx("/Users/air/Downloads/surgo_ccvi_with_raw_public_indicators/ccvi.xlsx",sheet=2) %>%
  dplyr::rename(statename=stateName,countyname=countyName,countycode=FIPS) %>%
  mutate(statename=str_to_title(statename)) %>% join(statecode)

CVI <- full_join(ccvi_county,ccvi_states) %>% dplyr::select(-countyname)


CVI$statename[!is.na(CVI$countycode)] <- ""

setwd(local)
nationalraw <- read.csv("./nationalraw.csv")

CVImerged <- join(nationalraw,CVI)


################## ***UNLOCK TO EXPORT*** ###############


setwd(local)
write.csv(CVImerged,"./nationalraw.csv",row.names=FALSE,na="")


# keep the global environment clean
rm(ccvi_states,statecode,ccvi_county,CVI,nationalraw)


#######################  MERGING RESIDENTIAL SEGREGATION SCORE  ######################

setwd(onedrive)
resseg <- read.csv("./Data/Processed/residSEg.csv") 



setwd(local)
nationalraw <- read.csv("./nationalraw.csv")
# merge with nationalraw
ressegToNR <- left_join(nationalraw,resseg)
ressegToNR$X_2013_Urbanization_Code  <-  factor(ifelse(ressegToNR$X_2013_Urbanization=="",-1,
                                                       ifelse(ressegToNR$X_2013_Urbanization=="Large Central Metro",6,
                                                              ifelse(ressegToNR$X_2013_Urbanization=="Large Fringe Metro",5,
                                                                     ifelse(ressegToNR$X_2013_Urbanization=="Medium Metro",4,
                                                                            ifelse(ressegToNR$X_2013_Urbanization=="Small Metro",3,
                                                                                   ifelse(ressegToNR$X_2013_Urbanization=="Micropolitan (Nonmetro)",2,1)))))))



ressegToNR$region_Code  <- factor(ifelse(ressegToNR$region=="",-1,
                                         ifelse(ressegToNR$region=="South",1,
                                                ifelse(ressegToNR$region=="West",2,
                                                       ifelse(ressegToNR$region=="Northeast",3,4)))))




################### ***UNLOCK TO EXPORT*** ###############

setwd(local)
write.csv(ressegToNR,"./nationalraw.csv",row.names=FALSE,na="")

#x<-subset(ressegToNR,nation=="1")


# keep the global environment clean





########### HOW MANY STATES CONTRIBUTED? ############


setwd(local)
raw <- ressegToNR%>% 
  filter(nation=="1"|county==""|!statename=="") %>% 
  dplyr::select(date,state,nation,county,statename,cases,dailycases) %>%
  mutate(date = anydate(date))
#raw1<-raw[!duplicated(raw[,c("statename","state")]),]

raw<-raw%>%
  group_by(statename) %>%
  slice(n()) 




totnationcases <- as.numeric(raw$dailycases[raw$nation=="1"]) 
totalnationdailycases <- totnationcases[!is.na(totnationcases)]


sharedailycases <- raw %>%
  group_by(statename) %>%
  mutate(
    contri = totalnationdailycases*0.5) %>%
  dplyr::select(date,nation,state,statename,dailycases,contri) %>% filter(!nation=="1"|!state=="") %>% arrange(desc(dailycases))

threshold <- max(sharedailycases$contri)

number_of_states <- as.numeric(length(which(cumsum(sharedailycases$dailycases) <= threshold)))

names <- as.data.frame(head(sharedailycases$statename,number_of_states))

contristates <- as_tibble(apply(names,2,paste,collapse=", ")) %>% dplyr::rename(statenames=value)



jsonlite::write_json(contristates,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/contristates.json")


rm(number_of_states,names,contristates,sharedailycases,raw,totnationcases,totalnationdailycases,threshold)


############  OUTCOME VS COUNTY CHARACTERISTICS DATA TO NATIONAL RAW #########

setwd(local)
ourraw <- read.csv("./nationalraw.csv")

names(ourraw)

ourraw$malegroup = quantcut(ourraw$male,5)
table(ourraw$malegroup)

ourraw$age65overgroup = quantcut(ourraw$age65over,5)
table(ourraw$age65overgroup)

ourraw$blackgroup = quantcut(ourraw$black,5)
table(ourraw$blackgroup)

ourraw$povertygroup = quantcut(ourraw$poverty,5)
table(ourraw$povertygroup)

ourraw$minoritygroup = quantcut(ourraw$minority,5)
table(ourraw$minoritygroup)

ourraw$groupquatergroup = quantcut(ourraw$groupquater,5)
table(ourraw$groupquatergroup)



ourraw$hispanicgroup = quantcut(ourraw$hispanic,5)
table(ourraw$hispanicgroup)

ourraw$nativesgroup = quantcut(ourraw$natives,5)
table(ourraw$nativesgroup)



ourraw$hhincomegroup = quantcut(ourraw$hhincome,5)
table(ourraw$hhincomegroup)

ourraw$cvigroup = quantcut(ourraw$ccvi,5)
table(ourraw$cvigroup)

ourraw$residseggroup = quantcut(ourraw$RS_blackwhite,5)
table(ourraw$residseggroup)

ourraw$anyconditiongroup = quantcut(ourraw$anyconditionPrevalence,5)
table(ourraw$anyconditiongroup)

ourraw$obesity2group = quantcut(ourraw$obesityPrevalence,5)
table(ourraw$obesity2group)

ourraw$diabetes2group = quantcut(ourraw$diabetesPrevalence,5)
table(ourraw$diabetes2group)

ourraw$heartdiseasegroup = quantcut(ourraw$heartdiseasePrevalence,5)
table(ourraw$heartdiseasegroup)

ourraw$copdgroup = quantcut(ourraw$copdPrevalence,5)
table(ourraw$copdgroup)

ourraw$ckdgroup = quantcut(ourraw$ckdPrevalence,5)
table(ourraw$ckdgroup)


###############  CASE RATE VS EXPOSURE CHARTS ################

black <- aggregate(cbind(caserate7day,caserate)~blackgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="black") %>% dplyr::rename(quintgroup=blackgroup)
male <- aggregate(cbind(caserate7day,caserate)~malegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="male") %>% dplyr::rename(quintgroup=malegroup)
poverty <- aggregate(cbind(caserate7day,caserate)~povertygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="poverty") %>% dplyr::rename(quintgroup=povertygroup)
age65over <- aggregate(cbind(caserate7day,caserate)~age65overgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="age65over") %>% dplyr::rename(quintgroup=age65overgroup)
minority <- aggregate(cbind(caserate7day,caserate)~minoritygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="minority") %>% dplyr::rename(quintgroup=minoritygroup)
groupquater <- aggregate(cbind(caserate7day,caserate)~groupquatergroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="groupquarter") %>% dplyr::rename(quintgroup=groupquatergroup)

hispanic <- aggregate(cbind(caserate7day,caserate)~hispanicgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hispanic") %>% dplyr::rename(quintgroup=hispanicgroup)
natives <- aggregate(cbind(caserate7day,caserate)~nativesgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="natives") %>% dplyr::rename(quintgroup=nativesgroup)
urbanrural <- aggregate(cbind(caserate7day,caserate)~X_013_Urbanization,ourraw,FUN="mean") %>% filter(!X_013_Urbanization=="") %>% mutate(quintileVar="urbanrural",lbl = X_013_Urbanization) %>% dplyr::rename(quintgroup=X_013_Urbanization)

hhincome <- aggregate(cbind(caserate7day,caserate)~hhincomegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hhincome") %>% dplyr::rename(quintgroup=hhincomegroup)
region <- aggregate(cbind(caserate7day,caserate)~region,ourraw,FUN="mean") %>% filter(!region=="") %>% mutate(quintileVar="region",lbl = region) %>% dplyr::rename(quintgroup=region)
cvi <- aggregate(cbind(caserate7day,caserate)~cvigroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CVI") %>% dplyr::rename(quintgroup=cvigroup)
residseg <- aggregate(cbind(caserate7day,caserate)~residseggroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="resSeg") %>% dplyr::rename(quintgroup=residseggroup)

anycondition <- aggregate(cbind(caserate7day,caserate)~anyconditiongroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="any condition") %>% dplyr::rename(quintgroup=anyconditiongroup)
diabetes2 <- aggregate(cbind(caserate7day,caserate)~diabetes2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="diabetes 2018") %>% dplyr::rename(quintgroup=diabetes2group)
obesity2 <- aggregate(cbind(caserate7day,caserate)~obesity2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="obesity 2018") %>% dplyr::rename(quintgroup=obesity2group)
heartdisease <- aggregate(cbind(caserate7day,caserate)~heartdiseasegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="heart disease") %>% dplyr::rename(quintgroup=heartdiseasegroup)
copd <- aggregate(cbind(caserate7day,caserate)~copdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="COPD") %>% dplyr::rename(quintgroup=copdgroup)
ckd <- aggregate(cbind(caserate7day,caserate)~ckdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CKD") %>% dplyr::rename(quintgroup=ckdgroup)


outcomes_pre <- full_join(black,male) %>% full_join(poverty) %>% full_join(age65over) %>%
  full_join(minority) %>% full_join(groupquater)  %>% full_join(hispanic) %>%
  full_join(natives) %>% full_join(urbanrural)  %>% full_join(hhincome) %>% full_join(region) %>%
  full_join(cvi) %>% full_join(residseg) %>% full_join(anycondition) %>% full_join(diabetes2) %>% full_join(obesity2) %>%
  full_join(heartdisease) %>% full_join(copd) %>% full_join(ckd) %>%
  dplyr::select(caserate7day,everything())

outcomes <- gather(outcomes_pre,
                   key="variable",
                   value="measure",
                   c("caserate","caserate7day")) %>% dplyr::select(quintileVar,quintgroup,lbl,variable,measure)


rm(outcomes_pre,black,male,poverty,age65over,minority,groupquater,hispanic,natives,urbanrural,hhincome,region,cvi,residseg)



###############  covidmortality VS EXPOSURE CHARTS ################


black_mort <- aggregate(cbind(covidmortality,covidmortality7day)~blackgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="black") %>% dplyr::rename(quintgroup=blackgroup)
male_mort <- aggregate(cbind(covidmortality,covidmortality7day)~malegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="male") %>% dplyr::rename(quintgroup=malegroup)
poverty_mort <- aggregate(cbind(covidmortality,covidmortality7day)~povertygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="poverty") %>% dplyr::rename(quintgroup=povertygroup)
age65over_mort <- aggregate(cbind(covidmortality,covidmortality7day)~age65overgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="age65over") %>% dplyr::rename(quintgroup=age65overgroup)
minority_mort <- aggregate(cbind(covidmortality,covidmortality7day)~minoritygroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="minority") %>% dplyr::rename(quintgroup=minoritygroup)
groupquater_mort <- aggregate(cbind(covidmortality,covidmortality7day)~groupquatergroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="groupquarter") %>% dplyr::rename(quintgroup=groupquatergroup)
hispanic_mort <- aggregate(cbind(covidmortality,covidmortality7day)~hispanicgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hispanic") %>% dplyr::rename(quintgroup=hispanicgroup)
natives_mort <- aggregate(cbind(covidmortality,covidmortality7day)~nativesgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="natives") %>% dplyr::rename(quintgroup=nativesgroup)
urbanrural_mort <- aggregate(cbind(covidmortality,covidmortality7day)~X_013_Urbanization,ourraw,FUN="mean") %>% filter(!X_013_Urbanization=="") %>% mutate(quintileVar="urbanrural",lbl = X_013_Urbanization) %>% dplyr::rename(quintgroup=X_013_Urbanization)
hhincome_mort <- aggregate(cbind(covidmortality,covidmortality7day)~hhincomegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="hhincome") %>% dplyr::rename(quintgroup=hhincomegroup)
region_mort <- aggregate(cbind(covidmortality,covidmortality7day)~region,ourraw,FUN="mean") %>% filter(!region=="") %>% mutate(quintileVar="region",lbl = region) %>% dplyr::rename(quintgroup=region)
cvi_mort <- aggregate(cbind(covidmortality,covidmortality7day)~cvigroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CVI") %>% dplyr::rename(quintgroup=cvigroup)
residseg_mort <- aggregate(cbind(covidmortality,covidmortality7day)~residseggroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="resSeg") %>% dplyr::rename(quintgroup=residseggroup)

anycondition_mort <- aggregate(cbind(covidmortality,covidmortality7day)~anyconditiongroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="any condition") %>% dplyr::rename(quintgroup=anyconditiongroup)
diabetes2_mort <- aggregate(cbind(covidmortality,covidmortality7day)~diabetes2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="diabetes 2018") %>% dplyr::rename(quintgroup=diabetes2group)
obesity2_mort <- aggregate(cbind(covidmortality,covidmortality7day)~obesity2group,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="obesity 2018") %>% dplyr::rename(quintgroup=obesity2group)
heartdisease_mort <- aggregate(cbind(covidmortality,covidmortality7day)~heartdiseasegroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="heart disease") %>% dplyr::rename(quintgroup=heartdiseasegroup)
copd_mort <- aggregate(cbind(covidmortality,covidmortality7day)~copdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="COPD") %>% dplyr::rename(quintgroup=copdgroup)
ckd_mort <- aggregate(cbind(covidmortality,covidmortality7day)~ckdgroup,ourraw,FUN="mean") %>% mutate(lbl = c("Very Low","Low","Moderate","High","Very High"),quintileVar="CKD") %>% dplyr::rename(quintgroup=ckdgroup)


outcomes_mort_pre <- full_join(black_mort,male_mort) %>% full_join(poverty_mort) %>% full_join(age65over_mort) %>%
  full_join(minority_mort) %>% full_join(groupquater_mort) %>% full_join(hispanic_mort) %>%
  full_join(natives_mort) %>% full_join(urbanrural_mort)  %>% full_join(hhincome_mort) %>% full_join(region_mort) %>% full_join(cvi_mort) %>%
  full_join(residseg_mort) %>% full_join(anycondition_mort) %>% full_join(diabetes2_mort) %>% full_join(obesity2_mort) %>%
  full_join(heartdisease_mort) %>% full_join(copd_mort) %>% full_join(ckd_mort) %>%
  dplyr::select(covidmortality7day,everything())


outcomes_mort <- gather(outcomes_mort_pre,
                        key="variable",
                        value="measure",
                        c("covidmortality","covidmortality7day")) %>% dplyr::select(quintileVar,quintgroup,lbl,variable,measure)


rm(outcomes_mort_pre,ourraw,black_mort,male_mort,poverty_mort,age65over_mort,minority_mort,groupquater_mort,diabetes_mort,hispanic_mort,natives_mort,urbanrural_mort,obesity_mort,hhincome_mort,region_mort,cvi_mort,residseg_mort)


################### ***JOINING CASERATE AND COVID MORTALITY PLOTS DATA*** ###############

merged <- full_join(outcomes,outcomes_mort)

merged$quintileVar[merged$quintileVa=="CVI"] <- "ccvi"
merged$quintileVar[merged$quintileVa=="any condition"] <- "anycondition"
merged$quintileVar[merged$quintileVa=="diabetes 2018"] <- "diabetes"
merged$quintileVar[merged$quintileVa=="obesity 2018"] <- "obesity"
merged$quintileVar[merged$quintileVa=="heart disease"] <- "heartdisease"
merged$quintileVar[merged$quintileVa=="COPD"] <- "copd"
merged$quintileVar[merged$quintileVa=="CKD"] <- "ckd"

################### ***UNLOCK TO EXPORT*** ###############

setwd(onedrive)
write.csv(merged,"./Data/Upload/lastbarcharts_merged.csv",na="",row.names=FALSE)


rm(outcomes_mort,outcomes)


########### INDICES DATA FROM OXFORD ############

oxford_indices <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv") %>%
  filter(CountryCode=="USA") %>%
  transform(date = as.Date(as.character(Date), "%Y%m%d")) %>%
  dplyr::select(date,CountryName,RegionName,StringencyIndex,GovernmentResponseIndex,ContainmentHealthIndex,EconomicSupportIndex)


states_indices <- oxford_indices %>%
  filter(!is.na(StringencyIndex),!is.na(GovernmentResponseIndex),!is.na(ContainmentHealthIndex),!is.na(EconomicSupportIndex)) %>%
  # group_by(RegionName) %>%
  # slice(n()) %>%
  dplyr::rename(statename=RegionName,country=CountryName,stringencyIndex=StringencyIndex,governmentResponseIndex=GovernmentResponseIndex,containmentHealthIndex=ContainmentHealthIndex,economicSupportIndex=EconomicSupportIndex) %>%
  mutate(nation=ifelse(statename=="",1,NA)) %>%
  dplyr::select(-country) %>%
  dplyr::select(date,nation,statename,everything())

#calling nationalraw

setwd(local)
ourraw <- read.csv("./nationalraw.csv")
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_2013_Urbanization"] <- "_013_Urbanization"
names(final_merged_covidtimeseries)[names(final_merged_covidtimeseries) == "X_2013_Urbanization_Code"] <- "_013_Urbanization_Code"



indices_data <- join(ourraw,states_indices)

indices_data<-indices_data %>%
  dplyr::select(-c("urbanrural"))

names(indices_data )[names(indices_data ) == "X_2013_Urbanization"] <- "urbanrural_text"
names(indices_data )[names(indices_data ) == "X_2013_Urbanization_Code"] <- "urbanrural"
names(indices_data )[names(indices_data ) == "RS_blackwhite"] <- "resSeg"
names(indices_data )[names(indices_data ) == "region"] <- "region_text"
names(indices_data )[names(indices_data ) == "region_Code"] <- "region"
names(indices_data )[names(indices_data ) == "diabetesPrevalence"] <- "diabetes"
names(indices_data )[names(indices_data ) == "heartdiseasePrevalence"] <- "heartdisease"
names(indices_data )[names(indices_data ) == "ckdPrevalence"] <- "ckd"
names(indices_data )[names(indices_data ) == "copdPrevalence"] <- "copd"
names(indices_data )[names(indices_data ) == "anyconditionPrevalence"] <- "anycondition"
names(indices_data )[names(indices_data ) == "obesityPrevalence"] <- "obesity"


anycondition_state<-aggregate(anycondition_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
obesity_state<-aggregate(Obesity_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
heartdisease_state<-aggregate(`Heart disease_prevalence` ~ STATE_NAME, Chronic_Conditions, FUN="mean")
COPD_state<-aggregate(COPD_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
diabetes_state<-aggregate(diabetes_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
CKD_state<-aggregate(CKD_prevalence ~ STATE_NAME, Chronic_Conditions, FUN="mean")
anycondition_num<-aggregate(anycondition_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
obesity_num<-aggregate(Obesity_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
heartdisease_num<-aggregate(`Heart disease_number` ~ STATE_NAME, Chronic_Conditions, FUN="mean")
COPD_num<-aggregate(COPD_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
diabetes_num<-aggregate(diabetes_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
CKD_num<-aggregate(CKD_number ~ STATE_NAME, Chronic_Conditions, FUN="mean")
resSeg<-subset(indices_data,!countyname%in%"")
resSeg_state<-aggregate(resSeg ~ state, resSeg, FUN="mean")
ccvi_state<-aggregate(ccvi ~ state, resSeg, FUN="mean")



chronic_state<-dplyr::left_join(x=anycondition_state,y=obesity_state)
chronic_state<-dplyr::left_join(x=chronic_state,heartdisease_state)
chronic_state<-dplyr::left_join(x=chronic_state,COPD_state)
chronic_state<-dplyr::left_join(x=chronic_state,diabetes_state)
chronic_state<-dplyr::left_join(x=chronic_state,CKD_state)
chronic_state<-dplyr::left_join(x=chronic_state,anycondition_num)
chronic_state<-dplyr::left_join(x=chronic_state,obesity_num)
chronic_state<-dplyr::left_join(x=chronic_state,heartdisease_num)
chronic_state<-dplyr::left_join(x=chronic_state,COPD_num)
chronic_state<-dplyr::left_join(x=chronic_state,diabetes_num)
chronic_state<-dplyr::left_join(x=chronic_state,CKD_num)


names(chronic_state)[names(chronic_state) == "STATE_NAME"] <- "statename"
names(chronic_state)[names(chronic_state) == "diabetes_prevalence"] <- "diabetes"
names(chronic_state)[names(chronic_state) == "Heart disease_prevalence"] <- "heartdisease"
names(chronic_state)[names(chronic_state) == "CKD_prevalence"] <- "ckd"
names(chronic_state )[names(chronic_state) == "COPD_prevalence"] <- "copd"
names(chronic_state)[names(chronic_state) == "anycondition_prevalence"] <- "anycondition"
names(chronic_state)[names(chronic_state) == "Obesity_prevalence"] <- "obesity"

names(chronic_state)[names(chronic_state) == "diabetes_number"] <- "diabetesNumber"
names(chronic_state)[names(chronic_state) == "Heart disease_number"] <- "heartdiseaseNumber"
names(chronic_state)[names(chronic_state) == "CKD_number"] <- "ckdNumber"
names(chronic_state )[names(chronic_state) == "COPD_number"] <- "copdNumber"
names(chronic_state)[names(chronic_state) == "anycondition_number"] <- "anyconditionNumber"
names(chronic_state)[names(chronic_state) == "Obesity_number"] <- "obesityNumber"

chronic_state$state<-fips(chronic_state$statename)
chronic_state$state<-str_remove(chronic_state$state, "^0+")
chronic_state$state<-as.numeric(chronic_state$state)
chronic_state<-dplyr::left_join(x=chronic_state,resSeg_state)
chronic_state<-dplyr::left_join(x=chronic_state,ccvi_state)
chronic_state<-chronic_state[,-14]
variable<-c("diabetes.y","heartdisease.y","ckd.y","copd.y","anycondition.y","obesity.y","diabetesNumber.y","heartdiseaseNumber.y","ckdNumber.y","copdNumber.y","anyconditionNumber.y","obesityNumber.y","resSeg.y","ccvi.y")
indices_data<-indices_data %>% subset(., select=which(!duplicated(names(.)))) 

indices_data$original_order <- 1:nrow(indices_data)

indices_data1 <- merge(indices_data, chronic_state, by = "statename", all = TRUE)

indices_data1<- indices_data1[order(indices_data1$original_order),]
indices_data1<-indices_data1[,c(2:4,1,5:110)]
indices_data=indices_data1

indices_data$anycondition.x <- ifelse(is.na(indices_data$anycondition.x), indices_data$anycondition.y, indices_data$anycondition.x)
names(indices_data)[names(indices_data) == "anycondition.x"] <- "anycondition"
indices_data$anycondition=round(indices_data$anycondition, 2)


indices_data$diabetes.x <- ifelse(is.na(indices_data$diabetes.x), indices_data$diabetes.y, indices_data$diabetes.x)
names(indices_data)[names(indices_data) == "diabetes.x"] <- "diabetes"
indices_data$diabetes=round(indices_data$diabetes, 2)


indices_data$heartdisease.x <- ifelse(is.na(indices_data$heartdisease.x), indices_data$heartdisease.y, indices_data$heartdisease.x)
names(indices_data)[names(indices_data) == "heartdisease.x"] <- "heartdisease"
indices_data$heartdisease=round(indices_data$heartdisease,2)

indices_data$ckd.x <- ifelse(is.na(indices_data$ckd.x), indices_data$ckd.y, indices_data$ckd.x)
names(indices_data)[names(indices_data) == "ckd.x"] <- "ckd"
indices_data$ckd=round(indices_data$ckd,2)

indices_data$copd.x <- ifelse(is.na(indices_data$copd.x), indices_data$copd.y, indices_data$copd.x)
names(indices_data)[names(indices_data) == "copd.x"] <- "copd"
indices_data$copd=round(indices_data$copd,2)

indices_data$obesity.x <- ifelse(is.na(indices_data$obesity.x), indices_data$obesity.y, indices_data$obesity.x)
names(indices_data)[names(indices_data) == "obesity.x"] <- "obesity"
indices_data$obesity=round(indices_data$obesity,2)

indices_data$obesityNumber.x <- ifelse(is.na(indices_data$obesityNumber.x), indices_data$obesityNumber.y, indices_data$obesityNumber.x)
names(indices_data)[names(indices_data) == "obesityNumber.x"] <- "obesityNumber"
indices_data$obesityNumber=round(indices_data$obesityNumber,2)

indices_data$diabetesNumber.x <- ifelse(is.na(indices_data$diabetesNumber.x), indices_data$diabetesNumber.y, indices_data$diabetesNumber.x)
names(indices_data)[names(indices_data) == "diabetesNumber.x"] <- "diabetesNumber"
indices_data$diabetesNumber=round(indices_data$diabetesNumber, 2)

indices_data$heartdiseaseNumber.x <- ifelse(is.na(indices_data$heartdiseaseNumber.x), indices_data$heartdiseaseNumber.y, indices_data$heartdiseaseNumber.x)
names(indices_data)[names(indices_data) == "heartdiseaseNumber.x"] <- "heartdiseaseNumber"
indices_data$heartdiseaseNumber=round(indices_data$heartdiseaseNumber,2)

indices_data$ckdNumber.x <- ifelse(is.na(indices_data$ckdNumber.x), indices_data$ckdNumber.y, indices_data$ckdNumber.x)
names(indices_data)[names(indices_data) == "ckdNumber.x"] <- "ckdNumber"
indices_data$ckdNumber=round(indices_data$ckdNumber,2)

indices_data$copdNumber.x <- ifelse(is.na(indices_data$copdNumber.x), indices_data$copdNumber.y, indices_data$copdNumber.x)
names(indices_data)[names(indices_data) == "copdNumber.x"] <- "copdNumber"
indices_data$copdNumber=round(indices_data$copdNumber,2)

indices_data$anyconditionNumber.x <- ifelse(is.na(indices_data$anyconditionNumber.x), indices_data$anyconditionNumber.y, indices_data$anyconditionNumber.x)
names(indices_data)[names(indices_data) == "anyconditionNumber.x"] <- "anyconditionNumber"
indices_data$anyconditionNumber=round(indices_data$anyconditionNumber, 2)

indices_data$resSeg.x <- ifelse(is.na(indices_data$resSeg.x), indices_data$resSeg.y, indices_data$resSeg.x)
names(indices_data)[names(indices_data) == "resSeg.x"] <- "resSeg"
indices_data$resSeg=round(indices_data$resSeg,2)

#indices_data$ccvi.x <- ifelse(is.na(indices_data$ccvi.x), indices_data$ccvi.y, indices_data$ccvi.x)
names(indices_data)[names(indices_data) == "ccvi.x"] <- "ccvi"
indices_data$ccvi=round(indices_data$ccvi,2)

#indices_data<-indices_data%>%dplyr::select(-c("diabetes.y","heartdisease.y","ckd.y","copd.y","anycondition.y","obesity.y","diabetesNumber.y","heartdiseaseNumber.y","ckdNumber.y","copdNumber.y","anyconditionNumber.y","obesityNumber.y","resSeg.y","ccvi.y"))
indices_data<-indices_data%>%dplyr::select(-c("diabetes.y","heartdisease.y","ckd.y","copd.y","anycondition.y","obesity.y","diabetesNumber.y","heartdiseaseNumber.y","ckdNumber.y","copdNumber.y","anyconditionNumber.y","obesityNumber.y","resSeg.y"))


#indices_data$resSeg

indices_data[which(indices_data$nation == 1),]$anycondition <- mean(chronic_state$anycondition)
indices_data[which(indices_data$nation == 1),]$obesity <- mean(chronic_state$obesity)
indices_data[which(indices_data$nation == 1),]$heartdisease <- mean(chronic_state$heartdisease)
indices_data[which(indices_data$nation == 1),]$copd <- mean(chronic_state$copd)
indices_data[which(indices_data$nation == 1),]$diabetes <- mean(chronic_state$diabetes)
indices_data[which(indices_data$nation == 1),]$ckd <- mean(chronic_state$ckd)
indices_data[which(indices_data$nation == 1),]$anyconditionNumber <- mean(chronic_state$anyconditionNumber )
indices_data[which(indices_data$nation == 1),]$obesityNumber  <- mean(chronic_state$obesityNumber )
indices_data[which(indices_data$nation == 1),]$heartdiseaseNumber  <- mean(chronic_state$heartdiseaseNumber )
indices_data[which(indices_data$nation == 1),]$copdNumber  <- mean(chronic_state$copdNumber )
indices_data[which(indices_data$nation == 1),]$diabetesNumber  <- mean(chronic_state$diabetesNumber )
indices_data[which(indices_data$nation == 1),]$ckdNumber  <- mean(chronic_state$ckdNumber )
indices_data[which(indices_data$nation == 1),]$resSeg  <- mean(resSeg_state$resSeg )
indices_data[which(indices_data$nation == 1),]$ccvi  <- mean(ccvi_state$ccvi )




##########Vaccination Data####################
#Series_Complete_Pop_Pct: Percent of people who are fully vaccinated (have second dose of a two-dose vaccine or one dose of a single-dose vaccine) based on the jurisdiction where recipient lives
#Administered_Dose1_Pop_Pct: Percent of population with at lease one dose based on the jurisdiction where recipient lives


library(RJSONIO)
library(RCurl)


VaccineTracker<-read.csv("/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackertimeseries.csv")
VaccineTracker<-VaccineTracker[,-c(29:31)]

########### TODAY'S VACCINE DATA ############
today<-getURL("https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_data")
casesdata <- jsonlite::fromJSON(today)[[2]] 

names(casesdata)[16]<-"percentVaccinatedDose1"
names(casesdata)[17]<-"percentVaccinatedDose2"
names(casesdata)[15]<-"Administered_Dose1"
names(casesdata)[28]<-"Administered_Dose2"
casesdata<-casesdata[,-c(6,18:27,29:30,39:46,48:50,51:72)]
casesdata<-casesdata[,-c(27:94)]

names(casesdata)[names(casesdata) == "LongName"] <- "statename"
casesdata<-casesdata[,-c(2,3)]
casesdata$statename[casesdata$statename== "New York State"] <-"New York"
keep<-c("statename","Population")
population<-final_merged_nationalraw[keep]
population<-subset(population,!statename%in%c(""))
casesdata<-dplyr::left_join(x = casesdata , y = population)
casesdata$FIPS<-fips(casesdata$statename)
casesdata<-subset(casesdata,!statename%in%"American Samoa"& !statename%in%"Bureau of Prisons"& !statename%in%"Dept of Defense"& !statename%in%"Federated States of Micronesia"& !statename%in%"Guam"& !statename%in%"Indian Health Svc"& !statename%in%"Marshall Islands"& !statename%in%"Northern Mariana Islands"& !statename%in%"Puerto Rico"& !statename%in%"Republic of Palau"& !statename%in%"Veterans Health"& !statename%in%"Long Term Care"& !statename%in%"Virgin Islands")
casesdata<-casesdata[!duplicated(casesdata),]
casesdata<-casesdata[,c(1:2,26,3:11,25,12:24)]

casesdata[is.na(casesdata)] <- -1
all<-casesdata[!(casesdata$statename=="United States"),]
casesdata$Population[casesdata$statename=="United States"]<-sum(all$Population)
casesdata<-casesdata[!duplicated(casesdata),]

########### MERGE TODAY WITH PREVIOUS VACCINE DATA (CUMULATIVE) ############
casesdata$AdministeredPartial<-casesdata$Administered_Dose1-casesdata$Series_Complete_Yes
casesdata$PercentAdministeredPartial<-casesdata$AdministeredPartial/casesdata$Census2019*100
casesdata$PercentAdministeredPartial<-round(casesdata$PercentAdministeredPartial,1)
casesdata$percentReceived<-casesdata$Doses_Administered/casesdata$Doses_Distributed
casesdata$percentReceived<-round(casesdata$percentReceived,1)

casesdata<-casesdata[,-c(17)]

a<-names(VaccineTracker)
b<-names(casesdata)
setdiff(b,a)
setdiff(a,b)

VaccineTracker<-VaccineTracker[,-29]
VaccineTracker1<-rbind(VaccineTracker,casesdata)
VaccineTracker1<-VaccineTracker1[
  with(VaccineTracker1, order(statename, Date)),
]


VaccineTracker1$FIPS[VaccineTracker1$FIPS==-1] <-"_nation"
VaccineTracker1$FIPS[VaccineTracker1$FIPS==""] <-"_nation"


detach("package:plyr", unload = TRUE)
VaccineTracker9<-VaccineTracker1%>%group_by(statename) %>%
  mutate(Dist_Per_100K_new = Dist_Per_100K - lag(Dist_Per_100K, default = 0))%>%
  mutate(Dist_Per_100K_new=ifelse (Dist_Per_100K_new<0,0,Dist_Per_100K_new))


VaccineTracker9<-VaccineTracker9[
  with(VaccineTracker9, order(statename, Date)),
]
VaccineTracker9$Dist_Per_100K_new=as.numeric(VaccineTracker9$Dist_Per_100K_new)



VaccineTracker0<-VaccineTracker9%>%group_by(statename) %>%
  mutate(Dist_new = Doses_Distributed - lag(Doses_Distributed, default = 0))%>%
  mutate(Dist_new=ifelse (Dist_new<0,0,Dist_new))

VaccineTracker0<-VaccineTracker0[
  with(VaccineTracker0, order(statename, Date)),
]
casesdata<-casesdata[
  with(casesdata, order(statename, Date)),
]


VaccineTracker0<-VaccineTracker0%>%group_by(statename) %>%
  mutate(distDate=ifelse(Dist_new==0,lag(Date),Date)) %>%
  mutate(Dist_Per_100K_new = ifelse (Dist_Per_100K_new==0,lag(Dist_Per_100K_new),Dist_Per_100K_new))%>%
  mutate(Dist_new = ifelse (Dist_new==0,lag(Dist_new),Dist_new))

VaccineTracker0<-VaccineTracker0%>%group_by(statename) %>%
  mutate(distDate=ifelse(Dist_new==0,lag(distDate),distDate)) %>%
  mutate(Dist_Per_100K_new = ifelse (Dist_Per_100K_new==0,lag(Dist_Per_100K_new),Dist_Per_100K_new))%>%
  mutate(Dist_new = ifelse (Dist_new==0,lag(Dist_new),Dist_new))
VaccineTracker0<-VaccineTracker0%>%group_by(statename) %>%
  mutate(distDate=ifelse(Dist_new==0,lag(distDate),distDate)) %>%
  mutate(Dist_Per_100K_new = ifelse (Dist_Per_100K_new==0,lag(Dist_Per_100K_new),Dist_Per_100K_new))%>%
  mutate(Dist_new = ifelse (Dist_new==0,lag(Dist_new),Dist_new))


VaccineTracker0$Date<-as.Date(VaccineTracker0$Date)
VaccineTracker0<-VaccineTracker0[!duplicated(VaccineTracker0),]


VaccineTracker0<- VaccineTracker0 %>% 
  distinct(Date,statename, .keep_all = TRUE)



casesdata$Dist_Per_100K_new<-VaccineTracker0$Dist_Per_100K_new[VaccineTracker0$Date==Sys.Date()]

casesdata$Dist_new<-VaccineTracker0$Dist_new[VaccineTracker0$Date==Sys.Date()]
casesdata$distDate<-VaccineTracker0$distDate[VaccineTracker0$Date==Sys.Date()]

VaccineTracker0$FIPS<-str_remove(VaccineTracker0$FIPS, "^0+")
VaccineTracker0$PercentAdministeredPartial<-round(VaccineTracker0$PercentAdministeredPartial,1)
VaccineTracker0$percentReceived<-round(VaccineTracker0$percentReceived,1)


raw<- jsonlite::fromJSON(today)[[2]] 
raw<-raw[,c(4,18:30,38:46,48:68)]
names(raw)[1]<-"statename"
raw$statename[raw$statename== "New York State"] <-"New York"
casesdata1<-left_join(casesdata,raw)
casesdata1[is.na(casesdata1)] <- -1

VaccineTracker_x<-VaccineTracker0%>%
  dplyr::group_by(statename)%>%
  dplyr::mutate(percentVaccinatedDose2_avg7 = lag(percentVaccinatedDose2, 7, fill = NA))

casesdata1$percentVaccinatedDose2_avg7<-VaccineTracker_x$percentVaccinatedDose2_avg7[VaccineTracker_x$Date=="2021-08-06"]
casesdata1$percentVaccinatedDose2_avg7<-round(casesdata1$percentVaccinatedDose2_avg7,2)

VaccineTracker0$percentVaccinatedDose2_avg7<-VaccineTracker_x$percentVaccinatedDose2_avg7
VaccineTracker0$percentVaccinatedDose2_avg7<-ifelse(is.na(VaccineTracker0$percentVaccinatedDose2_avg7),-999,VaccineTracker0$percentVaccinatedDose2_avg7)
VaccineTracker0$percentVaccinatedDose2_avg7<-round(VaccineTracker0$percentVaccinatedDose2_avg7,2)


write.csv(VaccineTracker0,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackertimeseries.csv",row.names=FALSE,na="")
write.csv(casesdata1,"/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard/Data/Upload/VaccineTrackerstatic.csv",row.names=FALSE,na="")

###############Vaccineation County Data and Merge it to nationalraw

vaccination_county_link <- "https://covid.cdc.gov/covid-data-tracker/COVIDData/getAjaxData?id=vaccination_county_condensed_data"
vaccination_county <- jsonlite::fromJSON(vaccination_county_link)[2][[1]]


date <- unique(vaccination_county$Date)
path_c19dashboard_shared_folder="/Users/air/OneDrive - Emory University/CovidHealthEquityDashboard"
write.csv(vaccination_county,paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/CDC_Covid Data Tracker_County Vaccination_",date,".csv"),row.names = FALSE)

f = list.files(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations"))
f = f[regexpr("CDC_Covid Data Tracker_County Vaccination_",f)>0]


vaccination_ts <- map_dfr(f,.f=function(x){
  read_csv(paste0(path_c19dashboard_shared_folder,"/Data/Raw/Vaccinations/",x))%>% mutate_at(vars(contains("Series")),~as.numeric(.))
})


countynames <- read.csv(paste0(path_c19dashboard_shared_folder,"/Data/Upload/nationalraw.csv")) %>% 
  distinct(state,county,countyname)


names(vaccination_county)[1]<-"Date"
names(vaccination_county)[3]<-"state_name"
names(vaccination_county)[5]<-"county_name"

vaccination_county$countyname<- paste(vaccination_county$county_name, vaccination_county$StateAbbr, sep = ", ")
library(usmap)
a=countypop
vaccination_county$countyname[vaccination_county$state_name=="Alaska"]<-paste(a$county[a$abbr=="AK"],vaccination_county$StateAbbr[vaccination_county$state_name=="Alaska"], sep = ", ")

vaccination_county$countyname<-trimws(vaccination_county$countyname, "r")

indices_data$countyname
merge<-left_join(indices_data,vaccination_county)    

merge1<-merge[-c(96:101)]
names(merge1)[96]<-"seriesComplete18Plus"
names(merge1)[97]<-"seriesComplete18PlusPopPct"
names(merge1)[98]<-"seriesComplete65Plus"
names(merge1)[99]<-"seriesComplete65PlusPopPct"
names(merge1)[100]<-"seriesCompleteYes"
names(merge1)[101]<-"seriesCompletePopPct"
names(merge1)[102]<-"CompletenessPct"
merge1<-merge1[-c(103)]
names(merge1)[103]<-"seriesComplete12Plus"
names(merge1)[104]<-"seriesComplete12PlusPopPct"
library(openxlsx)
texas<-openxlsx::read.xlsx("https://www.dshs.texas.gov/immunize/covid19/COVID-19-Vaccine-Data-by-County.xls",sheet=2)
texas<-texas[-c(1:3,258:259),]
names(texas)[1]<-"countyname"
names(texas)[6]<-"seriesCompleteYes"
texas$seriesCompleteYes<-as.numeric(texas$seriesCompleteYes)
texas$`Population,.16+`<-as.numeric(texas$`Population,.16+`)
texas$seriesCompletePopPct<-texas$seriesCompleteYes/texas$`Population,.16+`*100
texas$seriesCompletePopPct<-round(texas$seriesCompletePopPct,1) 
texas$People.Vaccinated.with.at.least.One.Dose<-as.numeric(texas$People.Vaccinated.with.at.least.One.Dose)
texas$CompletenessPct<-texas$seriesCompleteYes/texas$People.Vaccinated.with.at.least.One.Dose*100
texas$CompletenessPct<-round(texas$CompletenessPct,1)
texas$countyname<-paste(texas$countyname, "County, TX",sep=" ")
texas<-texas[-c(2:5,7:12)]


merge2<-left_join(merge1,texas,by = c("countyname"))

merge2$seriesCompleteYes.x<-ifelse(is.na(merge2$seriesCompleteYes.x),merge2$seriesCompleteYes.y,merge2$seriesCompleteYes.x)
merge2$seriesCompletePopPct.x<-ifelse(is.na(merge2$seriesCompletePopPct.x),merge2$seriesCompletePopPct.y,merge2$seriesCompletePopPct.x)
merge2$CompletenessPct.x<-ifelse(is.na(merge2$CompletenessPct.x),merge2$CompletenessPct.y,merge2$CompletenessPct.x)
merge2<-merge2[-c(118:129)]
names(merge2)[names(merge2) == "seriesCompleteYes.x"] <- "seriesCompleteYes"
names(merge2)[names(merge2) == "seriesCompletePopPct.x"] <- "seriesCompletePopPct"
names(merge2)[names(merge2) == "CompletenessPct.x"] <- "CompletenessPct"
merge2<-merge2[-c(106:117)]
variable<-c("seriesComplete18Plus","seriesComplete18PlusPopPct","seriesComplete65Plus","seriesComplete65PlusPopPct","seriesCompleteYes","seriesCompletePopPct","CompletenessPct","seriesComplete12Plus","seriesComplete12PlusPopPct")
merge2[, variable][is.na(merge2[, variable])] <- -1


merge_county<-subset(merge2,!is.na(county))
merge_state<-subset(merge2,is.na(county)&is.na(nation))
merge_nation<-subset(merge2,nation==1)
merge_county1=merge_county
merge_county1$seriesComplete18PlusPopPct<-ifelse(merge_county$seriesComplete18PlusPopPct==-1,0,merge_county$seriesComplete18PlusPopPct)
merge_county1$seriesComplete65PlusPopPct<-ifelse(merge_county$seriesComplete65PlusPopPct==-1,0,merge_county$seriesComplete65PlusPopPct)
merge_county1$seriesComplete12PlusPopPct<-ifelse(merge_county$seriesComplete12PlusPopPct==-1,0,merge_county$seriesComplete12PlusPopPct)

x<-aggregate(merge_county1$seriesComplete18PlusPopPct,by=list(state=merge_county1$state),FUN = "mean")
y<-aggregate(merge_county1$seriesComplete65PlusPopPct,by=list(state=merge_county1$state),FUN ="mean")
m<-aggregate(merge_county1$seriesComplete12PlusPopPct,by=list(state=merge_county1$state),FUN = "mean")

merge_state <- merge_state[-52,]

merge_state$seriesComplete18PlusPopPct<-x$`mean.merge_county1$seriesComplete18PlusPopPct`
merge_state$seriesComplete65PlusPopPct<-y$`mean.merge_county1$seriesComplete65PlusPopPct`
merge_state$seriesComplete12PlusPopPct<-m$`mean.merge_county1$seriesComplete12PlusPopPct`
merge_nation$seriesComplete18PlusPopPct<-mean(merge_state$seriesComplete18PlusPopPct)
merge_nation$seriesComplete65PlusPopPct<-mean(merge_state$seriesComplete65PlusPopPct)
merge_nation$seriesComplete12PlusPopPct<-mean(merge_state$seriesComplete12PlusPopPct)
merge_state$seriesComplete18PlusPopPct<-ifelse(merge_state$seriesComplete18PlusPopPct==0,-1,merge_state$seriesComplete18PlusPopPct)
merge_state$seriesComplete65PlusPopPct<-ifelse(merge_state$seriesComplete65PlusPopPct==0,-1,merge_state$seriesComplete65PlusPopPct)
merge_state$seriesComplete12PlusPopPct<-ifelse(merge_state$seriesComplete12PlusPopPct==0,-1,merge_state$seriesComplete12PlusPopPct)
merge3<-rbind(merge_state,merge_county,merge_nation)


z<-merge3[match(rownames(merge2), rownames(merge3)),]
names(z)[29]<-"urbanrural_text"
names(z)[30]<-"urbanrural"
z <- z[,-9]
z$urbanrural_text[z$urbanrural_text=="Small Metro"]<-"Remote rural areas"
z$urbanrural_text[z$urbanrural_text=="NonCore (Nonmetro)"]<-"Rural areas near cities"
z$urbanrural_text[z$urbanrural_text=="Micropolitan (Nonmetro)"]<-"Small cities"
z$urbanrural_text[z$urbanrural_text=="Medium Metro"]<-"Small suburbs"
z$urbanrural_text[z$urbanrural_text=="Large Fringe Metro"]<-"Large suburbs"
z$urbanrural_text[z$urbanrural_text=="Large Central Metro"]<-"Inner city"

setwd(onedrive)

write.csv(z,"./Data/Upload/nationalraw.csv",row.names=FALSE,na="")


################Check zero cases/deaths, if so then replace zero with previous non-zero cases/deaths################
#trial <- read.csv("./Data/Upload/covidtimeseries.csv")
modify <- trial
modify_state <- subset(modify,!statename=="")
modify_county <- subset(modify,!county=="")
modify_nation <-subset(modify,nation==1)



modify_state1 <- modify_state %>% 
  dplyr::group_by(statename) %>% 
  dplyr::mutate(dailycases = replace(dailycases,cumsum(dailycases !=0) >0 & dailycases == 0, NA)) %>% 
  fill(dailycases)

modify_state1 <- modify_state1 %>% 
  dplyr::group_by(statename) %>% 
  dplyr::mutate(dailydeaths = replace(dailydeaths,cumsum(dailydeaths !=0) >0 & dailydeaths == 0, NA)) %>% 
  fill(dailydeaths)


modify_state1<-modify_state1[
  with(modify_state1, order(state,county)),
]


modify_county1 <- modify_county %>% 
  dplyr::group_by(countyname) %>% 
  dplyr::mutate(dailycases = replace(dailycases,cumsum(dailycases !=0) >0 & dailycases == 0, NA)) %>% 
  fill(dailycases)

modify_county1 <- modify_county1 %>% 
  dplyr::group_by(countyname) %>% 
  dplyr::mutate(dailydeaths = replace(dailydeaths,cumsum(dailydeaths !=0) >0 & dailydeaths == 0, NA)) %>% 
  fill(dailydeaths)

modify_county1<-modify_county1[
  with(modify_county1, order(state,county)),
]



final <- rbind(modify_state1,modify_county1,modify_nation)

x <- final[order(match(trial$county,final$county)), ]

x<-x[
  with(x, order(state,county,date)),
]

x_nation <- subset(x,nation==1)
x_nonnation <-subset(x,!nation==1)

x_nonnation<-x_nonnation[
  with(x_nonnation, order(state,county,date)),
]
x1 <- rbind(x_nonnation,x_nation)


setwd(onedrive)
write.csv(x1,"./Data/Upload/covidtimeseries.csv", na="", row.names=F)



