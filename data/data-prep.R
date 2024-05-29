### data-prep.R ###
### Cleaning and preparing data-raw for analyses ###
### Lee Kennedy-Shaffer ###
### May 24, 2024 ###

require(tidyverse)
require(readxl)
require(haven)

proj_wd <- getwd()

## cholera data ##
### Run Coleman's Snow_ReadData.Rmd file to get regdata table ###
setwd("data-raw/cholera/")
source("Snow_ReadData.R")
setwd(proj_wd)
### Save regdata table in clean data folder: ###
save(regdata, file="data/cholera.Rda")


## zika data ##
### Read in zika_Table2.tab data
zika <- read_table(file="data-raw/zika/zika_Table2.tab",
                    col_names=TRUE)

### Fix 9 observations that should have missing BirthSchool
zika_fix <- zika %>% dplyr::filter(is.na(Pop)) %>% dplyr::select(-Pop) %>%
  dplyr::rename(Pop=interaction,
                interaction=year, 
                year=trt, 
                trt=Rate,
                Rate=BirthSchool) %>%
  mutate(BirthSchool=NA)
### Merge these observations back in, and create factor variable for State and log-rate variable
zika_full <- zika %>% dplyr::filter(!is.na(Pop)) %>% dplyr::add_row(zika_fix) %>%
  mutate(State=factor(x=as.numeric(substr(as.character(Code),1,2)),
                      levels=c(26,43), labels=c("Pernambuco","Rio Grande do Sul")),
         LogRate=log(Rate))
### Read in Births_Fig2.dta and create factor variable for State and log-births variable
zika_summ <- read_dta("data-raw/zika/Births_Fig2.dta") %>%
  mutate(State=factor(x=as.numeric(State), 
                      levels=c(26,43), labels=c("Pernambuco","Rio Grande do Sul")),
         LogRate=log(Rate),
         LogBirths=log(LiveBirths))
### Save output in clean data folder:
save(list=c("zika_full","zika_summ"), file="data/zika.Rda")


## mandate data ##
### Read in dates, vaccine data, and population data
Dates <- read_xlsx(path="data-raw/mandate/MandateDates.xlsx", sheet="Ballotpedia") %>%
  mutate(Mandate_Start=as.Date(`Start Date`)) %>% 
  dplyr::select(-c(`Start Date`))
Vax <- read_csv(file="data-raw/mandate/COVID-19_Vaccinations_in_the_United_States_Jurisdiction_20240508.csv") %>%
  mutate(DateT=mdy(Date))
### Territories/regions to exclude (non-states and VA2 and WV-too many corrections):
Exclude_Locs <- c("AS","BP2","DC","DD2","FM","GU","IH2","LTC","MP","PR","PW","RP","US","VA2","VI","WV")
### Exclusions and pull key variables:
Vax_rates <- Vax %>% dplyr::filter(!(Location %in% Exclude_Locs)) %>% 
  rename(SCP=Series_Complete_18PlusPop_Pct,
         D1P=Administered_Dose1_Recip_18PlusPop_Pct) %>%
  mutate(Year=year(DateT),Month=month(DateT)) %>%
  dplyr::select(Location,Year,Month,MMWR_week,DateT,SCP,D1P) %>%
  rename(Date=DateT,State=Location) %>%
  mutate(MMWR_year=if_else(MMWR_week==1 & Month==12,Year+1,
                           if_else(MMWR_week > 50 & Month==1,Year-1,Year))) %>%
  arrange(State,MMWR_year,MMWR_week,Date)
### Summarize to Weekly Level
Vax_wk_summ <- Vax_rates %>% group_by(State,MMWR_year,MMWR_week) %>%
  summarize(Start_Date=min(Date), End_Date=max(Date),
            SCP=max(SCP),D1P=max(D1P)) %>% ungroup()

### Compute weekly vaccination rates and add in mandate date info:
Vax_weekly <- Vax_wk_summ %>%
  mutate(MMWR_week_prior=if_else(MMWR_week==1,if_else(MMWR_year==2021,53,52),MMWR_week-1),
         MMWR_year_prior=if_else(MMWR_week==1,MMWR_year-1,MMWR_year)) %>%
  left_join(Vax_wk_summ %>% dplyr::rename(MMWR_week_prior=MMWR_week,MMWR_year_prior=MMWR_year,
                                          SCP_prior=SCP,D1P_prior=D1P) %>%
              dplyr::select(-c(Start_Date,End_Date)),
            by=join_by(State,MMWR_year_prior,MMWR_week_prior)) %>%
  dplyr::select(-c(MMWR_year_prior,MMWR_week_prior)) %>%
  dplyr::filter(MMWR_year > 2020) %>%
  mutate(SCP_diff=SCP-SCP_prior,D1P_diff=D1P-D1P_prior) %>%
  left_join(Dates, by="State") %>%
  mutate(Yr_Wk=paste0(MMWR_year,"_",formatC(MMWR_week, width=2, flag="0")),
         ever_mandate=!is.na(Mandate_Start),
         mandate=if_else(!ever_mandate,FALSE,
                         if_else(Mandate_Start<=End_Date,TRUE,FALSE)),
         Wks_mandate=ifelse(mandate==0,0,ceiling((End_Date-Mandate_Start+1)/7)),
         LeadLag=ifelse(is.na(Mandate_Start),NA,ceiling((End_Date-Mandate_Start+1)/7)))
### Save output in clean data folder:
save(Vax_weekly, file="data/mandate.Rda")


## lottery data ##
### Note that raw data files are not on the Epi-QEs github page, but are available at:
### https://doi.org/10.7910/DVN/K1XX02 and https://doi.org/10.7910/DVN/QYXN9L
### Load and save Fuller data:
load("data-raw/lottery/synth_data_clean.RData")
save(list=c("lotteries","state_vaccines","untreated_states"),
     file="data/lottery_fuller.Rda")
### Load Lang data:
lang_0624 <- readRDS("data-raw/lottery/weekly_data_2021-06-24.rds")
lang_0912 <- readRDS("data-raw/lottery/weekly_data_2021-09-12.rds")
lang_ann_dates <- read_csv("data-raw/lottery/lottery_announce_dates.csv") %>%
  mutate(state=str_trim(state),
         lott_date=as.Date(lottery_announce_date, format="%m/%d/%y"),
         lott_week=1+floor(as.numeric(lott_date-ymd("2021-01-04"))/7))
### Add states' lottery info to data sets:
lang_0624 <- lang_0624 %>% left_join(lang_ann_dates %>% 
                                       dplyr::select(state,lott_date,lott_week), 
                                     by=join_by(state)) %>%
  mutate(lottery=if_else(is.na(lott_date),FALSE,TRUE),
         rel_week=ifelse(is.na(lott_date),NA,week-lott_week),
         type=factor(if_else(state=="OH","Ohio",
                             if_else(lottery,"Other Lottery State",
                                     "Non-Lottery State")),
                     levels=c("Ohio","Other Lottery State","Non-Lottery State")),
         type2=factor(if_else(state=="OH","Ohio",
                              if_else(state=="NM","New Mexico",
                                      if_else(state=="ME","Maine",
                                              if_else(lottery,"Other Lottery State",
                                                      "Non-Lottery State")))),
                      levels=c("Ohio","New Mexico","Maine",
                               "Other Lottery State","Non-Lottery State")))
lang_0912 <- lang_0912 %>% left_join(lang_ann_dates %>% 
                                       dplyr::select(state,lott_date,lott_week), 
                                     by=join_by(state)) %>%
  mutate(lottery=if_else(is.na(lott_date),FALSE,TRUE),
         rel_week=ifelse(is.na(lott_date),NA,week-lott_week),
         type=factor(if_else(state=="OH","Ohio",
                             if_else(lottery,"Other Lottery State",
                                     "Non-Lottery State")),
                     levels=c("Ohio","Other Lottery State","Non-Lottery State")),
         type2=factor(if_else(state=="OH","Ohio",
                              if_else(state=="NM","New Mexico",
                                      if_else(state=="ME","Maine",
                                              if_else(lottery,"Other Lottery State",
                                                      "Non-Lottery State")))),
                      levels=c("Ohio","New Mexico","Maine",
                               "Other Lottery State","Non-Lottery State")))
### Save data:
save(list=c("lang_0624","lang_0912","lang_ann_dates"),
     file="data/lottery_lang.Rda")
### Note that Fuller et al. identify a lottery in Missouri that Lang et al. do not use; 
### otherwise, the announce dates differ only by a few days in some cases


## pcv data ##
### Note that raw data files are not on the Epi-QEs github page, but are available at:
### https://github.com/weinbergerlab/InterventionEvaluatR
### Load Bruhn et al. Chile data:
load("data-raw/pcv/data/pnas_chile.rda")
### Pre-processing of Chile data:
excl_cols <- colnames(pnas_chile[,apply(pnas_chile,2,FUN=function(x) mean(is.na(x))==1)])
pcv_chile <- pnas_chile %>% dplyr::select(!any_of(excl_cols)) %>%
  mutate(date=as.Date(date)) %>%
  pivot_longer(cols=!any_of(c("age_group","date","ach_noj")),
               names_to="series",
               values_to="value") %>%
  mutate(log_val=ifelse(is.na(value),NA,ifelse(value==0,log(0.5),log(value))),
         month=interval(as.Date("2001-01-01"),date) %/% months(1) + 1,
         month_ctr=interval(as.Date("2011-01-01"),date) %/% months(1),
         age_group=factor(age_group,
                          levels=c("92","8","3","4","5","6","7"),
                          labels=c("<24m","80+y","2-4y","5-17y","18-39y","40-64y","65-79y")),
         target=factor(series=="J12_18",
                       levels=c(TRUE,FALSE),
                       labels=c("Pneumonia","Control Series")))
### Save processed data:
save("pcv_chile",
     file="data/pcv_chile.Rda")
### Load Bruhn et al. Mexico data:
load("data-raw/pcv/data/pnas_mexico.rda")
### Pre-processing of Mexico data:
pnas_mexico_01 <- pnas_mexico %>% dplyr::filter(age_group==9)
excl_cols <- colnames(pnas_mexico_01[,apply(pnas_mexico_01,2,FUN=function(x) mean(is.na(x))==1)])
pcv_mexico_01 <- pnas_mexico_01 %>% dplyr::select(!any_of(excl_cols)) %>%
  mutate(date=as.Date(date)) %>%
  pivot_longer(cols=!any_of(c("age_group","date","ach_noj")),
               names_to="series",
               values_to="value") %>%
  mutate(log_val=ifelse(is.na(value),NA,ifelse(value==0,log(0.5),log(value))),
         month=interval(as.Date("2000-01-01"),date) %/% months(1) + 1,
         month_ctr=interval(as.Date("2006-01-01"),date) %/% months(1),
         age_group=factor(age_group, levels=c(9),
                          labels=c("<12 mo")),
         target=factor(series=="J12_18",
                       levels=c(TRUE,FALSE),
                       labels=c("Pneumonia","Control Series")))
### Save processed data:
save("pcv_mexico_01",
     file="data/pcv_mexico.Rda")


