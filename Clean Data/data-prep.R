### data-prep.R ###
### Cleaning and preparing Raw Data for analyses ###
### Lee Kennedy-Shaffer ###
### May 24, 2024 ###

require(tidyverse)
require(readxl)
require(haven)

proj_wd <- getwd()

## cholera data ##
### Run Coleman's Snow_ReadData.Rmd file to get regdata table ###
setwd("Raw Data/cholera/")
source("Snow_ReadData.R")
setwd(proj_wd)
### Save regdata table in Clean Data folder: ###
save(regdata, file="Clean Data/cholera.Rda")


## zika data ##
### Read in zika_Table2.tab data and remove subjects (by Code) with any missing data (8 codes, 16 observations)
zika <- read.table(file="Raw Data/zika/zika_Table2.tab",
                   header=TRUE, fill=TRUE)
zika_full <- zika %>% dplyr::filter(!(Code %in% unique(zika %>% filter(is.na(Pop)) %>% pull(Code))))
### Read in Births_Fig2.dta
zika_summ <- read_dta("Raw Data/zika/Births_Fig2.dta")
### Save output in Clean Data folder:
save(list=c("zika_full","zika_summ"), file="Clean Data/zika.Rda")


## mandate data ##
### Read in dates, vaccine data, and population data
Dates <- read_xlsx(path="Raw Data/mandate/MandateDates.xlsx", sheet="Ballotpedia") %>%
  dplyr::rename(Location=State) %>% mutate(Mandate_Start=as.Date(`Start Date`)) %>% 
  dplyr::select(-c(`Start Date`))
Vax <- read_csv(file="Raw Data/mandate/COVID-19_Vaccinations_in_the_United_States_Jurisdiction_20240508.csv") %>%
  mutate(DateT=mdy(Date))
Pops <- read_xlsx(path="Raw Data/mandate/Census_Pops.xlsx")
### Territories/regions to exclude:
Exclude_Locs <- c("AS","BP2","DC","DD2","FM","GU","IH2","LTC","MP","PR","PW","RP","US","VA2","VI")
### Exclude territories and calculate population-adjusted rates:
Vax_rates <- Vax %>% dplyr::filter(!(Location %in% Exclude_Locs)) %>% 
  left_join(Pops, by="Location") %>%
  rename(SCY=Series_Complete_Yes,SCP=Series_Complete_Pop_Pct) %>%
  dplyr::select(Location,Pop_Apr_2020,DateT,SCY,SCP) %>%
  mutate(SCP2=SCY/Pop_Apr_2020*100,
         SCY_100k=SCP2*1000) %>%
  arrange(Location,desc(DateT))
### Add 7-day prior values to data:
Vax_wk_prior <- Vax_rates %>% mutate(WkPrior=DateT-7) %>% 
  left_join(Vax_rates %>% rename(SCY_100k_wk=SCY_100k) %>%
              dplyr::select(Location,DateT,SCY_100k_wk) %>% 
              rename(WkPrior=DateT),
            by=join_by(Location,WkPrior)) %>%
  mutate(SCY_100k_diff=SCY_100k-SCY_100k_wk) %>%
  arrange(Location,desc(DateT))
### Get CDC MMWR weeks, Sunday--Saturday:
WkB1 <- seq.Date(from=as.Date("2021/03/13"), to=as.Date("2023/05/13"), by=7)
### Compute weekly vaccination rates:
Vax_weekly <- Vax_wk_prior %>% dplyr::filter(DateT %in% WkB1) %>%
  left_join(Dates, by="Location") %>%
  mutate(mandate=if_else(is.na(Mandate_Start),0,
                         if_else(Mandate_Start<=DateT,1,0)),
         Wks_mandate=ifelse(mandate==0,0,ceiling((DateT-Mandate_Start+1)/7)),
         LeadLag=ifelse(is.na(Mandate_Start),NA,ceiling((DateT-Mandate_Start+1)/7))) %>%
  left_join(tibble(DateT=WkB1,WeekNum=seq(from=1,by=1,length.out=length(WkB1))))
### Save output in Clean Data folder:
save(Vax_weekly, file="Clean Data/mandate.Rda")


## lottery data ##
### Note that raw data files are not on the Epi-QEs github page, but are available at:
### https://doi.org/10.7910/DVN/K1XX02 and https://doi.org/10.7910/DVN/QYXN9L
### Load and save Fuller data:
load("Raw Data/lottery/synth_data_clean.RData")
save(list=c("lotteries","state_vaccines","untreated_states"),
     file="Clean Data/lottery_fuller.Rda")
### Load and save Lang data:
lang_0624 <- readRDS("Raw Data/lottery/weekly_data_2021-06-24.rds")
lang_0912 <- readRDS("Raw Data/lottery/weekly_data_2021-09-12.rds")
lang_announce_dates <- read_csv("Raw Data/lottery/lottery_announce_dates.csv")
save(list=c("lang_0624","lang_0912","lang_announce_dates"),
     file="Clean Data/lottery_lang.Rda")
### Note that Fuller et al. identify a lottery in Missouri that Lang et al. do not use; 
### otherwise, the announce dates differ only by a few days in some cases


## pcv data ##
### Note that raw data files are not on the Epi-QEs github page, but are available at:
### https://github.com/weinbergerlab/InterventionEvaluatR
### Load and save data sets used in Bruhn et al.:
load("Raw Data/pcv/data/pnas_brazil.rda")
load("Raw Data/pcv/data/pnas_chile.rda")
load("Raw Data/pcv/data/pnas_ecuador.rda")
load("Raw Data/pcv/data/pnas_mexico.rda")
load("Raw Data/pcv/data/pnas_us_ipd.rda")
load("Raw Data/pcv/data/pnas_us_pneumonia.rda")
load("Raw Data/pcv/data/ecuador_mortality.rda")
save(list=c(paste0("pnas_",c("brazil","chile","ecuador","mexico","us_ipd","us_pneumonia")),
            "ecuador_mortality"),
     file="Clean Data/pcv.Rda")


