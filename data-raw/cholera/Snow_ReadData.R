## ----message=FALSE, results='hide'-----------------------------------------------------------------------
# Copyright (c) 2019, Thomas Coleman
#
#  -------  Licensed under BSD 2-Clause "Simplified" License  -------
#
# Results and discussion in "Causality in the Time of Cholera: John Snow as a Prototype 
# for Causal Inference (Working Paper)" available at SSRN: https://papers.ssrn.com/abstract=3262234

# June 2023, modified to use alternate sub-district population measures:
#   Based on variable (flag) "population_adj" which is set in the "calling file" (NB - this is not a robust method)
#     = 0: no adjustment to subdistrict, use original population
#     = 1: zero out Lambeth in Southwark-only subdistricts
#     = 2: adjust upwards so subdistrict sums match district reports
#     = 3: adjust by-supplier subdistrict downwards if combined SV + Lambeth is greater than 1854
#     = 4: adjust subdistrict population upwards if combined SV + Lambeth is greater than 1854. OVERWRITE POP1854!!! 
#  If not set, then use 3

#   Based on variable (flag) "population_1851" which is set in the "calling file" (NB - this is not a robust method)
#     = 0: use 1849 & 1854 population
#     = 1: use 1851 population (rather than 1849 & 1854)
#  If not set, then use 0

#rm(list=ls())    # starts a fresh workspace
#
library(knitr)
options(scipen=5)
# The following libraries are used for the Negative Binomial regression and the robust standard error analysis
#install.packages("sandwich")
#install.packages("lmtest")
library("MASS")
library("sandwich") 
library("lmtest") 
library("lme4")           ## For random effects Poisson & Negative Binomial # Is this part of base R?
library("dplyr")          ## load For doing data manipulation, such as group_by and sum. The "select" function gets 
                          # masked so call by dplyr::select

# From excellent site http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html
# Function for calculating overdispersion of a model.
# I've modified to do either Pearson residuals or Deviance residuals.
# I thought deviance residuals were the better
overdisp_fun <- function(model,type="deviance") {
  # Works for type="pearson" or "deviance"
    rdf <- df.residual(model)
    rp <- residuals(model,type=type)
    resid.chisq <- sum(rp^2)   # This is the "Residual Deviance"
    prat <- resid.chisq/rdf
    pval <- pchisq(resid.chisq, df=rdf, lower.tail=FALSE)
    if (type == "deviance") {
      xoverdisp <- c(ResidDev=resid.chisq,rdf=rdf,ratio_chisqdf=prat,p_ResidDev=pval)
    }
    else {
      xoverdisp <- c(pearson=resid.chisq,rdf=rdf,ratio_chisqdf=prat,p=pval)
    }
  # Now calculate the pseudo-Rsq and the rate Rsq
    if (class(model)[1] == "glmerMod") {
      xPseudoRsq <- 0   
      # For the life of me I cannot figure out how to get the Null Deviance for the lmer model (RE model)
      # The lrtest does not work, and in any case gives the wrong answer for glm models (it fits the "restricted
      # mean-only model without the offset - really stupid)
      # The residual deviance is easy - that is above (resid.chisq)
    }
    else {
      xPseudoRsq <- 1 - (model$deviance/model$null.deviance)
    }
    # Data fram from model. 1st element will be counts, last log(offset)
    x1 <- model.frame(model)
    xlcount <- (x1[,1])
    x2 <- dim(x1)
    xoffset <- x1[,x2[2]]
    xpred <- predict(model)
    xlrate_act <- xlcount / exp(xoffset)
    xlrate_pred <- exp(xpred) / exp(xoffset)
    xRsq <- (cor(xlrate_pred,xlrate_act))^2
    xsdrate <- 10000 * sd(xlrate_act)
    xlrate_act <- log(xlcount) - xoffset
    xlrate_pred <- xpred - xoffset
    xlnRsq <- (cor(xlrate_pred,xlrate_act))^2
    xsdlrate <- sd(xlrate_act)
    #x1 <- list(xoverdisp,xpseudoRsq)
    xoverdisp <- append(xoverdisp,c(PseudoRsq=xPseudoRsq,lnRateRsq=xlnRsq,RateRsq=xRsq,RateSD=xsdrate,lnRateSD=xsdlrate,BIC=BIC(model)))
    xoverdisp
}





# Read in the data from Snow 1855 "On the mode of communication of cholera"
tableviii <- read.csv(file="Snow1855_TableVIII.csv", header=TRUE, sep=",", skip=5,comment.char="#")
tablevii <- read.csv(file="Snow1855_TableVII.csv", header=TRUE, sep=",", skip=5,comment.char="#")
tablexii <- read.csv(file="Snow1855_TableXII.csv", header=TRUE, sep=",", skip=5,comment.char="#")

# Read in the data from John Snow 1856, "Cholera and the water supply in the south district of London in 1854", 
#   These data were copied from the 1936 book "Snow on cholera, being a reprint of two papers" edited by Frost
# Table V by District (for running Poisson & Neg Binomial count regressions)
# Table VI by sub-district (for running Koch & Denike's tests)

# Table I "Showing the results of the Author's personal Inquiry into Twenty-One Sub-Districts"
# Table II "Showing the results of Inquiry made by Mr. Whiting in Eleven Sub-Districts"
# (My "tableI_1856" combines Snow's Tables I & II)

tableI_1856 <- read.csv(file="Snow1856_TableI.csv", header=TRUE, sep=",", skip=5,comment.char="#")

tableV_1856 <- read.csv(file="Snow1856_TableV.csv", header=TRUE, sep=",", skip=5,comment.char="#")
tableVI_1856 <- read.csv(file="Snow1856_TableVI.csv", header=TRUE, sep=",", skip=5,comment.char="#")

tableSimonIII <- read.csv(file="Simon1856_TableIII_pop.csv",header=TRUE, sep=",", skip=5,comment.char="#")

tableSimonIII_StNoDeath <- read.csv(file="Simon1856_TableIII_StNoDeath.csv",header=TRUE, sep=",", skip=6,comment.char="#")



## --------------------------------------------------------------------------------------------------------
# Adjust the subdistrict population up by the fraction of reported subdistrict population 
# (from Table V) versus summed subdistrict population (summed from Table I, which is the same as Table VI)
# Sum the subdsitricts using dplyr "group_by" and "summarize"
xx1 <- tableI_1856[1:32,] %>%
  group_by(district) %>% 
  summarise(across(c(pop_southwark,pop_lambeth),sum)) 

# Join subdistrict data for first 7 weeks (tableI_1856) with overall 1849 & 1854 deaths from Table XII
xx2 <- inner_join(tableI_1856[tableI_1856$district != "na",], tablexii[tablexii$district != "na",c("subDistrict","deaths1849","deaths1854")], by = "subDistrict")
# Join the first 4 weeks from 1855 Table VII
xx2 <- inner_join(xx2,tablevii[,c("deathsOverall","deathsSouthwark","deathsLambeth","deathsPump","deathThames","deathsUnascertained","subDistrict")],by="subDistrict",suffix=c(".7wks",".first4wks"))
# Put in the 1849 & 1854 population estimates from Simon 1856 Table III
xx2 <- left_join(xx2, tableSimonIII[tableSimonIII$district != "Total",c("subDistrict","pop1849","pop1854")], by = "subDistrict")
# Join the subdistricts (Table I) with reported districts (Table V)
xx3 <- left_join(xx2, tableV_1856[!is.na(tableV_1856$districtID),c("district","pop_southwark","pop_lambeth")], by = "district",suffix=c("",".district"))
# Join the district "missing population" by supplier - the population in "Streets where no death occurred" by district
xx3 <- left_join(xx3,tableSimonIII_StNoDeath[,c("district","lambeth_misspop","southwark_misspop")], by = "district")
# And join with summed districts (from xx1 just above)
xx4 <- inner_join(xx3, xx1, by = "district",suffix=c(".x",".sumdistrict"))

# Finally, adjust population 
# Modified 15-jun-2023 to adjust pop_southwark_adj as well as perc_southwark
xpop_southwark_0 <- xx4$pop_southwark.x
xpop_lambeth_0 <- xx4$pop_lambeth.x
xpop_combined_0 <- xpop_southwark_0 + xpop_lambeth_0
# "perc_0" is no adjustment to by-supplier population
xx4$perc_southwark_0 <- xpop_southwark_0 / xx4$pop1854
xx4$perc_lambeth_0 <- xpop_lambeth_0 / xx4$pop1854
# Set the Lambeth population for the Southwark-only "First 12" subdistricts to zero - it should be zero but is not in Simon's tables
xpop_southwark_1 <- xpop_southwark_0
xpop_lambeth_1 <- xpop_lambeth_0
xpop_lambeth_1[xx4$supplier == "SouthwarkVauxhall"] = 0
# "perc_1" sets Lambeth population in Southwark-only subdistricts to zero
xx4$perc_southwark_1 <- xpop_southwark_1 / xx4$pop1854
xx4$perc_lambeth_1 <- xpop_lambeth_1 / xx4$pop1854
xpop_combined_1 <- xpop_southwark_1 + xpop_lambeth_1
# This adjusts up by-supplier subdistrict population based on the "Houses supplied in streets where no death occurred" 
#    from Simon Table III
#xpop_southwark_2 <- (xpop_southwark_1*(xx4$pop_southwark.district/xx4$pop_southwark.sumdistrict))
xpop_southwark_2 <- xpop_southwark_1*(1 + xx4$southwark_misspop/xx4$pop_southwark.sumdistrict)
   # Use "pmax" for the summed lambeth to replace zeros with 1s so they do not produce NAN
#xpop_lambeth_2 <- xpop_lambeth_1*(xx4$pop_lambeth.district/pmax(1,xx4$pop_lambeth.sumdistrict))
xpop_lambeth_2 <- xpop_lambeth_1*(1 + xx4$lambeth_misspop/pmax(1,xx4$pop_lambeth.sumdistrict))
xpop_combined_2 <- xpop_southwark_1 + xpop_lambeth_1
# "perc_2" is subdistrict populations are adjusted up so summed subdistricts match districts
# But for many subdistricts (and districts) this population is now higher than "estimated 1854)
# So create two new estimates
#   _3 adjusts subdistrict by-supplier down to match subdistrict 1854 estimates
#   _4 adjusts subdistrict 1854 upwards to match by-supplier subdistrict
xx4$perc_southwark_2 <- xpop_southwark_2 / xx4$pop1854
xx4$perc_lambeth_2 <- xpop_lambeth_2 / xx4$pop1854
xpop_combined_2 <- xpop_southwark_2 + xpop_lambeth_2
# "perc_3" is The supplier-specific population adjusted back down , as percentage of total (1854) population
# Reduce combined population to maximum of 1854 pop, then adjust downward the S&V and Lambeth pop
xpop_combined_3 <- pmin(xx4$pop1854,xpop_combined_2)
xx <- xpop_combined_3 / xpop_combined_2   # ratio of excess population
xpop_southwark_3 <- xpop_southwark_2 * xx
xpop_lambeth_3 <- xpop_lambeth_2 * xx
xx4$perc_southwark_3 <- (xpop_southwark_3/xx4$pop1854) 
xx4$perc_lambeth_3 <- (xpop_lambeth_3/xx4$pop1854)
# Increase overall as appropriate. NB: THIS MAY BE DANGEROUS
xpop_combined_4 <- xpop_combined_2
xpop_total <- pmax(xx4$pop1854,xpop_combined_2)
xpop_southwark_4 <- xpop_southwark_2
xpop_lambeth_4 <- xpop_lambeth_2
xx4$perc_southwark_4 <- (xpop_southwark_4/xpop_total) 
xx4$perc_lambeth_4 <- (xpop_lambeth_4/xpop_total)


# Changing here will set to different population adjustments:
#  _0: no adjustment
#  _1: zero out Lambeth in Southwark-only subdistricts
#  _2: adjust upwards so subdistrict sums match district reports
#  _3: adjust by-supplier subdistrict downwards if combined SV + Lambeth is greater than 1854
#  _4: adjust subdistrict total upwards if combined SV + Lambeth is greater than 1854. OVERWRITE POP1854!!!
#  _3 is the default if flag population_adj does not exist
xx4$perc_southwark_adj <- xx4$perc_southwark_3
xx4$perc_lambeth_adj <- xx4$perc_lambeth_3
xx4$perc_combined_adj <- (xpop_combined_3 / xx4$pop1854)
xx4$pop_southwark_adj <- xpop_southwark_3
xx4$pop_lambeth_adj <- xpop_lambeth_3
xx4$pop_combined_adj <- xpop_combined_3
if(exists("population_adj")){
    if(population_adj == 0) {
      xx4$perc_southwark_adj <- xx4$perc_southwark_0
      xx4$perc_lambeth_adj <- xx4$perc_lambeth_0
      xx4$perc_combined_adj <- (xpop_combined_0 / xx4$pop1854)
      xx4$pop_southwark_adj <- xpop_southwark_0
      xx4$pop_lambeth_adj <- xpop_lambeth_0
      xx4$pop_combined_adj <- xpop_combined_0
    }
    if(population_adj == 1) {
      xx4$perc_southwark_adj <- xx4$perc_southwark_1
      xx4$perc_lambeth_adj <- xx4$perc_lambeth_1
      xx4$perc_combined_adj <- (xpop_combined_1 / xx4$pop1854)
      xx4$pop_southwark_adj <- xpop_southwark_1
      xx4$pop_lambeth_adj <- xpop_lambeth_1
      xx4$pop_combined_adj <- xpop_combined_1
    }
    if(population_adj == 2) {
      xx4$perc_southwark_adj <- xx4$perc_southwark_2
      xx4$perc_lambeth_adj <- xx4$perc_lambeth_2
      xx4$perc_combined_adj <- (xpop_combined_2 / xx4$pop1854)
      xx4$pop_southwark_adj <- xpop_southwark_2
      xx4$pop_lambeth_adj <- xpop_lambeth_2
      xx4$pop_combined_adj <- xpop_combined_2
    }
    if(population_adj == 4) {
      xx4$perc_southwark_adj <- xx4$perc_southwark_4
      xx4$perc_lambeth_adj <- xx4$perc_lambeth_4
      xx4$pop1854 <- xpop_total     # NB: OVERWRITES POP1854!!!
      xx4$perc_combined_adj <- (xpop_combined_4 / xx4$pop1854)
      xx4$pop_southwark_adj <- xpop_southwark_4
      xx4$pop_lambeth_adj <- xpop_lambeth_4
      xx4$pop_combined_adj <- xpop_combined_4
    }
  # by default (and if population_adj does not exist) do _3
  } 
xx4$perc_other_adj <- 1 - xx4$perc_combined_adj





#x1 <- subset(xx4,supplier == "SouthwarkVauxhall" | supplier == "SouthwarkVauxhall_Lambeth")
x1 <- xx4
x1849 <- x1[c("subDistrict","district","pop1851","supplier","lambethdegree")]
# Rename "pop1849" to "population"
x1849$population <- x1$pop1849
# Use 1851 population if flag set
#   Based on variable (flag) "population_1851" which is set in the "calling file" (NB - this is not a robust method)
#     = 0: use 1849 & 1854 population
#     = 1: use 1851 population (rather than 1849 & 1854)
#  If not set, then use 0
if(exists("population_1851")) {
  if(population_1851 == 1) {
    x1849$population <- x1$pop1851
  }
}
#names(x1849)[names(x1849) == "pop1849"] <- "population"


x1849$deaths <- x1$deaths1849
x1849$rate <- 10000 * x1$deaths1849 / x1849$population
x1849$seq <- c(seq(1,length(x1849$deaths)))
# Create a dummy variable for Thames deaths 
x1849$ThamesDummy <- x1$deathThames.7wks > 5
#x1849$dum1854 <- 0
x2 <- dim(x1849)[1]
xyear <- factor(c(rep(1849,x2),rep(1854,x2)))
x1849$year <- xyear[1:x2]
x1854 <- x1849
x1854$population <- x1$pop1854
# Use 1851 population if flag set
if(exists("population_1851")) {
  if(population_1851 == 1) {
    x1854$population <- x1$pop1851
  }
}
#x1849$lambethdegree <- "dirty"
x1854$deaths <- x1$deaths1854
x1854$rate <- 10000 * x1$deaths1854 / x1854$population
x1854$seq <- c(seq(1,length(x1849$deaths)))
#x1854$dum1854 <- 1
x1854$year <- xyear[(x2+1):(2*x2)]

x1849$subDistrict_1854 <- "a_none"
x1854$subDistrict_1854 <- x1854$subDistrict
x1849$subDistrict_1849 <- x1849$subDistrict
x1854$subDistrict_1849 <- "a_none"

x1849$perc_other <- x1$perc_other_adj
x1854$perc_other <- x1$perc_other_adj
x1849$perc_lambeth <- x1$perc_lambeth_adj
x1854$perc_lambeth <- x1$perc_lambeth_adj
x1849$perc_lambeth54 <- 0
x1854$perc_lambeth54 <- x1$perc_lambeth_adj
x1849$perc_southwark <- x1$perc_southwark_adj
x1854$perc_southwark <- x1$perc_southwark_adj

# population per house seems to come in as factor - convert to numeric
x1849$pop_per_house <- as.numeric(as.character(tableI_1856$pop_per_house[1:x2]))
x1854$pop_per_house <- as.numeric(as.character(tableI_1856$pop_per_house[1:x2]))

# Number of weeks for the epidemics
#  1849: From Vinten-Johanssen: "The start of the epidemic is generally given as the week ending 26 May, 
#        and the end in November, between the 10th and the 24th (24-26 weeks)."
#  1854: p. 89 of OMCC2, Table XII is through week ending 21 October, 15 weeks
#        but note that 1856 Table VI is 17 weeks (so higher counts)

x1849$weeks <- 1
x1854$weeks <- 1
x1849$popepidemic <- x1849$population * x1849$weeks
x1854$popepidemic <- x1854$population * x1854$weeks


# -------------
# Now for "Direct" observations, with deaths by supplier for first 7 weeks of 1854
# Also stack data for:
#   1849 - full epidemic, combined all sources
#          deaths Table XII, population Table VIII
#   1854 - first 7 weeks, split by supplier (Southwark & Vauxhall, Lambeth, Other)
#          deaths Table VIII, population 1856 Table VI
#   1854 - last 8 weeks, combined all sources
#          deaths Table XII minus VIII, population Table VIII

# Start with x1854 as the "template" and then fill (overwrite) the population and deaths
# First make new variable "year_split1st7" which splits 1854 into "1854" and "1854late" to allow 
#   time effect for first and 2nd part of 1854 epidemic
x1849$year_split1st7 <- x1849$year
x1849$year_split1st4 <- x1849$year
x1854$year_split1st7 <- x1854$year
x1854$year_split1st4 <- x1854$year
x1854VIII_southwark <- x1854
x1854VIII_southwark$year_split1st7 <- "1854early"
x1854VIII_southwark$year_split1st4 <- "1854_2nd3"
x1854VIII_southwark$weeks <- 7/15
x1854VIII_southwark$deaths <- x1$deathsSouthwark.7wks
x1854VIII_southwark$population <- x1854VIII_southwark$population * x1854VIII_southwark$perc_southwark
x1854VIII_southwark$rate <- 10000 * x1854VIII_southwark$deaths / x1854VIII_southwark$population
x1854VIII_southwark$perc_lambeth <- 0
x1854VIII_southwark$perc_lambeth54 <- 0
x1854VIII_southwark$perc_other <- 0
x1854VIII_southwark$perc_southwark <- 1
x1854VIII_southwark$subDistrict_1854 <- x1854$subDistrict
x1854VIII_southwark$popepidemic <- x1854VIII_southwark$population * x1854VIII_southwark$weeks

x1854VIII_lambeth <- x1854
x1854VIII_lambeth$year_split1st7 <- "1854early"
x1854VIII_lambeth$year_split1st4 <- "1854_2nd3"
x1854VIII_lambeth$weeks <- 7/15
x1854VIII_lambeth$deaths <- x1$deathsLambeth.7wks
x1854VIII_lambeth$population <- x1854VIII_lambeth$population * x1854VIII_lambeth$perc_lambeth
x1854VIII_lambeth$rate <- 10000 * x1854VIII_lambeth$deaths / pmax(1,x1854VIII_lambeth$population)
x1854VIII_lambeth$perc_lambeth <- 1
x1854VIII_lambeth$perc_lambeth54 <- 1
x1854VIII_lambeth$perc_other <- 0
x1854VIII_lambeth$perc_southwark <- 0
x1854VIII_lambeth$subDistrict_1854 <- x1854$subDistrict
x1854VIII_lambeth$popepidemic <- x1854VIII_lambeth$population * x1854VIII_lambeth$weeks

x1854VIII_other <- x1854
x1854VIII_other$year_split1st7 <- "1854early"
x1854VIII_other$year_split1st4 <- "1854_2nd3"
x1854VIII_other$weeks <- 7/15
x1854VIII_other$deaths <- x1$deathsPump.7wks + x1$deathThames.7wks
x1854VIII_other$population <- x1854VIII_other$population * x1854VIII_other$perc_other
x1854VIII_other$rate <- 10000 * x1854VIII_other$deaths / pmax(1,x1854VIII_other$population)
x1854VIII_other$perc_lambeth <- 0
x1854VIII_other$perc_lambeth54 <- 0
x1854VIII_other$perc_other <- 1
x1854VIII_other$perc_southwark <- 0
x1854VIII_other$popepidemic <- x1854VIII_other$population * x1854VIII_other$weeks
x1854VIII_other$subDistrict_1854 <- x1854$subDistrict

x1854_last8 <- x1854
x1854_last8$year_split1st7 <- "1854late"
x1854_last8$year_split1st4 <- "1854_last8"
x1854_last8$weeks <- 8/15
x1854_last8$deaths <- x1$deaths1854 - x1$deathsOverall.7wks
x1854_last8$rate <- 10000 * x1854_last8$deaths / pmax(1,x1854_last8$population)
x1854_last8$popepidemic <- x1854_last8$population * x1854_last8$weeks
x1854_last8$subDistrict_1854 <- x1854$subDistrict



# Now create data for first 4 weeks (Table VII), next 3 weeks (Table VIII - Table VII), and last 8 weeks (all suppliers combined)
# Start with the already-created "x1854VIII_" arrays
x1854VII_southwark = x1854VIII_southwark
x1854VII_lambeth = x1854VIII_lambeth
x1854VII_other = x1854VIII_other
x1854VIII4wk_southwark = x1854VIII_southwark
x1854VIII4wk_lambeth = x1854VIII_lambeth
x1854VIII4wk_other = x1854VIII_other
# We only need to overwrite the weeks and deaths, AND THE RATES
x1854VII_southwark$weeks <- 3/15
x1854VII_southwark$deaths <- x1$deathsSouthwark.first4wks
x1854VII_southwark$rate <- 10000 * x1854VII_southwark$deaths / pmax(1,x1854VII_southwark$population)
x1854VII_southwark$year_split1st4 <- "1854_1st4"
x1854VII_lambeth$weeks <- 3/15
x1854VII_lambeth$deaths <- x1$deathsLambeth.first4wks
x1854VII_lambeth$rate <- 10000 * x1854VII_lambeth$deaths / pmax(1,x1854VII_lambeth$population)
x1854VII_lambeth$year_split1st4 <- "1854_1st4"
x1854VII_other$weeks <- 3/15
x1854VII_other$deaths <- x1$deathsPump.first4wks + x1$deathThames.first4wks
x1854VII_other$rate <- 10000 * x1854VII_other$deaths / pmax(1,x1854VII_other$population)
x1854VII_other$year_split1st4 <- "1854_1st4"
# And update the deaths for x1854VIII to be "next 4 weeks" (first 7 - first 3)
x1854VIII4wk_southwark$weeks <- 4/15
x1854VIII4wk_southwark$deaths <- x1854VIII_southwark$deaths - x1$deathsSouthwark.first4wks
x1854VIII4wk_southwark$rate <- 10000 * x1854VIII4wk_southwark$deaths / pmax(1,x1854VIII4wk_southwark$population)
x1854VIII4wk_lambeth$weeks <- 4/15
x1854VIII4wk_lambeth$deaths <- x1854VIII_lambeth$deaths - x1$deathsLambeth.first4wks
x1854VIII4wk_lambeth$rate <- 10000 * x1854VIII4wk_lambeth$deaths / pmax(1,x1854VIII4wk_lambeth$population)
x1854VIII4wk_other$weeks <- 4/15
x1854VIII4wk_other$deaths <- x1854VIII_other$deaths - (x1$deathsPump.first4wks + x1$deathThames.first4wks)
x1854VIII4wk_other$rate <- 10000 * x1854VIII4wk_other$deaths / pmax(1,x1854VIII4wk_other$population)


x1854VIII_lambeth <- x1854VIII_lambeth[x1854VIII_lambeth$supplier == "SouthwarkVauxhall_Lambeth",]
x1854VIII_other <- x1854VIII_other[!x1854VIII_other$population == 0,]
x1854VIII_other <- x1854VIII_other[!is.na(x1854VIII_other$population),]
x1854VII_lambeth <- x1854VII_lambeth[x1854VII_lambeth$supplier == "SouthwarkVauxhall_Lambeth",]
x1854VII_other <- x1854VII_other[!x1854VII_other$population == 0,]
x1854VII_other <- x1854VIII_other[!is.na(x1854VII_other$population),]
x1854VIII4wk_lambeth <- x1854VIII4wk_lambeth[x1854VIII4wk_lambeth$supplier == "SouthwarkVauxhall_Lambeth",]
x1854VIII4wk_other <- x1854VIII4wk_other[!x1854VIII4wk_other$population == 0,]
x1854VIII4wk_other <- x1854VIII4wk_other[!is.na(x1854VIII4wk_other$population),]


# All data stacked. These are data from 1855 Table VIII for first 7 weeks (mixed populations) and last 8 weeks (all supplier combined)
# From this one can extract various regression data
xdata_combined <- rbind(x1849,x1854,x1854VIII_lambeth,x1854VIII_other,x1854VIII_southwark,x1854_last8)
xdata_combined$early1854Dummy <- xdata_combined$year_split1st7 == "1854early"

# Stack the 1849, 1854 1st 3wks, 1854 next 4 wks, 1854 last 8 wks
xdata_combined_1st4 <- rbind(x1849,x1854,x1854VII_lambeth,x1854VII_other,x1854VII_southwark,x1854VIII4wk_lambeth,x1854VIII4wk_other,x1854VIII4wk_southwark,x1854_last8)


# Make regression data-files
#regdata <- rbind(x1849,x1854)
#regdata_mixedDiD7wks <- rbind(x1849,x1854VIII_lambeth,x1854VIII_other,x1854VIII_southwark,x1854_last8)
# Select the "First 12" and "Next 16" subdistricts - i.e. exclude the "Lambeth Co only" subdistricts
x1 <- subset(xdata_combined,supplier == "SouthwarkVauxhall" | supplier == "SouthwarkVauxhall_Lambeth")
# regdata is for DiD with 1849 & 1854 combined data only
regdata <- subset(x1,year_split1st7 == "1849" | year_split1st7 == "1854")
# regdata_mixedDiD7wks is for DiD with 1849 combined, 1854 early direct comparison, 1854 late combined
regdata_mixedDiD7wks <- subset(x1,year_split1st7 != "1854" )
regdata_mixedDiD7wks_xOther <- subset(regdata_mixedDiD7wks,(perc_other != 1.0))
#  regdata1855VIIjoint: 1854 early (first 7 weeks) direct comparison for S&V and Lambeth only (excluding "other")
#                      and for jointly-supplied "Next 16" subdistricts
#  regdata1855VIIIboth: 1854 early (first 7 weeks) direct comparison for S&V and Lambeth only (excluding "other")
#                      for all 28 subdistrict ("first 12" supplied only by S&V) and jointly-supplied "Next 16" subdistricts
regdata1855VIIIboth <- subset(x1,year_split1st7 == "1854early" )
regdata1855VIIIboth <- subset(regdata1855VIIIboth,perc_other < 1.0 )
regdata1855VIIIjoint <- subset(regdata1855VIIIboth,supplier == "SouthwarkVauxhall_Lambeth")
regdata1855VIIIboth[regdata1855VIIIboth$perc_southwark ==1,"supplier"] = "Southwark"
regdata1855VIIIboth[regdata1855VIIIboth$perc_lambeth ==1,"supplier"] = "xLambeth"
regdata1855VIIIjoint[regdata1855VIIIjoint$perc_southwark ==1,"supplier"] = "Southwark"
regdata1855VIIIjoint[regdata1855VIIIjoint$perc_lambeth ==1,"supplier"] = "xLambeth"

x1849 <- subset(x1,year_split1st7 == "1849")
x1854 <- subset(x1,year_split1st7 == "1854")


# Select the "First 12" and "Next 16" subdistricts - i.e. exclude the "Lambeth Co only" subdistricts
x1 <- subset(xdata_combined_1st4,supplier == "SouthwarkVauxhall" | supplier == "SouthwarkVauxhall_Lambeth")
# regdata_mixedDiD1st4wks is for DiD with 1849 combined, 1854 1st 3wks, next 4wks (separate by S&V vs Lambeth),  final 8wks combined
regdata_mixedDiD1st4wks <- subset(x1,year_split1st7 != "1854" )
regdata_mixedDiD1st4wks_xOther <- subset(regdata_mixedDiD1st4wks,(perc_other != 1.0))


#regdata


## ----include=FALSE, eval=FALSE---------------------------------------------------------------------------
## 
## xpopcom <- matrix(0,ncol=3,nrow=4)
## rownames(xpopcom) <- c("Adj0","Adj1","Adj2","Adj3")
## colnames(xpopcom) <- c("S&VCustomers","Lambeth","Combined")
## 
## 
## xpopcom[1,1] <- sum(xx4$pop1854[xx4$supplier != "Lambeth"]*xx4$perc_southwark_0[xx4$supplier != "Lambeth"])
## xpopcom[2,1] <- sum(xx4$pop1854[xx4$supplier != "Lambeth"]*xx4$perc_southwark_1[xx4$supplier != "Lambeth"])
## xpopcom[3,1] <- sum(xx4$pop1854[xx4$supplier != "Lambeth"]*xx4$perc_southwark_2[xx4$supplier != "Lambeth"])
## xpopcom[4,1] <- sum(xx4$pop1854[xx4$supplier != "Lambeth"]*xx4$perc_southwark_3[xx4$supplier != "Lambeth"])
## xpopcom[1,2] <- sum(xx4$pop1854[xx4$supplier != "Lambeth"]*xx4$perc_lambeth_0[xx4$supplier != "Lambeth"])
## xpopcom[2,2] <- sum(xx4$pop1854[xx4$supplier != "Lambeth"]*xx4$perc_lambeth_1[xx4$supplier != "Lambeth"])
## xpopcom[3,2] <- sum(xx4$pop1854[xx4$supplier != "Lambeth"]*xx4$perc_lambeth_2[xx4$supplier != "Lambeth"])
## xpopcom[4,2] <- sum(xx4$pop1854[xx4$supplier != "Lambeth"]*xx4$perc_lambeth_3[xx4$supplier != "Lambeth"])
## 
## xpopcom[,3] <- xpopcom[,1] + xpopcom[,2]
## 


## --------------------------------------------------------------------------------------------------------

#rm(list=c("xx1","xx2","xx3","xx4","x1","xpopcom","x1854VII_lambeth","x1854VIII_other","x1854VII_southwark",
#          "x1854_last8","xx","xpop_southwark_0","xpop_lambeth_0","xpop_southwark_1","xpop_lambeth_1","xpop_southwark_2","xpop_lambeth_2","xpop_southwark_3","xpop_lambeth_3"))


