load(file="../data/zika.Rda")

## If you have not installed these packages before,
##  run the following line:
# install.packages(c("tidyverse","knitr","lme4"))
## Either way, the packages must be loaded:
require(tidyverse)
require(knitr)
require(lme4)

## Plot birth rate over time by state:
#| fig-cap: "Plot of live birth rate (per 1,000 population) by year and state, Brazil, 2008–2016"
#| fig-alt: "A line plot with the line for Pernambuco slowly declining from around 16 in 2008 to around 15.5 in 2015 and then to 14 in 2016 while the line for Rio Grande do Sul slowly increases from around 12 in 2008 to around 13 in 2015 and then falls to 12.5 in 2016."
ggplot(data=zika_summ) +
  geom_line(mapping=aes(x=Year, y=Rate, color=State, linetype=State),
            linewidth=1.5) +
  theme_bw() +
  scale_y_continuous(name="Birth Rate (per 1000)",
                     limits=c(0,17),
                     breaks=seq(0,16,by=4),
                     minor_breaks=seq(0,16,by=2)) +
  geom_vline(xintercept=2015, linetype="longdash", color="grey50")

## Plot log of number of births over time by state:
#| fig-cap: "Plot of natural log of the number of live births by year and state, Brazil, 2008–2016"
#| fig-alt: "A line plot with the line for Pernambuco steadily increasing from around 11.2 in 2008 to around 11.5 in 2015 and then falling to around 11.4 in 2016 while the line for Rio Grande do Sul steadily increases from around 11.3 in 2008 to around 11.7 in 2015 and then holds steady in 2016."
ggplot(data=zika_summ) +
  geom_line(mapping=aes(x=Year, y=LogBirths, color=State, linetype=State),
            linewidth=1.5) +
  theme_bw() +
  scale_y_continuous(name="Log number of live births",
                     limits=c(10,12),
                     breaks=seq(10,12,by=1),
                     minor_breaks=seq(10,12,by=.25)) +
  geom_vline(xintercept=2015, linetype="longdash", color="grey50")

## Plot log of birth rate over time by state:
#| fig-cap: "Plot of natural log of the rate of live births (per 1,000 population) by year and state, Brazil, 2008–2016"
#| fig-alt: "A line plot with the line for Pernambuco declining from 2.8 in 2008 to just above 2.7 in 2010, holding steady until 2015, and then declining to around 2.64 in 2016, while the line for Rio Grande do Sul holds steady around 2.5 from 2008 to 2010, then increases steadily to nearly 2.6 in 2015 and declines to around 2.54 in 2016."
ggplot(data=zika_summ) +
  geom_line(mapping=aes(x=Year, y=LogRate, color=State, linetype=State),
            linewidth=1.5) +
  theme_bw() +
  scale_y_continuous(name="Log Birth Rate (per 1000)",
                     limits=c(2,3),
                     breaks=seq(2,3,by=.2),
                     minor_breaks=seq(2,3,by=.1)) +
  geom_vline(xintercept=2015, linetype="longdash", color="grey50")

## Get differences within-units:
did_2x2 <- zika_summ %>% dplyr::filter(Year %in% c(2014,2016)) %>%
  pivot_wider(id_cols=State,
              names_from=Year,
              values_from=Rate) %>%
  mutate(`Diff, 2016–2014`=`2016`-`2014`)
## Get differences within years:
yr_diffs <- did_2x2 %>%
  dplyr::filter(State=="Pernambuco") %>% 
  dplyr::select(-c(State)) - 
  did_2x2 %>%
  dplyr::filter(State=="Rio Grande do Sul") %>% 
  dplyr::select(-c(State))
## Combine into presentation table:
did_2x2_f <- did_2x2 %>% add_row(bind_cols(State="Diff, Treated–Untreated",
                                 yr_diffs))

## Print formatted table of two-by-two analysis:
knitr::kable(did_2x2_f,
             digits=1)

## Fit linear TWFE model and extract estimate and 95% CI:
zika_lm <- lm(Rate~State+year+interaction, data=zika_full)
summary(zika_lm)
confint(zika_lm, level=0.95)["interaction",]

## Fit a linear mixed effects model to account for clustering by municipality:
zika_lmer <- lmer(Rate~State+year+interaction+(1|Code), data=zika_full)
summary(zika_lmer)
confint(zika_lmer, level=0.95)["interaction",]

## First, get a list of unique codes by state
codes <- zika_full %>% dplyr::select(Code,State) %>% distinct()
## Then, write a function that samples codes within each state:
boot_data <- function() slice_sample(codes, by=State, prop=1, replace=TRUE) %>%
  expand_grid(StudyYear=rep(c(2014,2016))) %>%
  left_join(zika_full, by = join_by(Code, State, StudyYear))
## Set seed for reproducibility:
set.seed(3671)
## Run the sampling and analysis 1000 times to get a distribution:
lm_boot <- replicate(n=1000,
                     expr=lm(Rate~State+year+interaction,
                             data=boot_data())$coefficients["interaction"])
## Examine the median (estimate) and 2.5 and 97.5 percentiles:
quantile(lm_boot, probs=c(0.5,0.025,0.975))

## Create a data set with a row for each pair of years
zika_plac <- tibble(`Treated Year`=unique(zika_summ$Year),
                    `Untreated Year`=`Treated Year`-2) %>%
  dplyr::filter(`Untreated Year` %in% unique(zika_summ$Year)) %>%
  left_join(zika_summ %>% dplyr::select(Year,State,Rate) %>% 
              pivot_wider(id_cols=Year, names_from=State, values_from=Rate),
            by=join_by(`Treated Year`==Year)) %>% 
  rename(P1=Pernambuco, R1=`Rio Grande do Sul`) %>%
  left_join(zika_summ %>% dplyr::select(Year,State,Rate) %>% 
              pivot_wider(id_cols=Year, names_from=State, values_from=Rate),
            by=join_by(`Untreated Year`==Year)) %>% 
  rename(P0=Pernambuco, R0=`Rio Grande do Sul`)
## Get the estimate for each pair of years
zika_plac <- zika_plac %>%
  mutate(Estimate=(P1-P0)-(R1-R0))

## Plot two-by-two DID estimates for placebo years:
#| fig-cap: "Plot of two-by-two DID estimates for placebo treatment years (2010–2015) and actual treated year (2016)"
#| fig-alt: "A scatter plot with points ranging from around -0.868 to -0.135 in the 2010 through 2015 years, with a point around -1.3 in 2016."
ggplot(data=zika_plac) +
  geom_point(mapping=aes(x=`Treated Year`, y=Estimate),
             size=2) +
  theme_bw() +
  labs(x="Treated (Placebo) Year", y="DID Estimate") +
  scale_y_continuous(limits=c(-2,0), breaks=seq(-2,0,by=.5))

## Fit TWFE model with clustering by municipality and log-scale:
zika_log_lmer <- lmer(LogRate~State+year+interaction+(1|Code), data=zika_full)
summary(zika_log_lmer)
confint(zika_log_lmer, level=0.95)
## Exponentiate estimates and CI to get RR estimates:
exp(summary(zika_log_lmer)$coefficients["interaction","Estimate"])
exp(confint(zika_log_lmer, level=0.95))

## Add log-transformed DID estimate to placebo data set:
zika_plac <- zika_plac %>% mutate(LogEstimate=log(P1/P0)-log(R1/R0))

## Plot two-by-two log-transformed DID estimates for placebo years:
#| fig-cap: "Plot of two-by-two log-transformed DID estimates for placebo treatment years (2010–2015) and actual treated year (2016)"
#| fig-alt: "A scatter plot with points ranging from around 0 to -0.05 in the 2010 through 2015 years, with a point around -0.09 in 2016."
ggplot(data=zika_plac) +
  geom_point(mapping=aes(x=`Treated Year`, y=LogEstimate),
             size=2) +
  theme_bw() +
  labs(x="Treated (Placebo) Year", y="Log-Transformed DID Estimate") +
  scale_y_continuous(limits=c(-0.25,0.25), breaks=seq(-0.25,0.25,by=.125))

## # To execute this code into the document, change the previous line to true
## ## Fit Poisson GLM:
## zika_glm <- glm(Births~State+year+interaction+offset(log(Pop)), data=zika_full,
##                 family=poisson(link="log"))
## summary(zika_glm)
## ## Compute bootstrap 95% CI for Poisson GLM:
## set.seed(4008335)
## glm_boot <- replicate(n=1000,
##                       expr=glm(Births~State+year+interaction+offset(log(Pop)), data=boot_data(),
##                                family=poisson(link="log"))$coefficients["interaction"])
## glm_boot_res <- quantile(glm_boot, probs=c(0.5,0.025,0.975))
## ## Exponentiate results:
## exp(c(Estimate=unname(coef(zika_glm)["interaction"]),
##       CI.Lower=glm_boot_res["2.5%"],
##       CI.Upper=glm_boot_res["97.5%"]))
## ## Fit Poisson GLMM, clustered by municipality:
## zika_glmer <- glmer(Births~State+year+interaction+offset(log(Pop))+(1|Code),
##                     data=zika_full,
##                     family=poisson(link="log"))
## zika_glmer_CI <- confint(zika_glmer, parm="interaction", level=0.95)
## ## Exponentiate results:
## exp(c(Estimate=unname(summary(zika_glmer)$coefficients["interaction","Estimate"]),
##       CI.Lower=zika_glmer_CI["interaction","2.5 %"],
##       CI.Upper=zika_glmer_CI["interaction","97.5 %"]))
