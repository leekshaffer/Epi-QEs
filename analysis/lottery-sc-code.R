load(file="../data/lottery_lang.Rda")

## If you have not installed these packages before,
##  run the following line:
# install.packages(c("tidysynth","devtools"))
## Either way, require the libraries:
require(tidyverse)
require(knitr)
require(tidysynth)
require(devtools)
## If augsynth has not yet been installed, run the following line:
# devtools::install_github("ebenmichael/augsynth")
require(augsynth)

## We first save the annnouncement date for Ohio's lottery:
Ohio_ann <- lang_ann_dates %>% dplyr::filter(state=="OH") %>%
  pull(lott_date)
## Plot time series of mandates themselves:
#| fig-cap: "Plot of fully vaccinated percentages by state and lottery status, January–June 2021"
#| fig-alt: "A line plot with line for each state over the time range specified. Ohio is highlighted and generally in the middle of the range."
ggplot(data=lang_0624,
       mapping=aes(group=state,linetype=type,color=type,alpha=type,
                   x=last_day,y=people_fully_vaccinated_per_hundred)) +
  scale_alpha_manual(name=NULL, breaks=c("Ohio","Other Lottery State","Non-Lottery State"),
                     values=c(1,0.8,0.5)) +
  geom_line() + theme_bw() +
  geom_vline(xintercept=Ohio_ann, linetype="dotted") +
  labs(x="Day (2021)", y="Percent Fully Vaccinated",
       linetype=NULL,color=NULL)

ggplot(data=lang_0912,
       mapping=aes(group=state,linetype=type,color=type,alpha=type,
                   x=last_day,y=people_fully_vaccinated_per_hundred)) +
  scale_alpha_manual(name=NULL, breaks=c("Ohio","Other Lottery State","Non-Lottery State"),
                     values=c(1,0.8,0.5)) +
  geom_line() + theme_bw() +
  geom_vline(xintercept=Ohio_ann, linetype="dotted") +
  labs(x="Day (2021)", y="Percent Fully Vaccinated",
       linetype=NULL,color=NULL)

## Create data set that averages across states within each group:
lang_avg <- lang_0624 %>% group_by(type,week,last_day) %>%
  dplyr::summarize(full_vax_avg=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE))

ggplot(data=lang_avg,
       mapping=aes(linetype=type,color=type,alpha=type,
                   x=last_day,y=full_vax_avg)) +
  scale_alpha_manual(name=NULL, breaks=c("Ohio","Other Lottery States (Mean)","Non-Lottery States (Mean)"),
                     values=c(1,0.8,0.5)) +
  geom_line() + theme_bw() +
  geom_vline(xintercept=Ohio_ann, linetype="dotted") +
  labs(x="Day (2021)", y="Percent Fully Vaccinated",
       linetype=NULL,color=NULL)

## Conduct SC analysis for Ohio, excluding other lottery states:
synth_ohio <- lang_0624 %>% dplyr::filter(type != "Other Lottery State") %>%
  ## initialize SC object by specifying outcome, unit variable, time variable, 
  ### and when/where intervention turns on:
  synthetic_control(outcome=people_fully_vaccinated_per_hundred,
                    unit=state,
                    time=centered_week,
                    i_unit="OH",
                    i_time=0,
                    generate_placebos=T) %>%
  ## create predictors for SC model:
  generate_predictor(time_window=-17,lag17=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-16,lag16=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-15,lag15=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-14,lag14=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-13,lag13=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-12,lag12=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-11,lag11=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-10,lag10=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-9,lag09=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-8,lag08=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-7,lag07=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-6,lag06=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-5,lag05=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-4,lag04=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-3,lag03=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-2,lag02=people_fully_vaccinated_per_hundred) %>%
  generate_predictor(time_window=-1,lag01=people_fully_vaccinated_per_hundred) %>%
  ## generate SC weights:
  generate_weights(optimization_window=(-17):(-1),
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6) %>%
  ## run SC:
  generate_control()

## Pull weights and print in descending order
synth_ohio %>% grab_unit_weights() %>% 
  dplyr::filter(weight > 0.001) %>% dplyr::arrange(desc(weight))

## Plot the weights for control units and variables.
#| fig-cap: "Control unit and variable weights for synthetic control fit of Ohio's fully vaccinated percentage prior to the lottery implementation, May 12, 2021."
#| fig-alt: "Left: bar plot of control unit weights. The positive weights are only for Kansas (0.256), Wisconsin (0.192), Virginia (0.173), Georgia (0.168), Iowa (0.066), Hawaii (0.061), Pennsylvania (0.056), and Connecticut (0.029). Right: bar plot of variable weights, from lag01 down to lag17. The weight on lag01 is above 0.20, falling roughly exponentially to about 0 for lag17."
## Plot weights on control units and variables:
synth_ohio %>% plot_weights()

## Examine fit:
synth_ohio %>% grab_balance_table()

synth_ohio %>% plot_trends() +
  labs(x="Weeks from Lottery Announcement",
       y="Percent Fully Vaccinated",
       title=NULL) +
  theme_bw()

synth_ohio %>% plot_differences() +
  labs(x="Weeks from Lottery Announcement",
       y="Difference in Percent Fully Vaccinated, Observed–Synthetic",
       title=NULL) +
  theme_bw()

synth_ohio %>% plot_placebos() +
  labs(x="Weeks from Lottery Announcement",
       y="Difference in Percent Fully Vaccinated, Observed–Synthetic",
       title=NULL) +
  theme_bw()

synth_ohio %>% plot_placebos(prune=FALSE) +
  labs(x="Weeks from Lottery Announcement",
       y="Difference in Percent Fully Vaccinated, Observed–Synthetic",
       title=NULL) +
  theme_bw()

synth_ohio %>% plot_mspe_ratio() +
  labs(title=NULL) +
  theme_bw()

## Get all pre- and post-intervention MSPEs, ratios, and ranks:
synth_ohio %>% grab_significance()
## Get pre- and post-intervention MSPEs, ratio, and p-value 
### for actual treated unit(s) only:
synth_ohio %>% grab_significance() %>%
  dplyr::filter(type=="Treated")

## Pull the synthetic control results for each time point:
SC_res <- synth_ohio %>% grab_synthetic_control(placebo=TRUE) %>%
  mutate(diff=real_y-synth_y)
SC_res

## For each time period, compute a permutation p-value
time_an <- function(time) {
  SC_res_time <- SC_res %>% dplyr::filter(time_unit==time)
  Est <- SC_res_time %>% dplyr::filter(.placebo==0) %>% pull(diff)
  P.Val <- mean(abs(SC_res_time %>% pull(diff)) >= abs(Est))
  return(c(time_unit=time,estimate=Est,p.value=P.Val))
}
by_time_res <- as_tibble(t(sapply(X=unique(SC_res %>% pull(time_unit)),
                                  FUN=time_an)))
by_time_res

ggplot(data=by_time_res) +
  geom_point(mapping=aes(x=time_unit, y=p.value), size=1.5) +
  theme_bw() +
  geom_hline(yintercept=0.05, color="red", 
             linetype="dotted", size=1.3) +
  geom_vline(xintercept=0, linetype="dotted", 
             linewidth=1.3) +
  scale_y_continuous(limits=c(0,1), expand=expansion(),
                     breaks=seq(0,1,by=0.2)) +
  labs(title=NULL,
       x="Weeks from Lottery Announcement",
       y="Placebo Test P-Values")

## Create synthetic control fit with covariates:
synth_oh_cov <- lang_0624 %>% dplyr::filter(type != "Other Lottery State") %>%
  ## initialize SC object by specifying outcome, unit variable, time variable, 
  ### and when/where intervention turns on:
  synthetic_control(outcome=people_fully_vaccinated_per_hundred,
                    unit=state,
                    time=centered_week,
                    i_unit="OH",
                    i_time=0,
                    generate_placebos=T) %>%
  ## create predictors for SC model:
  ### four-week groups for outcome:
  generate_predictor(time_window=-17:-14,lag17_14=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE)) %>%
  generate_predictor(time_window=-13:-10,lag13_10=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE)) %>%
  generate_predictor(time_window=-9:-6,lag9_6=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE)) %>%
  generate_predictor(time_window=-5:-2,lag5_2=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE)) %>%
  ### outcome, case rate, death rate, and vaccination rate in week prior:
  generate_predictor(time_window=-1,
                     lag01=people_fully_vaccinated_per_hundred,
                     total_cases=tot_cases_per_million,
                     total_deaths=tot_death_per_million,
                     vax_rate=daily_vaccinations_per_million) %>%
  ### four previous week average for cases and deaths:
  generate_predictor(time_window=-5:-2,
                     recent_cases=mean(new_case_per_million, na.rm=TRUE),
                     recent_deaths=mean(new_death_per_million, na.rm=TRUE)) %>%
  ## generate SC weights on same optimization window:
  generate_weights(optimization_window=-17:-1,
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6) %>%
  ## run SC:
  generate_control()

### Examine fit and results:
synth_oh_cov %>% plot_weights()
synth_oh_cov %>% grab_balance_table()
synth_oh_cov %>% plot_trends()
synth_oh_cov %>% plot_differences()
synth_oh_cov %>% plot_placebos()
synth_oh_cov %>% grab_significance() %>% 
  dplyr::filter(type=="Treated")

### First, create a dataset with a treatment indicator:
OH_data <- lang_0624 %>% 
  dplyr::filter(type2 %in% c("Ohio","Non-Lottery State")) %>%
  mutate(treated=if_else(state=="OH" & rel_week >= 0,1,0))
### Then, run the augsynth function:
OH_as <- augsynth(form=people_fully_vaccinated_per_hundred~treated, # outcome~treatment
                  unit=stateF, # units, as a factor variable
                  time=week, # time period variable
                  data=OH_data, # data set
                  progfunc="None", # fits without any outcome model
                  scm=TRUE, # fits with SC weighting
                  fixedeff=FALSE) # fits without de-meaning/intercepts
## Prints average ATT estimate in post-intervention periods:
OH_as
## Prints estimate for each time period with conformal inference CI:
summary(OH_as)

## The original SC fit from the tidysynth package:
#| fig-cap: "Time series of the difference, Observed – Synthetic Ohio, in percent fully vaccinated rate by week (centered at lottery announcement date, May 12, 2021)."
#| fig-alt: "Line plot that fluctuates near 0 from Week -17 to -6, is at -0.6 at Week -5, between -0.25 and 0.3 from Weeks -4 to -2, and then decreases steadily to -1.25 at Week 3 and stays near there until Week 6."
synth_ohio %>% plot_differences() +
  labs(x="Weeks from Lottery Announcement",
       y="Difference in Percent Fully Vaccinated, Observed–Synthetic",
       title=NULL) +
  theme_bw()

## The new SC fit from the augsynth package:
#| fig-cap: "Time series of the difference, Observed – Synthetic Ohio, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that is the same as the previous plot, except the x-axis goes from 0 to 25, with a vertical line at 19."
plot(OH_as, 
     inf=FALSE) # Suppresses plotting of conformal inference CIs

### Run the augsynth fit with a ridge outcome model and SC
OH_as_r <- augsynth(form=people_fully_vaccinated_per_hundred~treated, # outcome~treatment
                  unit=stateF, # units, as a factor variable
                  time=week, # time period variable
                  data=OH_data, # data set
                  progfunc="Ridge", # fits with ridge outcome model
                  scm=TRUE, # fits with SC weighting
                  fixedeff=FALSE) # fits without de-meaning/intercepts
## Prints average ATT estimate in post-intervention periods:
OH_as_r

## The ridge-adjusted SC fit from the augsynth package:
#| fig-cap: "Time series of the difference, Observed – Ridge-adjusted Synthetic Ohio, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that is very similar to the previous plot."
plot(OH_as_r, 
     inf=FALSE) # Suppresses plotting of conformal inference CIs

### Run the augsynth fit with fixed effects and no outcome model
OH_as_fe <- augsynth(form=people_fully_vaccinated_per_hundred~treated, # outcome~treatment
                  unit=stateF, # units, as a factor variable
                  time=week, # time period variable
                  data=OH_data, # data set
                  progfunc="None", # fits without outcome model
                  scm=TRUE, # fits with SC weighting
                  fixedeff=TRUE) # fits with de-meaning/intercepts
## Prints average ATT estimate in post-intervention periods:
OH_as_fe

## The de-meaned SC fit from the augsynth package:
#| fig-cap: "Time series of the difference, Observed – de-meaned Synthetic Ohio, in percent fully vaccinated rate by week, starting from the week ending 1/17/21."
#| fig-alt: "Line plot that fluctuates near 0, never exceeding 0.5 in either direction, from Week 0 to 18, and then steadily decreases to around -2 in Week 22 and stays around there through Week 25."
plot(OH_as_fe, 
     inf=FALSE) # Suppresses plotting of conformal inference CIs

ggplot(data=lang_0624,
       mapping=aes(group=state, linetype=type2, color=type2,
                   alpha=type2, linewidth=type2,
                   x=last_day,y=people_fully_vaccinated_per_hundred)) +
  scale_alpha_manual(name=NULL, breaks=c("Ohio","New Mexico","Maine",
                                         "Other Lottery State",
                                         "Non-Lottery State"),
                     values=c(1,1,1,0.5,0.8)) + 
  scale_linewidth_manual(name=NULL, breaks=c("Ohio","New Mexico","Maine",
                                         "Other Lottery State",
                                         "Non-Lottery State"),
                         values=c(1.3,1.3,1.3,1,1)) +
  geom_line() + theme_bw() +
  geom_vline(data=lang_0624 %>% 
               dplyr::filter(type2 %in% c("Ohio","New Mexico","Maine"),
                                              rel_week==0),
             mapping=aes(xintercept=lott_date, group=type2, color=type2),
             linetype="dotted") +
  labs(x="Day (2021)", y="Percent Fully Vaccinated",
       linetype=NULL,color=NULL)

## Create data set for New Mexico as treated unit:
NM_data <- lang_0624 %>% 
  dplyr::filter(type2 %in% c("New Mexico","Non-Lottery State")) %>%
  mutate(treated=if_else(state=="NM" & rel_week >= 0,1,0))
## Create data set for Maine as treated unit:
ME_data <- lang_0624 %>% 
  dplyr::filter(type2 %in% c("Maine","Non-Lottery State")) %>%
  mutate(treated=if_else(state=="ME" & rel_week >= 0,1,0))

## Fit standard SCs using augsynth:
NM_as <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=NM_data,
                  progfunc="None", scm=TRUE, fixedeff=FALSE)
ME_as <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="None", scm=TRUE, fixedeff=FALSE)

plot(NM_as, 
     inf=FALSE)

## Create a data set with the weights and sort largest to smallest in absolute value.
NM_as_w <- tibble(State=rownames(NM_as$weights),
                  Weight=NM_as$weights[,1]) %>%
  arrange(desc(abs(Weight)))
## Print results:
NM_as_w

## Fit de-meaned SC using augsynth:
NM_as_fe <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=NM_data,
                  progfunc="None", scm=TRUE, fixedeff=TRUE)

plot(NM_as_fe, 
     inf=FALSE)

## Create a data set with the weights and sort largest to smallest in absolute value.
NM_as_fe_w <- tibble(State=rownames(NM_as_fe$weights),
                  Weight=NM_as_fe$weights[,1]) %>%
  arrange(desc(abs(Weight)))
## Print results:
NM_as_fe_w

## Fit ridge-adjusted SC using augsynth:
NM_as_r <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=NM_data,
                  progfunc="Ridge", scm=TRUE, fixedeff=FALSE)

plot(NM_as_r, 
     inf=FALSE)

## Create a data set with the weights and sort largest to smallest in absolute value.
NM_as_r_w <- tibble(State=rownames(NM_as_r$weights),
                  Weight=NM_as_r$weights[,1]) %>%
  arrange(desc(abs(Weight)))
## Print results:
NM_as_r_w

plot(ME_as, 
     inf=FALSE)

## Fit de-meaned SC using augsynth:
ME_as_fe <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="None", scm=TRUE, fixedeff=TRUE)

plot(ME_as_fe, 
     inf=FALSE)

## Fit ridge-adjusted SC using augsynth:
ME_as_r <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="Ridge", scm=TRUE, fixedeff=FALSE)

plot(ME_as_r, 
     inf=FALSE)

## Fit de-meaned and ridge-adjusted SC using augsynth:
ME_as_r_fe <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="Ridge", scm=TRUE, fixedeff=TRUE)

plot(ME_as_r_fe, 
     inf=FALSE)

# install.packages("gsynth") # Run once if not yet installed
library(gsynth)

## Then we can fit the model using the GSYN option for progfunc:
ME_gsynth <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                  unit=stateF,
                  time=week,
                  data=ME_data,
                  progfunc="GSYN")

plot(ME_gsynth, 
     inf=FALSE)

## Create data set with indicator for treatment
Mult_data <- lang_0624 %>% 
  mutate(treated=if_else(!lottery,0,if_else(rel_week>=0,1,0)))
## Fit augmented SC on full data set:
Mult_as <- augsynth(form=people_fully_vaccinated_per_hundred~treated,
                    unit=stateF,
                    time=week,
                    data=Mult_data)
## Print results and summary:
Mult_as
summary(Mult_as)

plot(Mult_as)
