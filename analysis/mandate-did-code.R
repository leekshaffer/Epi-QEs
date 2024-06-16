load(file="../data/mandate.Rda")

## If you have not installed these packages before,
##  run the following line:
# install.packages(c("bacondecomp","did2s","DIDmultiplegt"))
## Either way, require the libraries:
require(tidyverse)
require(knitr)
require(lme4)
require(bacondecomp)
require(did2s)
require(DIDmultiplegt)

## Plot time series of mandates themselves:
#| fig-cap: "Plot of state employee vaccination mandate timings, U.S. states, June 2021–February 2022"
#| fig-alt: "A bar plot with bars for each state over the time range specified, with twenty states switching from red to blue sometime between August 2021 and October 2021, and the rest remaining red throughout."
ggplot(data=Vax_weekly, 
       mapping=aes(x=End_Date,y=State, fill=mandate)) + 
  geom_tile() +
  scale_x_date(name="Date", date_breaks="4 weeks", expand=c(0,0),
               limits=c(as.Date("2021-06-05"),as.Date("2022-03-05")),
               date_labels="%m/%d/%y") +
  theme_bw() + guides(fill="none")

## Get lists of the states in the data and the states with a mandate at some point
states <- unique(Vax_weekly %>% pull(State))
mandate_states <- unique(Vax_weekly %>% 
                           dplyr::filter(mandate) %>% pull(State))

## Plot time series of first-dose proportion by state:
#| fig-cap: "Plot of the proportion of U.S. adults with at least one COVID-19 vaccine dose, by state and state vaccine mandate status, June 2021–February 2022"
#| fig-alt: "A line plot with lines for each state for dates from June 2021 to February 2022, and first-dose percentages generally increasing from a range around 45–80 at the beginning to a range around 70–90 at the end."
ggplot() +
  geom_line(data=Vax_weekly %>% 
              dplyr::filter(!(State %in% mandate_states)),
            mapping=aes(x=End_Date, y=D1P, group=State),
            color="grey50", linetype="dashed") +
  geom_line(data=Vax_weekly %>% 
              dplyr::filter(State %in% mandate_states),
            mapping=aes(x=End_Date, y=D1P, group=State,
                        color=mandate),
            linetype="solid") +
  scale_color_manual(name="Mandate",
                     breaks=c(TRUE,FALSE),
                     values=c("blue","grey50"),
                     labels=c("Post-Mandate","Pre-/No Mandate")) +
  scale_y_continuous(name="First-Dose Percentage",
                     limits=c(0,100), expand=c(0,0)) +
  scale_x_date(name="Date", date_breaks="6 weeks", 
               expand=c(0,0),
               limits=c(as.Date("2021-06-05"),as.Date("2022-03-05")),
               date_labels="%m/%d/%y") +
  theme_bw()

## Exclude New Hampshire, Pennsylvania, Washington:
Vax_adj <- Vax_weekly %>% 
  dplyr::filter(!(State %in% c("NH","PA","WA")))
## Select weeks for analysis:
Yr_Wk_Sel <- paste0("2021_",(25:42))

## Plot time series of first-dose proportion by state in smaller time window:
#| fig-cap: "Plot of the proportion of U.S. adults with at least one COVID-19 vaccine dose, by state and state vaccine mandate status, July 2021–October 2021"
#| fig-alt: "A line plot with lines for each state for dates from July 2021 to October 2021, and first-dose percentages generally increasing from a range around 45–85 at the beginning to a range around 60–90 at the end."
ggplot() +
  geom_line(data=Vax_adj %>% 
              dplyr::filter(!(State %in% mandate_states), 
                            Yr_Wk %in% Yr_Wk_Sel),
            mapping=aes(x=End_Date, y=D1P, group=State),
            color="grey50", linetype="dashed") +
  geom_line(data=Vax_adj %>% 
              dplyr::filter(State %in% mandate_states, 
                            Yr_Wk %in% Yr_Wk_Sel),
            mapping=aes(x=End_Date, y=D1P, group=State,
                        color=mandate),
            linetype="solid") +
  scale_color_manual(name="Mandate",
                     breaks=c(TRUE,FALSE),
                     values=c("blue","grey50"),
                     labels=c("Post-Mandate","Pre-/No Mandate")) +
  scale_y_continuous(name="Full Vaccination Percentage",
                     limits=c(0,100), expand=c(0,0)) +
  scale_x_date(name="Date", date_breaks="3 weeks", expand=c(0,0),
               limits=c(as.Date("2021-07-03"),as.Date("2021-10-24")),
               date_labels="%m/%d/%y") +
  theme_bw()

## Fit TWFE model with no covariates:
TWFE_D1P <- lmer(D1P~factor(MMWR_week)+State+mandate+(1|State),
                 data=Vax_weekly %>% 
                   dplyr::filter(Yr_Wk %in% Yr_Wk_Sel))
## Get 95% CI:
TWFE_D1P_CI <- confint(TWFE_D1P, parm="mandateTRUE", level=0.95)
## Fit TWFE model with prior-week as covariate:
TWFE_D1P_ctrl <- lmer(D1P~D1P_prior+factor(MMWR_week)+State+
                        mandate+(1|State),
                      data=Vax_weekly %>% 
                        dplyr::filter(Yr_Wk %in% Yr_Wk_Sel))
TWFE_D1P_ctrl_CI <- confint(TWFE_D1P_ctrl, 
                            parm="mandateTRUE", level=0.95)

## Summarize results from the two models:
TWFE_results <- tibble(Model=c("Fixed Effects Only",
                               "Fixed Effects + Prior Week Value"),
                       Estimate=format(c(summary(TWFE_D1P)$coefficients["mandateTRUE","Estimate"],
                                  summary(TWFE_D1P_ctrl)$coefficients["mandateTRUE","Estimate"]), digits=3, nsmall=3),
                       `95% CI`=c(paste(format(TWFE_D1P_CI["mandateTRUE",], digits=3, nsmall=3), collapse=", "),
                                  paste(format(TWFE_D1P_ctrl_CI["mandateTRUE",], digits=3, nsmall=3), collapse=", ")))

## Print the formatted table:
knitr::kable(TWFE_results,
             caption="Table 1. Results from TWFE models on first-dose percentage.")

## Conduct the decomposition and print summary:
bacon <- bacon(D1P~mandate,
               data=Vax_adj %>% 
                 dplyr::filter(Yr_Wk %in% Yr_Wk_Sel),
               id_var="State",
               time_var="MMWR_week")
## Full decomposition:
bacon

## Plot decomposition results:
#| fig-cap: "Goodman-Bacon decomposition plot for TWFE model with no covariates"
#| fig-alt: "Scatter plot of 2x2 DID weight vs estimate with three types of points: Earlier vs Later Treated, with estimate values from -4.5 to 2 and weights from 0 to 0.025; Later vs Earlier Treated, with estimate values from -4 to 3 and weights from 0 to 0.02; and Treated vs Untreated, with estimate values from -5.5 to 1 and weights from 0.04 to 0.21."
ggplot(data=bacon, 
       mapping=aes(x=estimate,y=weight,
                   shape=type,color=type)) +
  geom_point() + theme_bw() +
  labs(x="2x2 DID Estimate", y="Weight",
       shape="Comparison Type",
       color="Comparison Type")

## Get total weight as a treated group:
Ov_Wt <- bacon %>% group_by(treated) %>%
  dplyr::summarize(PosWeight=sum(weight)) %>%
  rename(`Treatment Time (Week)`=treated) %>%
  ungroup() %>%
## Add a column with total weight as control group:
  left_join(bacon %>% group_by(untreated) %>%
              dplyr::summarize(NegWeight=sum(weight)),
            by=join_by(`Treatment Time (Week)`==untreated)) %>%
## Subtract to get overall weight
  mutate(Weight=PosWeight-NegWeight)

## Plot decomposition results:
#| fig-cap: "Goodman-Bacon decomposition plot of overall weights on each timing group for TWFE model with no covariates"
#| fig-alt: "Scatter plot of weight vs. treatment time for each timing group. The values are positive, between 0.05 and 0.20, for all treatment weeks from 31 to 39 (with no points for 34 or 37). The value is around -0.02 for week 42."
ggplot(data=Ov_Wt, 
       mapping=aes(x=`Treatment Time (Week)`,y=Weight)) +
  scale_x_continuous(limits=c(30,42),breaks=seq(30,42,by=2)) +
  geom_point(size=2.5, shape="triangle") + theme_bw()

## Run event_study function with estimator="all":
ES <- event_study(data=Vax_adj %>% dplyr::filter(Yr_Wk %in% Yr_Wk_Sel) %>%
                    left_join(Vax_weekly %>% dplyr::filter(LeadLag==0) %>% 
                                dplyr::select(State,MMWR_week) %>%
                                rename(First_Week=MMWR_week),
                              by=join_by(State)) %>%
                    left_join(tibble(State=unique(Vax_weekly %>% pull(State))) %>%
                                arrange(State) %>%
                                mutate(StateID=1:length(unique(Vax_weekly %>% pull(State)))),
                              by=join_by(State)),
                  yname="SCP",
                  idname="StateID",
                  gname="First_Week",
                  tname="MMWR_week",
                  estimator="all")
## List all results:
ES
## Pick out a specific method:
ES %>% dplyr::filter(estimator=="Borusyak, Jaravel, Spiess (2021)")

## Plot the results of the various estimators:
#| fig-cap: "Event-study results from various staggered adoption methods on the effect of state employee COVID-19 vaccine mandates, event times from -16 to 12."
#| fig-alt: "Six event-study plots showing effect estimates and 95% confidence intervals at each lead/lag time from -16 to 12. The plots are labelled: TWFE, Borusyak, Jaravel, Spiess (2021), Callway and Sant'Anna (2020), Gardner (2021), Roth and Sant'Anna (2021), and Sun and Abraham (2020)."
plot_event_study(ES)

## Plot the results for just the dynamic estimator:
#| fig-cap: "Event-study results using the dynamic specification described in Borusyak et al. (2021/2024) on the effect of state employee COVID-19 vaccine mandates, event times from -10 to 8."
#| fig-alt: "A single event-study plot showing effect estimates and 95% confidence intervals at each lead/lag time from -10 to 8. The event times less than 0 tend to have positive estimates with CIs crossing or nearly crossing 0, and those greater than 0 tend to have negative estimates with CIs crossing or nearly crossing 0. The CIs are narrowest near event time 0, and the estimates are closest to 0 there."
plot_event_study(ES %>% 
                   dplyr::filter(estimator=="Borusyak, Jaravel, Spiess (2021)"),
                 horizon=c(-10,8))

## Plot the results for just the Sun and Abraham estimator:
#| fig-cap: "Event-study results using the estimator from Sun and Abraham (2020) on the effect of state employee COVID-19 vaccine mandates, event times from -10 to 8."
#| fig-alt: "A single event-study plot showing effect estimates and 95% confidence intervals at each lead/lag time from -10 to 8. The event times less than 0 tend to have positive estimates with CIs crossing or nearly crossing 0, and those greater than 0 tend to have negative estimates with CIs crossing or nearly crossing 0. The CIs are narrowest near event time 0, and the estimates are closest to 0 there."
plot_event_study(ES %>% 
                   dplyr::filter(estimator=="Sun and Abraham (2020)"),
                 horizon=c(-10,8))

## Plot the results for just the Callaway abnd Sant'Anna estimator:
#| fig-cap: "Event-study results using the estimator from Callaway and Sant'Anna (2020) on the effect of state employee COVID-19 vaccine mandates, event times from -10 to 8."
#| fig-alt: "A single event-study plot showing effect estimates and 95% confidence intervals at each lead/lag time from -10 to 8. The event times less than 0 tend to have estimates near 0 with (mostly) fairly narrow CIs, and those greater than 0 tend to have negative estimates with wide CIs crossing or nearly crossing 0. The CIs are narrowest near event time 0, and the estimates are closest to 0 there."
plot_event_study(ES %>% 
                   dplyr::filter(estimator=="Callaway and Sant'Anna (2020)"),
                 horizon=c(-10,8))

## First, we prepare a data set with a variable with the First_Week of
##  mandate for each mandate state and a unique state ID number.
Vax_firstWeek <- Vax_adj %>% 
  dplyr::filter(Yr_Wk %in% Yr_Wk_Sel) %>%
  left_join(Vax_adj %>% 
              dplyr::filter(LeadLag==0) %>% 
              dplyr::select(State,MMWR_week) %>%
              rename(First_Week=MMWR_week),
            by=join_by(State)) %>%
  left_join(tibble(State=unique(Vax_adj %>% pull(State))) %>%
              arrange(State) %>%
              mutate(StateID=1:length(unique(Vax_adj %>% pull(State)))),
            by=join_by(State))
## Run analysis method:
dCdH <- did_multiplegt(df=Vax_firstWeek,
                       Y="D1P",
                       G="StateID",
                       T="MMWR_week",
                       D="mandate",
                       placebo=10,
                       dynamic=7,
                       brep=0,
                       cluster="StateID")
## See full results:
dCdH

## Creating data set of placebo (in-time) effects. Note the time is negated to match usual event time notation.
placebo <- as_tibble(dCdH) %>% dplyr::select(starts_with("placebo")) %>%
  pivot_longer(cols=everything(),
               names_to="Lead",
               names_prefix="placebo_",
               values_to="Estimate") %>%
  mutate(`Event Time`=-1*as.numeric(Lead))
## Creating data set of single first-switch estimate.
estimate <- tibble(`Event Time`=1,
                   Estimate=dCdH$effect["treatment"])
## Creating data set of dynamic effects. Note the time is incremented by 1 to match usual event time notation where 1 is the first period on treatment.
dynamic <- as_tibble(dCdH) %>% dplyr::select(starts_with("dynamic")) %>%
  pivot_longer(cols=everything(),
               names_to="Lag",
               names_prefix="dynamic_",
               values_to="Estimate") %>%
  mutate(`Event Time`=as.numeric(Lag)+1)
## Combined event time data set from this method:
dCdH_comb <- bind_rows(placebo %>% dplyr::select(-Lead),
                       estimate,
                       dynamic %>% dplyr::select(-Lag))
dCdH_comb %>% arrange(`Event Time`)

## Plot the results of first-switching analysis:
#| fig-cap: "Event-study results using the First-Switching Estimator from de Chaisemartin and d'Haultfoeuille (2020) on the effect of state employee COVID-19 vaccine mandates, event times from -10 to 8."
#| fig-alt: "A single event-study plot showing effect estimates at each lead/lag time from -10 to 8. The event times less than 0 tend to have small negative estimates (between 0 and -0.125), except for time -9 which has an estimate of nearly 0.25. The event time of 1 has an estimate slightly below 0 as well. The estimates from event times 2 through 4 are around -0.15, from event times 5 to 7 are around -0.40, and from event time 8 is around -0.3."

ggplot(dCdH_comb) + geom_point(mapping=aes(x=`Event Time`, y=Estimate)) +
  theme_bw()

## Run the following code only if you are able to install the 
###  DIDmultiplegtDYN package. Otherwise, delete or turn eval to false
###  to avoid errors.
#| eval: true

## Run the above analysis with the DIDmultiplegtDYN package instead:
# install.packages("DIDmultiplegtDYN")
require(DIDmultiplegtDYN)

dCdH_dyn <- did_multiplegt_dyn(df=Vax_firstWeek,
                               outcome="D1P",
                               group="StateID",
                               time="MMWR_week",
                               treatment="mandate",
                               effects=8,
                               placebo=8,
                               cluster="StateID",
                               graph_off=FALSE)
dCdH_dyn
