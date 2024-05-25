### mandate-did.R ###
### Analyzing COVID-19 state vaccine mandates using advanced DID methods ###
### Lee Kennedy-Shaffer ###
### May 24, 2024 ###

## See Rains and Richards 2024 (https://doi.org/10.1073/pnas.2313610121) for context and
## https://doi.org/10.1073/pnas.2313610121 for their replication files.
## Note that the data and analysis here are not an exact replication of their data or analysis.

## Libraries:
require(tidyverse)
require(bacondecomp)
require(did2s)
require(DIDmultiplegt)

## load data:
load("data/mandate.Rda")

## Plot timeline:
ggplot(data=Vax_weekly, mapping=aes(x=End_Date,y=State, fill=mandate)) + geom_tile() +
  scale_x_date(name="Date", date_breaks="4 weeks", expand=c(0,0),
               limits=c(as.Date("2021-06-05"),as.Date("2022-03-05")),
               date_labels="%m/%d/%y") +
  theme_bw() + guides(fill="none")

## Mandate State Plot:
states <- unique(Vax_weekly %>% pull(State))
mandate_states <- unique(Vax_weekly %>% dplyr::filter(mandate) %>% pull(State))
ggplot() +
  geom_line(data=Vax_weekly %>% dplyr::filter(!(State %in% mandate_states)),
            mapping=aes(x=End_Date, y=SCP, group=State),
            color="grey50", linetype="dashed") +
  geom_line(data=Vax_weekly %>% dplyr::filter(State %in% mandate_states),
            mapping=aes(x=End_Date, y=SCP, group=State,
                        color=mandate),
            linetype="solid") +
  scale_color_manual(name="Mandate",
                       breaks=c(TRUE,FALSE),
                     values=c("blue","grey50"),
                       labels=c("Post-Mandate","Pre-/No Mandate")) +
  scale_y_continuous(name="Full Vaccination Percentage",limits=c(0,100), expand=c(0,0)) +
  scale_x_date(name="Date", date_breaks="6 weeks", expand=c(0,0),
               limits=c(as.Date("2021-06-05"),as.Date("2022-03-05")),
               date_labels="%m/%d/%y") +
  theme_bw() + labs(title="Vaccination Percentage by Week and Mandate Status")

## Mandate Start Weeks
Vax_weekly %>% dplyr::filter(LeadLag==0)

## Select Weeks for Analysis
Yr_Wk_Sel <- paste0("2021_",(25:45))

## Plot on Selected Weeks
ggplot() +
  geom_line(data=Vax_weekly %>% dplyr::filter(!(State %in% mandate_states), Yr_Wk %in% Yr_Wk_Sel),
            mapping=aes(x=End_Date, y=SCP, group=State),
            color="grey50", linetype="dashed") +
  geom_line(data=Vax_weekly %>% dplyr::filter(State %in% mandate_states, Yr_Wk %in% Yr_Wk_Sel),
            mapping=aes(x=End_Date, y=SCP, group=State,
                        color=mandate),
            linetype="solid") +
  scale_color_manual(name="Mandate",
                     breaks=c(TRUE,FALSE),
                     values=c("blue","grey50"),
                     labels=c("Post-Mandate","Pre-/No Mandate")) +
  scale_y_continuous(name="Full Vaccination Percentage",limits=c(0,100), expand=c(0,0)) +
  scale_x_date(name="Date", date_breaks="4 weeks", expand=c(0,0),
               limits=c(as.Date("2021-07-03"),as.Date("2021-11-13")),
               date_labels="%m/%d/%y") +
  theme_bw() + labs(title="Vaccination Percentage by Week and Mandate Status")

## First-Dose Outcome
ggplot() +
  geom_line(data=Vax_weekly %>% dplyr::filter(!(State %in% mandate_states), Yr_Wk %in% Yr_Wk_Sel),
            mapping=aes(x=End_Date, y=D1P, group=State),
            color="grey50", linetype="dashed") +
  geom_line(data=Vax_weekly %>% dplyr::filter(State %in% mandate_states, Yr_Wk %in% Yr_Wk_Sel),
            mapping=aes(x=End_Date, y=D1P, group=State,
                        color=mandate),
            linetype="solid") +
  scale_color_manual(name="Mandate",
                     breaks=c(TRUE,FALSE),
                     values=c("blue","grey50"),
                     labels=c("Post-Mandate","Pre-/No Mandate")) +
  scale_y_continuous(name="First Dose Administered Percentage",limits=c(0,100), expand=c(0,0)) +
  scale_x_date(name="Date", date_breaks="4 weeks", expand=c(0,0),
               limits=c(as.Date("2021-07-03"),as.Date("2021-11-13")),
               date_labels="%m/%d/%y") +
  theme_bw() + labs(title="Vaccination Percentage by Week and Mandate Status")

## TWFE on Selected Weeks
TWFE_SCP <- lm(SCP~factor(MMWR_week)+State+mandate,
               data=Vax_weekly %>% dplyr::filter(Yr_Wk %in% Yr_Wk_Sel))
TWFE_SCP
confint(TWFE_SCP, level=0.95)
TWFE_SCP_ctrl <- lm(SCP~SCP_prior+factor(MMWR_week)+State+mandate,
               data=Vax_weekly %>% dplyr::filter(Yr_Wk %in% Yr_Wk_Sel))
confint(TWFE_SCP_ctrl, level=0.95)
TWFE_SCP_lmer <- lmer(SCP~factor(MMWR_week)+State+mandate+(1|State),
                      data=Vax_weekly %>% dplyr::filter(Yr_Wk %in% Yr_Wk_Sel))
TWFE_SCP_lmer
confint(TWFE_SCP_lmer, level=0.95)
TWFE_SCP_ctrl_lmer <- lmer(SCP~SCP_prior+factor(MMWR_week)+State+mandate+(1|State),
                      data=Vax_weekly %>% dplyr::filter(Yr_Wk %in% Yr_Wk_Sel))
confint(TWFE_SCP_ctrl_lmer, level=0.95)

## Goodman-Bacon Decomposition
bacon <- bacon(SCP~mandate,
               data=Vax_weekly %>% dplyr::filter(Yr_Wk %in% Yr_Wk_Sel),
               id_var="State",
               time_var="MMWR_week")
bacon
ggplot(data=bacon, mapping=aes(x=estimate,y=weight,shape=type,color=type)) +
  geom_point() + theme_bw()

## Dynamic DID (Sun&Abraham, using did2s)
ES <- event_study(data=Vax_weekly %>% dplyr::filter(Yr_Wk %in% Yr_Wk_Sel) %>%
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
ES
plot_event_study(ES)

## dCdH
dCdH <- did_multiplegt(df=Vax_weekly %>% dplyr::filter(Yr_Wk %in% Yr_Wk_Sel) %>%
                         left_join(Vax_weekly %>% dplyr::filter(LeadLag==0) %>% 
                                     dplyr::select(State,MMWR_week) %>%
                                     rename(First_Week=MMWR_week),
                                   by=join_by(State)) %>%
                         left_join(tibble(State=unique(Vax_weekly %>% pull(State))) %>%
                                     arrange(State) %>%
                                     mutate(StateID=1:length(unique(Vax_weekly %>% pull(State)))),
                                   by=join_by(State)),
                       Y="SCP",
                       G="StateID",
                       T="MMWR_week",
                       D="mandate",
                       dynamic=10,
                       brep=100,
                       cluster="StateID")
dCdH




