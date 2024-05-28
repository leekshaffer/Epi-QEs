### pcv-sc.R ###
### Analyzing PCV impact using advanced SC methods ###
### Lee Kennedy-Shaffer ###
### May 28, 2024 ###

## See Bruhn et al. 2017 (https://doi.org/10.1073/pnas.1612833114) for context and
## https://github.com/weinbergerlab/InterventionEvaluatR for replication files and R package.

## Libraries:
require(tidyverse)
require(tidysynth)
require(augsynth)
require(gsynth)

## Load data:
load(file="data/pcv_chile.Rda")
## Target age group:
pcv_chile_02 <- pcv_chile %>% dplyr::filter(age_group=="<24m")
excl_series_02 <- pcv_chile_02 %>% group_by(series) %>% 
  dplyr::summarize(Excl=mean(is.na(value))) %>% dplyr::filter(Excl==1) %>% pull(series)
pcv_chile_02 <- pcv_chile_02 %>% dplyr::filter(!(series %in% excl_series_02))

## Plot:
ggplot(pcv_chile %>% dplyr::filter(series=="J12_18"), 
       mapping=aes(x=date, y=value, group=age_group, color=age_group)) +
  geom_line() + geom_vline(xintercept=c(as.Date("2011-01-01"),as.Date("2012-01-01")),
                           linetype="dotted") +
  labs(color="Age Group",x="Date", y="Cases") +
  theme_bw()

ggplot(pcv_chile_02,
       mapping=aes(x=date, y=value, group=series, 
                   color=target, alpha=target, linetype=target)) +
  geom_line() + geom_vline(xintercept=c(as.Date("2011-01-01"),as.Date("2012-01-01")),
                           linetype="dotted") +
  scale_alpha_manual(name="Disease Category",breaks=c("Pneumonia","Control Series"), values=c(1,.6)) +
  labs(color="Disease Category", linetype="Disease Category", x="Date", y="Cases") +
  theme_bw()

ggplot(pcv_chile_02,
       mapping=aes(x=date, y=log_val, group=series, 
                   color=target, alpha=target, linetype=target)) +
  geom_line() + geom_vline(xintercept=c(as.Date("2011-01-01"),as.Date("2012-01-01")),
                           linetype="dotted") +
  scale_alpha_manual(name="Disease Category",breaks=c("Pneumonia","Control Series"), values=c(1,.6)) +
  labs(color="Disease Category", linetype="Disease Category", x="Date", y="Log Cases") +
  theme_bw()

## Set Up Regular SC:
synth_pcv <- pcv_chile_02 %>% 
  synthetic_control(outcome=log_val,
                    unit=series,
                    time=month_ctr,
                    i_unit="J12_18",
                    i_time=0,
                    generate_placebos=F) %>%
  generate_predictor(time_window=-120,val120=log_val)

### Quickly add predictors for each pre-intervention month:
preds1 <- pcv_chile_02 %>% dplyr::filter(target=="Pneumonia",month_ctr<0) %>% 
  pivot_wider(id_cols=month_ctr, names_from=series, values_from=log_val) %>%
  mutate(variable=paste0("log_val_",abs(month_ctr))) %>%
  dplyr::select(variable,!any_of(c("month_ctr","variable")))
preds2 <- pcv_chile_02 %>% dplyr::filter(target=="Control Series",month_ctr<0) %>% 
  pivot_wider(id_cols=month_ctr, names_from=series, values_from=log_val) %>%
  mutate(variable=paste0("log_val_",abs(month_ctr))) %>%
  dplyr::select(variable,!any_of(c("month_ctr","variable")))
synth_pcv$`.predictors` <- list(preds1,preds2)

### Run SC:
synth_pcv <- synth_pcv %>%
  generate_weights(optimization_window=-120:-1,
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6) %>%
  generate_control()

synth_pcv %>% plot_weights
synth_pcv %>% plot_trends()
synth_pcv %>% plot_differences()


### Augmented SC:
#### Prep data by adding "treated" Column:
pcv_chile_02_as <- pcv_chile_02 %>% 
  mutate(treated=if_else(target=="Pneumonia" & month_ctr >= 0,1,0))

as_pcv <- augsynth(form=log_val~treated,
                   unit=series,
                   time=month_ctr,
                   data=pcv_chile_02_as,
                   progfunc="Ridge", scm=T, fixedeff=T)
as_pcv_wts <- tibble(series=rownames(as_pcv$weights),
                     weight=as_pcv$weights[,1]) %>%
  arrange(desc(weight))
as_pcv_wts
plot(as_pcv)
as_pcv_summ <- summary(as_pcv)
as_pcv_summ
as_pcv_summ$att

