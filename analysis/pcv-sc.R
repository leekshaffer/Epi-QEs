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
# require(gsynth)

## Load data:
load(file="data/pcv_mexico.Rda")
## Select <12 mo age group:
pcv_mexico_U12 <- pcv_mexico %>% dplyr::filter(age_group=="0-12m")

## Plot:
ggplot(pcv_mexico_U12,
       mapping=aes(x=date, y=value, group=series, 
                   color=target, alpha=target, linetype=target)) +
  geom_line() + 
  geom_vline(xintercept=unlist(key_dates_mexico %>% dplyr::select(-country)),
             linetype="dotted") +
  scale_alpha_manual(name="Disease Category",breaks=c("Pneumonia","Control Series"), values=c(1,.6)) +
  labs(color="Disease Category", linetype="Disease Category", x="Date", y="Cases") +
  theme_bw()

ggplot(pcv_mexico_U12,
       mapping=aes(x=date, y=log_val, group=series, 
                   color=target, alpha=target, linetype=target)) +
  geom_line() + 
  geom_vline(xintercept=unlist(key_dates_mexico %>% dplyr::select(-country)),
             linetype="dotted") +
  scale_alpha_manual(name="Disease Category",breaks=c("Pneumonia","Control Series"), values=c(1,.6)) +
  labs(color="Disease Category", linetype="Disease Category", x="Date", y="Log Cases") +
  theme_bw()

## Set Up Regular SC:
synth_pcv <- pcv_mexico_U12 %>% 
  synthetic_control(outcome=log_val,
                    unit=series,
                    time=month_ctr,
                    i_unit="J12_18",
                    i_time=0,
                    generate_placebos=F)

### Quickly add predictors for each pre-intervention month:
preds1 <- pcv_mexico_U12 %>% dplyr::filter(target=="Pneumonia",month_ctr<0) %>% 
  pivot_wider(id_cols=month_ctr, names_from=series, values_from=log_val) %>%
  mutate(variable=paste0("log_val_",abs(month_ctr))) %>%
  dplyr::select(variable,!any_of(c("month_ctr","variable")))
preds2 <- pcv_mexico_U12 %>% dplyr::filter(target=="Control Series",month_ctr<0) %>% 
  pivot_wider(id_cols=month_ctr, names_from=series, values_from=log_val) %>%
  mutate(variable=paste0("log_val_",abs(month_ctr))) %>%
  dplyr::select(variable,!any_of(c("month_ctr","variable")))
synth_pcv$`.predictors` <- list(preds1,preds2)

### Run standard SC:
synth_pcv <- synth_pcv %>%
  generate_weights(optimization_window=(min(pcv_mexico_U12$month_ctr)):-1,
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6) %>%
  generate_control()

### Results from standard SC:
synth_pcv %>% plot_weights()
synth_pcv %>% plot_trends()
synth_pcv %>% plot_differences()


## Augmented SC:
#### Note need to use factor form of series variable
as_pcv <- augsynth(form=log_val~treated,
                         unit=seriesF,
                         time=month_ctr,
                         data=pcv_mexico_U12,
                         progfunc="none",scm=T)
as_pcv_wts <- tibble(series=rownames(as_pcv$weights),
                     weight=as_pcv$weights[,1]) %>%
  arrange(desc(weight))
as_pcv_wts
plot(as_pcv)
as_pcv_summ <- summary(as_pcv)
as_pcv_summ

## Ridge-Augmented SC:
as_pcv_ridge <- augsynth(form=log_val~treated,
                         unit=seriesF,
                         time=month_ctr,
                         data=pcv_mexico_U12,
                         progfunc="Ridge",scm=T)
plot(as_pcv_ridge)

## FE-Augmented SC:
as_pcv_fe <- augsynth(form=log_val~treated,
                      unit=seriesF,
                      time=month_ctr,
                      data=pcv_mexico_U12,
                      progfunc="none",scm=T, fixedeff=T)
plot(as_pcv_fe)

## Ridge-FE-Augmented SC:
as_pcv_ridge_fe <- augsynth(form=log_val~treated,
                      unit=seriesF,
                      time=month_ctr,
                      data=pcv_mexico_U12,
                      progfunc="ridge",scm=T, fixedeff=T)
plot(as_pcv_ridge_fe)



