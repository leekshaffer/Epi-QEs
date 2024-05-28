### lottery-sc.R ###
### Analyzing COVID-19 state vaccine lotteries using synthetic control methods ###
### Lee Kennedy-Shaffer ###
### May 28, 2024 ###

## See Lang et al. 2023 (https://doi.org/10.1017/XPS.2021.32) for context and
## https://doi.org/10.7910/DVN/QYXN9L for their replication files.

## Libraries:
require(tidyverse)
require(tidysynth)

## Load data:
load(file="data/lottery_lang.Rda")
Ohio_ann <- lang_ann_dates %>% dplyr::filter(state=="OH") %>% pull(lott_date)
Ohio_ann_week <- lang_ann_dates %>% dplyr::filter(state=="OH") %>% pull(lott_week)

## Plotting:
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

## Note: on laptop, ran in ~1 minute

## Investigate weights:
synth_ohio %>% grab_unit_weights() %>% 
  dplyr::filter(weight > 0.001) %>% dplyr::arrange(desc(weight))
synth_ohio %>% plot_weights()

## Investigate fit:
synth_ohio %>% grab_balance_table()
synth_ohio %>% plot_trends()
synth_ohio %>% plot_differences()

## Placebo Tests
synth_ohio %>% plot_placebos()
synth_ohio %>% plot_placebos(prune=FALSE)
synth_ohio %>% plot_mspe_ratio()
synth_ohio %>% grab_significance() %>% dplyr::filter(unit_name=="OH")

## More specific tests/estimates:
SC_res <- synth_ohio %>% grab_synthetic_control(placebo=TRUE) %>%
  mutate(diff=real_y-synth_y)

times <- unique(SC_res %>% pull(time_unit))
time_an <- function(time) {
  SC_res_time <- SC_res %>% dplyr::filter(time_unit==time)
  Est <- SC_res_time %>% dplyr::filter(.placebo==0) %>% pull(diff)
  P.Val <- mean(abs(SC_res_time %>% pull(diff)) >= abs(Est))
  return(c(time_unit=time,estimate=Est,p.value=P.Val))
}
by_time_res <- as_tibble(t(sapply(X=times, FUN=time_an)))


### Add some covariates to SC:
synth_oh_2 <- lang_0624 %>% dplyr::filter(type != "Other Lottery State") %>%
  ## initialize SC object by specifying outcome, unit variable, time variable, 
  ### and when/where intervention turns on:
  synthetic_control(outcome=people_fully_vaccinated_per_hundred,
                    unit=state,
                    time=centered_week,
                    i_unit="OH",
                    i_time=0,
                    generate_placebos=T) %>%
  ## create predictors for SC model:
  generate_predictor(time_window=-17:-14,lag17_14=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE)) %>%
  generate_predictor(time_window=-13:-10,lag13_10=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE)) %>%
  generate_predictor(time_window=-9:-6,lag9_6=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE)) %>%
  generate_predictor(time_window=-5:-2,lag5_2=mean(people_fully_vaccinated_per_hundred, na.rm=TRUE)) %>%
  generate_predictor(time_window=-1,
                     lag01=people_fully_vaccinated_per_hundred,
                     cases=tot_cases_per_million,
                     deaths=tot_death_per_million,
                     vax_num=daily_vaccinations_per_million) %>%
  generate_predictor(time_window=-5:-2,
                     recent_cases=mean(new_case_per_million, na.rm=TRUE),
                     recent_deaths=mean(new_death_per_million, na.rm=TRUE)) %>%
  ## generate SC weights:
  generate_weights(optimization_window=-17:-1,
                   margin_ipop = .02,sigf_ipop = 7,bound_ipop = 6) %>%
  ## run SC:
  generate_control()

## Weights, trends, results:
synth_oh_2 %>% grab_unit_weights() %>% 
  dplyr::filter(weight > 0.001) %>% dplyr::arrange(desc(weight))
synth_oh_2 %>% grab_balance_table()
synth_oh_2 %>% plot_trends()
synth_oh_2 %>% plot_differences()
synth_oh_2 %>% plot_placebos()
