### zika-did.R ###
### Analyzing zika birth rates using DID methods ###
### Lee Kennedy-Shaffer ###
### May 24, 2024 ###

## See Taddeo et al. 2022 (https://doi.org/10.4310/21-SII704) for context and
## https://doi.org/10.7910/DVN/ENG0IY for source data.

## Libraries:
require(tidyverse)
require(lme4)

### load data:
load("data/zika.Rda")

### Plot trends (cf. Fig. 2(a) and Fig. 2(b) in Taddeo)
ggplot(data=zika_summ, mapping=aes(x=Year, y=Rate, color=State, linetype=State)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(name="Birth Rate (per 1000)",
                     limits=c(0,17),
                     breaks=seq(0,16,by=4),
                     minor_breaks=seq(0,16,by=2)) +
  geom_vline(xintercept=2015, linetype="longdash", color="grey50")
ggplot(data=zika_summ, mapping=aes(x=Year, y=LogBirths, color=State, linetype=State)) +
  geom_line() +
  theme_bw() +
  scale_y_continuous(name="Log number of live births",
                     limits=c(10,12),
                     breaks=seq(10,12,by=1),
                     minor_breaks=seq(10,12,by=.25)) +
  geom_vline(xintercept=2015, linetype="longdash", color="grey50")

### Linear models
zika_lm <- lm(Rate~trt+year+interaction, data=zika_full)
summary(zika_lm)
confint(zika_lm, level=0.95)
### Mixed effects model for clustered CI:
zika_lmer <- lmer(Rate~trt+year+interaction+(1|Code), data=zika_full)
summary(zika_lmer)
confint(zika_lmer, level=0.95)
### Block-bootstrap for CI:
codes <- zika_full %>% dplyr::select(Code,State) %>% distinct() # get unique Codes by State
boot_data <- function() slice_sample(codes, by=State, prop=1, replace=TRUE) %>%
  expand_grid(StudyYear=rep(c(2014,2016))) %>%
  left_join(zika_full, by = join_by(Code, State, StudyYear))
set.seed(3671)
lm_boot <- replicate(n=1000,
                     expr=lm(Rate~trt+year+interaction,
                             data=boot_data())$coefficients["interaction"])
quantile(lm_boot, probs=c(0.5,0.025,0.975))

### Log-scale model
zika_log_lm <- lm(LogRate~trt+year+interaction, data=zika_full)
summary(zika_log_lm)
confint(zika_log_lm, level=0.95)
### Mixed effects model for clustered CI:
zika_log_lmer <- lmer(LogRate~trt+year+interaction+(1|Code), data=zika_full)
summary(zika_log_lmer)
confint(zika_log_lmer, level=0.95)
### Block-bootstrap for CI:
set.seed(98391)
log_boot <- replicate(n=1000,
                      expr=lm(LogRate~trt+year+interaction,
                              data=boot_data())$coefficients["interaction"])
quantile(log_boot, probs=c(0.5,0.025,0.975))

### Poisson glm model
zika_glm <- glm(Births~trt+year+interaction+offset(log(Pop)), data=zika_full,
                family=poisson(link="log"))
summary(zika_glm)
confint(zika_glm, level=0.95)
zika_glmer <- glmer(Births~trt+year+interaction+offset(log(Pop))+(1|Code), 
                    data=zika_full,
                    family=poisson(link="log"))
summary(zika_glmer)
confint(zika_glmer, level=0.95)
set.seed(4008335)
glm_boot <- replicate(n=1000,
                      expr=glm(Births~trt+year+interaction+offset(log(Pop)), data=boot_data(),
                               family=poisson(link="log"))$coefficients["interaction"])
quantile(glm_boot, probs=c(0.5,0.025,0.975))
