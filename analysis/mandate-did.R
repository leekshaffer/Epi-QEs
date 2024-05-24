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


