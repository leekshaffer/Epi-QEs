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
load(file="data/pcv.Rda")
