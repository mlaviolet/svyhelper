# scratch work for implementing NCHS standards

library(tidyverse)
library(srvyr)

load("R:/OCPH/EPI/BHSDM/Group/BRFSS/2017/Final/WorkingDataFile/nhbrfs17_working.Rdata")

nh_svy <- as_survey_design(nh17brfs, strata = X.STSTR, weights = X.LLCPWT)

valid_n <- function(x) unweighted(length(na.omit(x)))
# nh_svy %>% summarize(n = valid_n(DIABETE3))
# 
# nh_svy %>% 
#   summarize(n = valid_n(DIABETE3))

svy_freq <- function(design, grp, svyvar, outcome) {
  grp_var <- sym(grp)
  ind_var <- sym(svyvar)
  design %>%
    group_by(!!grp_var) %>% 
    summarize(n = unweighted(length(na.omit(!!ind_var))), # drops NA in indicator
              pct = survey_mean(!!ind_var == outcome,
                                vartype = c("var", "ci"), na.rm = TRUE, 
                                proportion = TRUE, prop_method = "beta")) %>% 
    drop_na() %>% # drop NA in group
    mutate(n_eff = pct * (1 - pct) / pct_var,
           ci_width = pct_upp - pct_low,
           ci_rel_width = ci_width / pct) %>% 
    # mutate_at(c(vars(starts_with("pct"), "ci_width")),
    #           function(x) round(100 * x, 1)) %>% 
    rename_at(vars(grp), ~ "Population") %>% 
    # add indicator as column
    mutate(indicator = outcome) %>% 
    select(indicator, Population, n, n_eff, everything(), -pct_var) 
  }



svy_freq(nh_svy, "SEX", "DIABETE3", "Told have diabetes")
# use near() to determine if zero events--pct should be 0 or 1

# proportion
svy_prop <- nh_svy %>% 
  group_by(SEX) %>% 
  summarize(n = unweighted(length(na.omit(SEX, DIABETE3))),
            pct = survey_mean(DIABETE3 == "Told have diabetes", na.rm = TRUE,
                              vartype = c("var", "ci"), proportion = TRUE, 
                              prop_method = "beta"))
## compute CI length and relative CI 

# total
svy_ttl <- nh_svy %>% 
  summarize(ttl = survey_total(DIABETE3 == "Told have diabetes", na.rm = TRUE,
                              vartype = "ci", deff = TRUE))

# unweighted sample size; remove missings
nh_svy %>% 
  group_by(DIABETE3) %>% 
  summarize(n = na.omit(length(na.omitunweighted(n()))) %>% 
  summarize(n = sum(n, na.rm = TRUE))
  
# degrees of freedom
survey::degf(nh_svy)

# number of events == 0?
# get from table of unweighted counts

