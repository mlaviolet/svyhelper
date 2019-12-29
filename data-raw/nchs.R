# scratch work for implementing NCHS standards

library(tidyverse)
library(srvyr)

load("R:/OCPH/EPI/BHSDM/Group/BRFSS/2017/Final/WorkingDataFile/nhbrfs17_working.Rdata")

nh_svy <- as_survey_design(nh17brfs, strata = X.STSTR, weights = X.LLCPWT)

# proportion
svy_prop <- nh_svy %>% 
  summarize(pct = survey_mean(DIABETE3 == "Told have diabetes", na.rm = TRUE,
                              vartype = "ci", proportion = TRUE, 
                              prop_method = "beta"))
## compute CI length and relative CI 

# total
svy_ttl <- nh_svy %>% 
  summarize(ttl = survey_total(DIABETE3 == "Told have diabetes", na.rm = TRUE,
                              vartype = "ci", deff = TRUE))

# unweighted sample size; remove missings
nh_svy %>% 
  group_by(DIABETE3) %>% 
  summarize(n = na.omit(unweighted(n()))) %>% 
  summarize(n = sum(n, na.rm = TRUE))
  
# degrees of freedom
survey::degf(nh_svy)

# number of events == 0?
# get from table of unweighted counts

