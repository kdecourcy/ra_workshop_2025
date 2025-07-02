
library(tidyverse)
library(dplyr)
library(epiextractr)
library(epidatatools)
library(labelled)
library(here)
library(blsAPI)
library(openxlsx)
library(realtalk)

# set CPS years to analyze
cps_years <- 1979:2024

#org
org_vars <- c("year", "age", "emp", "selfemp", "selfinc", "orgwgt", "wage")

# cpi data (wage adjustments)
cpi <- c_cpi_u_extended_annual %>% 
  rename(cpi = c_cpi_u_extended)

# define CPI base
cpi_base <- cpi$cpi[cpi$year == "2024"] 

# import ORG data for sample size
raw_org <- load_org(cps_years, org_vars) %>% 
  filter(selfinc!=1 | is.na(selfinc), selfemp!=1, emp==1, !is.na(wage)) %>%
  # merge CPI-U-RS data
  left_join(cpi, by='year') %>% 
  # inflation adjust wages to $2024
  mutate(wgt = orgwgt/12,
         realwage = wage*(cpi_base/cpi))

# generate real annual average wages
avg_wage_real <- raw_org %>% 
  group_by(year) %>% 
  summarise(avg_wage = weighted.mean(x = realwage, w = wgt))
            
