# This file runs the robustness of TWFE estimates
# C. Controling for tax implementation later in the year (in manuscript)
# D. Considering the implementation year as the adjustment period (in manuscript)

rm(list = ls())
source(file.path(codepath, "000path.r"))

 
tax <- read.csv(file.path(datapath,  "tax_changes_check.csv"))  %>% 
                separate(col = date, into = c("month", "day", "year"), sep = "/")  %>% 
                mutate(year_change = as.numeric(year) + 2000)  %>% 
                mutate(state = trimws(State))  %>% 
                select(-c(amount, tax_rate, year, State))  

crosswalk  <-  rbind(data.frame(state.abb, state.name), c("DC", "Washington, DC"))

tax  <- tax   %>% 
        merge(crosswalk, by.x = "state", by.y = "state.name", all.x = T)
                


dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))   %>% 
                merge(tax, by.x = c("state_abb", "year_change_per"), 
                by.y = c("state.abb", "year_change"), all.x = T)  %>% 
                mutate(late_year = ifelse(month > 7 & year == year_change_per, 1, 0), 
                late_year = ifelse(is.na(late_year) == T, 0, late_year)
                )
                

dat2 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv")) %>% 
                merge(tax, by.x = c("state_abb", "year_change_per"), 
                by.y = c("state.abb", "year_change"), all.x = T)  %>% 
                mutate(late_year = ifelse(month > 7 & year == year_change_per, 1, 0)
                )

start_year1 <- min(dat$year)
start_year2 <- min(dat2$year)

# formula for baseline specification
formula_base <- current_smoker ~ tax_change + late_year | 
                 year + msa_id

# formula with SFA
formula_sfa <- current_smoker ~ tax_change + late_year + per_barban05 | 
                 year + msa_id

formula_sfa2 <- current_smoker ~ tax_change + late_year + per_barban10 | 
                 year + msa_id


# formula with controls
formula_control <- current_smoker ~ tax_change + late_year +
                    cbsaunp00:factor(year) + factor98:factor(year) +
                    change_cursmk:factor(year) + log_msapop00:factor(year) + per_barban05 |  #nolint
                    year + msa_id

formula_control2 <- current_smoker ~ tax_change + late_year +
                    cbsaunp10:factor(year) + factor98:factor(year) +
                    change_cursmk:factor(year) + log_msapop10:factor(year) + per_barban10 |  #nolint
                    year + msa_id


for(i in 1:2) {
       if(i == 1){
              dat <- dat
              dat2 <- dat2
       }else{
              dat <- dat %>% filter(year_change_per != start_year1)
              dat2 <- dat2 %>% filter(year_change_per != start_year2)
       }
twfe1 <- feols(formula_base,
            subset(dat), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

twfe2 <- feols(formula_sfa,
            subset(dat), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

twfe3 <- feols(formula_control,
            subset(dat), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j



twfe4 <- feols(formula_base,
            subset(dat2), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

twfe5 <- feols(formula_sfa2,
            subset(dat2), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

twfe6 <- feols(formula_control2,
            subset(dat2), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

       if(i == 1) {
              setFixest_dict(c(current_smoker = "Panel A. % current smoker"))
              dict = c(tax_change = "Indicator for tax increase")

              etable(twfe1, twfe2, twfe3, twfe4, twfe5, twfe6, dict = dict,
              keep = c("%tax_change"), tex = TRUE, fitstat = c("n", "r2"),
              drop.section = c("fixef") , replace = T, # nolint
              style.tex = style.tex("aer"), 
              file = paste(outpath, "01reg_twfe_exclude1styear.tex", sep ="/"))
       }else{
              setFixest_dict(c(current_smoker = "Panel B. % current smoker (drop always treated)"))  # nolint
              dict = c(tax_change = "Indicator for tax increase")

              etable(twfe1, twfe2, twfe3, twfe4, twfe5, twfe6, dict = dict,
              keep = c("%tax_change"), tex = TRUE, fitstat = c("n", "r2"),
              extralines = list("-_Smoke Free air laws" = c("", "$\\checkmark$", "$\\checkmark$", # nolint
                         "", "$\\checkmark$", "$\\checkmark$"),  
                         "-_Additional Controls" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$"), # nolint
                         "-_BRFSS SMART 2004-10" = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", ""), # nolint
                         "-_BRFSS SMART 2014-20" = c("", "", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")), # nolint
              fixef.group=list("Year and MMSA FE "="year|msa_id"), drop.section = c("fixef") , replace = T, # nolint
              style.tex = style.tex("aer"), 
              file = paste(outpath, "01reg_twfe_ver02_exclude1styear.tex", sep ="/"))
       }


}

#####
# ver 02
#####

# This file runs the TWFE estimates

tax <- read.csv(file.path(datapath,  "tax_changes_check.csv"))  %>% 
                separate(col = date, into = c("month", "day", "year"), sep = "/")  %>% 
                mutate(year_change = as.numeric(year) + 2000)  %>% 
                mutate(state = trimws(State))  %>% 
                select(-c(amount, tax_rate, year, State))  

crosswalk  <-  rbind(data.frame(state.abb, state.name), c("DC", "Washington, DC"))

tax  <- tax   %>% 
        merge(crosswalk, by.x = "state", by.y = "state.name", all.x = T)
                


dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))   %>% 
                merge(tax, by.x = c("state_abb", "year_change_per"), 
                by.y = c("state.abb", "year_change"), all.x = T)  %>% 
                mutate(late_year = ifelse(month > 7 & year == year_change_per, 1, 0), 
                late_year = ifelse(is.na(late_year) == T, 0, late_year)
                )  %>% 
                filter(late_year != 1)
                

dat2 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv")) %>% 
                merge(tax, by.x = c("state_abb", "year_change_per"), 
                by.y = c("state.abb", "year_change"), all.x = T)  %>% 
                mutate(late_year = ifelse(month > 7 & year == year_change_per, 1, 0)
                )  %>% 
                filter(late_year != 1)

start_year1 <- min(dat$year)
start_year2 <- min(dat2$year)

# formula for baseline specification
formula_base <- current_smoker ~ tax_change  | 
                 year + msa_id

# formula with SFA
formula_sfa <- current_smoker ~ tax_change  + per_barban05 | 
                 year + msa_id

formula_sfa2 <- current_smoker ~ tax_change  + per_barban10 | 
                 year + msa_id


# formula with controls
formula_control <- current_smoker ~ tax_change  +
                    cbsaunp00:factor(year) + factor98:factor(year) +
                    change_cursmk:factor(year) + log_msapop00:factor(year) + per_barban05 |  #nolint
                    year + msa_id

formula_control2 <- current_smoker ~ tax_change  +
                    cbsaunp10:factor(year) + factor98:factor(year) +
                    change_cursmk:factor(year) + log_msapop10:factor(year) + per_barban10 |  #nolint
                    year + msa_id


for(i in 1:2) {
       if(i == 1){
              dat <- dat
              dat2 <- dat2
       }else{
              dat <- dat %>% filter(year_change_per != start_year1)
              dat2 <- dat2 %>% filter(year_change_per != start_year2)
       }
twfe1 <- feols(formula_base,
            subset(dat), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

twfe2 <- feols(formula_sfa,
            subset(dat), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

twfe3 <- feols(formula_control,
            subset(dat), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j



twfe4 <- feols(formula_base,
            subset(dat2), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

twfe5 <- feols(formula_sfa2,
            subset(dat2), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

twfe6 <- feols(formula_control2,
            subset(dat2), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j

       if(i == 1) {
              setFixest_dict(c(current_smoker = "Panel A. % current smoker"))
              dict = c(tax_change = "Indicator for tax increase")

              etable(twfe1, twfe2, twfe3, twfe4, twfe5, twfe6, dict = dict,
              keep = c("%tax_change"), tex = TRUE, fitstat = c("n", "r2"),
              drop.section = c("fixef") , replace = T, # nolint
              style.tex = style.tex("aer"), 
              file = paste(outpath, "01reg_twfe_exclude1styear_ver02.tex", sep ="/"))
       }else{
              setFixest_dict(c(current_smoker = "Panel B. % current smoker (drop always treated)"))  # nolint
              dict = c(tax_change = "Indicator for tax increase")

              etable(twfe1, twfe2, twfe3, twfe4, twfe5, twfe6, dict = dict,
              keep = c("%tax_change"), tex = TRUE, fitstat = c("n", "r2"),
              extralines = list("-_Smoke Free air laws" = c("", "$\\checkmark$", "$\\checkmark$", # nolint
                         "", "$\\checkmark$", "$\\checkmark$"),  
                         "-_Additional Controls" = c("", "", "$\\checkmark$", "", "", "$\\checkmark$"), # nolint
                         "-_BRFSS SMART 2004-10" = c("$\\checkmark$", "$\\checkmark$", "$\\checkmark$", "", "", ""), # nolint
                         "-_BRFSS SMART 2014-20" = c("", "", "", "$\\checkmark$", "$\\checkmark$", "$\\checkmark$")), # nolint
              fixef.group=list("Year and MMSA FE "="year|msa_id"), drop.section = c("fixef") , replace = T, # nolint
              style.tex = style.tex("aer"), 
              file = paste(outpath, "01reg_twfe_ver02_exclude1styear_ver02.tex", sep ="/"))
       }


}