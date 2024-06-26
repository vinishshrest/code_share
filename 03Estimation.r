# This file runs the TWFE estimates

dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))

dat <- dat %>% mutate(current_smoker = current_smoker * 100)

# formula for baseline specification
formula_base <- current_smoker ~ tax_change | 
                 year + msa_id

# formula with controls
formula_control <- current_smoker ~ tax_change +
                    unemp2000:factor(year) + 
                    anti1998:factor(year) + 
                    log_pop00:factor(year) +
                    change90to00:factor(year) | 
                    year + msa_id


twfe1 <- feols(formula_base,
            subset(dat), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j
summary(twfe1)

twfe2 <- feols(formula_control,
            subset(dat), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j
summary(twfe2)

twfe3 <- feols(formula_base,
            subset(dat, tax_change0203 == 0), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j
summary(twfe3)

twfe4 <- feols(formula_control,
            subset(dat, tax_change0203 == 0), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j
summary(twfe4)

setFixest_dict(c(current_smoker = "percent current smoker"))

dict = c(tax_change = "Indicator for tax increase")

etable(twfe1, twfe2, twfe3, twfe4 , dict = dict,
       keep = c("%tax_change"), tex = TRUE, fitstat = c("n", "r2"),
       extralines = list("-_Smoke Free air laws" = c("", "$\\checkmark$", "", "$\\checkmark$"),  # nolint
                         "-_Additional Controls" = c("", "$\\checkmark$", "", "$\\checkmark$"), # nolint
                         "-_Drop treated in 2002-03" = c("", "", "$\\checkmark$", "$\\checkmark$")), # nolint
       fixef.group=list("Year and MMSA FE "="year|msa_id"), drop.section = c("fixef") , replace = T, # nolint
       style.tex = style.tex("aer"), 
       file = paste(outpath, "01reg_twfe.tex", sep ="/"))



        twfe2 <- feols(current_smoker ~ tax_change | 
                 year + msa_id,
               subset(dat, tax_change0203 == 0), cluster = c("fipsstatecode")) # nolint # , year_change_per1 != j
        summary(twfe2)



library(did)
dat <- dat %>% mutate(year_change_per = ifelse(tax_change0203 == 1, start_year, year_change_per), # nolint
                      year_change_per = ifelse(is.na(year_change_per) == T, 0, year_change_per))   # nolint

out <- att_gt(yname = "current_smoker",
              gname = "year_change_per",
              idname = "msa_id",
              tname = "year",
              xformla = ~ log_cigsales00 + change90to00 + anti1998 + log_pop00,
              data = subset(dat),
              est_method = "reg", panel = FALSE, control_group = "nevertreated"
              )

              ggdid(out)


agg.es <- aggte(out, type = "dynamic", na.rm = TRUE)
ggdid(agg.es)


out <- att_gt(yname = "current_smoker",
              gname = "year_change_per",
              idname = "fipsstatecode",
              tname = "year",
              xformla = ~ log_cigsales00 + change90to00 + anti1998 + log_pop00,
              data = subset(dat),
              est_method = "reg", panel = FALSE, control_group = "notyettreated"
              )

              ggdid(out)


agg.es <- aggte(out, type = "dynamic", na.rm = TRUE)
ggdid(agg.es)









dat2 <- data.frame(dat %>%
        group_by(year, treat) %>%
        summarize(Mcurrent_smoker = mean(current_smoker, na.rm = T))
)


ggplot(dat2, aes(year, Mcurrent_smoker, group = treat, color = factor(treat))) + geom_point()

table(dat$statename[dat$treat == 0])

table(dat$fipsstatecode[dat$treat == 0])