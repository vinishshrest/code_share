############################
############################
# Description: This file runs the TWFE, canonical event study, and Sun & Abraham event study.
# It produces tables and event study results (figures). It loops over the early (2004-2010) and 
# later (2015-2020) samples from BRFSS SMART. 
############################
############################

# clear objects 
rm(list = ls())
source("/Users/vshrestha/Dropbox/cigtaxes_new/code/000path.r")

# read in data
dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))
dat2 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

# loop over the early and later samples
for(i in 1:2) {

        # declare the baseline models 
        formula_base_tw <- current_smoker ~ tax_change | 
                 year + msa_id

        formula_base <- current_smoker ~ i(year_around, treat, bin = "bin::1", 
                                c(-1, omitfar)) |
                                year + msa_id

        formula_sun <- current_smoker ~ sunab(year_change_per2, year, 
                ref.p = c(-1, omitfar) ) |
                year + msa_id

        if(i == 1){ # 2004-2010 data  
                start_year <- min(dat$year)
                end_year <- max(dat$year)
                omitfar <- min(dat$year_around)

                # models with controls
                formula_control <- current_smoker ~ cbsaunp00:factor(year) + factor98:factor(year) + log_msapop00:factor(year) +  # nolint
                        change_cursmk:factor(year) +
                        i(year_around, treat, bin = "bin::1", # nolint
                        c(-1, omitfar)) |
                        year + msa_id

                formula_control_tw <- current_smoker ~ tax_change +
                    cbsaunp00:factor(year) + 
                    change_cursmk:factor(year) + per_barban05 + factor98:factor(year) |  # nolint
                    year + msa_id

                formula_suncontrol <- current_smoker ~ cbsaunp00:factor(year) + factor98:factor(year) + log_msapop00:factor(year) +   # nolint
                        change_cursmk:factor(year) +
                        sunab(year_change_per2, year, ref.p = c(-1, omitfar)) | # nolint
                        year + msa_id

                dat <- dat
        }else{ # 2015-2020 data  
                start_year <- min(dat2$year)
                end_year <- max(dat2$year)
                omitfar <- -5

                formula_control <- current_smoker ~ cbsaunp10:factor(year) + factor98:factor(year) + log_msapop10:factor(year) +  # nolint
                        change_cursmk:factor(year) +
                        i(year_around, treat, bin = "bin::1", # nolint
                        c(-1, omitfar)) |
                        year + msa_id

                formula_control_tw <- current_smoker ~ tax_change +
                    cbsaunp10:factor(year) + 
                    change_cursmk:factor(year) + per_barban10 + factor98:factor(year) | # nolint
                    year + msa_id

                formula_suncontrol <- current_smoker ~ cbsaunp10:factor(year) + factor98:factor(year) + log_msapop10:factor(year) +   # nolint
                        change_cursmk:factor(year) +
                        sunab(year_change_per2, year, ref.p = c(-1, omitfar)) | # nolint
                        year + msa_id

                dat <- dat2
        }

        # a. twfe ES baseline
        est_tw0 <- feols(formula_base_tw,
                        subset(dat, year_change_per != start_year), cluster = c("fipsstatecode")) # nolint 

        est_a0 <- feols(formula_base,
                        subset(dat, year_change_per != start_year), cluster = c("fipsstatecode")) # nolint 
        ggiplot(est_a0)

        # b. twfe ES controls
        est_tw1 <- feols(formula_control_tw,
                        subset(dat, year_change_per != start_year), cluster = c("fipsstatecode")) # nolint 

        est_a1 <- feols(formula_control,
                        subset(dat, year_change_per != start_year), cluster = c("fipsstatecode")) # nolint 
        ggiplot(est_a1)

        # for Sun and Abraham, if never_treated give value of 10000
        dat <- dat %>% mutate(year_change_per2 = ifelse(treat == 0, 10000, year_change_per))  # nolint

                # c. Sun & Abraham baseline
        est_a2 <- feols(formula_sun, # nolint
                        subset(dat), cluster = c("fipsstatecode")) 

        ggiplot(est_a2)

        # d. Sun & Abraham with controls
        est_a3 <- feols(formula_suncontrol,
                        subset(dat), cluster = c("fipsstatecode")) 

        ggiplot(est_a3)

        # TWFE and SA Event Study Estimates (means)
        if(i == 1){
                hline <- mean(coefficients(est_a0)[5:10]) # without controls (2004-2010 data)
                hline_control <- mean(coefficients(est_a1)[5:10]) # with controls (2004-2010 data)
        }else{
                hline <- mean(coefficients(est_a0)[4:8]) # without controls (2015-2020 data)
                hline_control <- mean(coefficients(est_a0)[4:8]) # with controls (2015-2010 data)
        }

        # plot without controls
        hline_tw <- coefficients(est_tw0)[[1]]
        f0 <- ggiplot(list('canonical event study' = est_a0, 'Sun & Abraham (2020)' = est_a2),  # nolint
                        main = 'A. Without Controls', ref.line = -1, pt.join = TRUE) + # nolint
                        geom_hline(yintercept = c(hline, hline_tw), lwd = c(1, 0.75) , lty = c("dashed", "dotted"), color=c("black", "red")) + # nolint
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # nolint
                        panel.background = element_blank(),  axis.line = element_line(color = "black"), # nolint
                        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1), 
                        legend.position = c(0.25, 0.1), text = element_text(size = 17)) +
                        ylab("estimates & \n confidence intervals") + xlab("relative year")

        # plot with controls
        hline_tw <- coefficients(est_tw1)[[1]]
        f1 <- ggiplot(list('canonical event study' = est_a1, 'Sun & Abraham (2020)' = est_a3), # nolint
                        main = 'B. With Controls', ref.line = -1, pt.join = TRUE) + 
                        geom_hline(yintercept = c(hline_control, hline_tw), lwd = c(1, 0.75) , lty = c("dashed", "dotted"), color=c("black", "red")) + # nolint
                        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # nolint
                        panel.background = element_blank(),  axis.line = element_line(color = "black"), # nolint
                        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1), 
                        legend.position = c(0.25, 0.1), text = element_text(size = 17)) +
                        ylab("estimates & \n confidence intervals") + xlab("relative year")

        if(i == 1){
                ggsave(ggarrange(f0, f1, ncol = 1, nrow = 2),
                        file =  file.path(outpath, "TWFE_SA_eventstudy.pdf"), height = 10, width = 6) # nolint

        ###############
        # ES Table
        ###############
                setFixest_dict(c(current_smoker = "% current smoker"))  # nolint
                dict = c(tax_change = "Indicator for tax increase")

                etable(est_a0, est_a1,  dict = dict,
                keep = c("year_around"), tex = TRUE, fitstat = c("n", "r2"),
                extralines = list("-_Smoke Free air laws" = c("", "$\\checkmark$"),  
                         "-_Additional Controls" = c("", "$\\checkmark$"), # nolint
                         "-_BRFSS SMART 2004-10" = c("", "$\\checkmark$"), # nolint
                         "-_BRFSS SMART 2014-20" = c("", "$\\checkmark$")), # nolint
                fixef.group=list("Year and MMSA FE "="year|msa_id"), drop.section = c("fixef") , replace = T, # nolint
                style.tex = style.tex("aer"), 
                file = paste(outpath, "01reg_ES_2004to2010.tex", sep ="/"))
        }else{
                ggsave(ggarrange(f0, f1, ncol = 1, nrow = 2),
                        file =  file.path(outpath, "TWFE_SA_eventstudy_ver02.pdf"), height = 10, width = 6)# nolint

                setFixest_dict(c(current_smoker = "% current smoker"))  # nolint
                        dict = c(tax_change = "Indicator for tax increase")

        ###############
        # ES Table
        ###############
                etable(est_a0, est_a1,  dict = dict,
                keep = c("year_around"), tex = TRUE, fitstat = c("n", "r2"),
                extralines = list("-_Smoke Free air laws" = c("", "$\\checkmark$"),  
                         "-_Additional Controls" = c("", "$\\checkmark$"), # nolint
                         "-_BRFSS SMART 2004-10" = c("", "$\\checkmark$"), # nolint
                         "-_BRFSS SMART 2014-20" = c("", "$\\checkmark$")), # nolint
              fixef.group=list("Year and MMSA FE "="year|msa_id"), drop.section = c("fixef") , replace = T, # nolint
              style.tex = style.tex("aer"), 
              file = paste(outpath, "01reg_ES_2015to2020.tex", sep ="/"))
        }
}

