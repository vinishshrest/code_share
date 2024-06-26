# This file runs Callaway and Sant Anna Estimator
library(did)
dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

dat <- dat %>% mutate( # nolint
                      year_change_per = ifelse(is.na(year_change_per) == T, 0, year_change_per)) %>%   # nolint
               filter(year_change_per != 2015)

############################################
############################################
#
# Necessary functions 
#
############################################
############################################
fun_dat <- function(obj){
        agg.es <- aggte(obj, type = "dynamic", na.rm = TRUE, balance_e = 2)
        egt <- c(agg.es$egt) # length of exposure in the case of dynamic
        att.gt <- c(agg.es$att.egt)  # group time average treatment effect 
        se.egt <- c(agg.es$se.egt)
        crit.val.egt <- agg.es$crit.val.egt   # nolint this the critical value used to calculate the uniform confidence band for 
# dynamic effects
        overall.att <- agg.es$overall.att
        overall.se <- agg.es$overall.se
        store <- data.frame(egt, att.gt, se.egt)
        store <- store %>%
            mutate(upper = att.gt - se.egt * crit.val.egt, 
            lower = att.gt + se.egt * crit.val.egt)
    return(list(store, overall.att, overall.se))
}

fun_plot <- function(data, overall.att, figlab) {
    f <- ggplot(data, aes(x = egt, y = att.gt)) + geom_point(pch = 1, size = 3) +  # nolint
        geom_errorbar(aes(ymin = lower, ymax = upper), width=.2) + 
        ylab("Effect by the length of exposure") + xlab("relative year") +
        ggtitle(figlab) +
        geom_vline(xintercept = -1, linetype = "dotted") + 
        geom_hline(yintercept = c(0, overall.att) , linetype = c("solid", "solid"), lwd = c(0.15, 0.5), color = c("black", "red")) +
        theme_bw() + 
        theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())   # nolint
    return(f)
}


#############################################
#############################################
#
# Part A. Never Treated as comparison
#
#############################################
#############################################

# 1. Without covariates

out <- att_gt(yname = "current_smoker",
              gname = "year_change_per",
              idname = "msa_id",
              tname = "year",
              xformla = ~ 1,
              data = subset(dat),
              panel = TRUE,
              bstrap = TRUE,
              anticipation = 0, 
              control_group = "nevertreated", 
              weightsname = NULL,            
              allow_unbalanced_panel = "FALSE",
              alp = 0.05,
              cband = TRUE,
              biters = 1000,
              clustervars = NULL,
              est_method = "dr",
              base_period = "universal" #varying
              )

              ggdid(out)


agg.es <- aggte(out, type = "dynamic", na.rm = TRUE, balance_e = 2)

store <- fun_dat(out)
att_nt1 <- store[[2]]
se_nt1 <- store[[3]]
f0 <- fun_plot(store[[1]], store[[2]], figlab = "A. Baseline specification (nt)") # nolint 



#f0 <- ggdid(agg.es, ylab = "effect", xlab = "relative year") + geom_point(size = 4, pch = 1) + 
#geom_vline(xintercept = -1, linetype = "dotted") + ggtitle("A. Baseline specification (never treated as comparison)")



# 2. With covariates

out2 <- att_gt(yname = "current_smoker",
              gname = "year_change_per",
              idname = "msa_id",
              tname = "year",
              xformla = ~ log_msapop10 + cbsaunp10 + factor98 + change_cursmk, # nolint
              data = subset(dat),
              panel = TRUE,
              bstrap = TRUE,
              anticipation = 0, 
              control_group = "nevertreated", 
              weightsname = NULL,            
              allow_unbalanced_panel = "TRUE",
              alp = 0.05,
              cband = TRUE,
              biters = 1000,
              clustervars = NULL,
              est_method = "reg",
              base_period = "universal" #varying
              )

ggdid(out2)


agg.es2 <- aggte(out2, type = "dynamic", na.rm = TRUE, balance_e = 2)

store <- fun_dat(out2)
att_nt2 <- store[[2]]
se_nt2 <- store[[3]]
f1 <- fun_plot(store[[1]], store[[2]], figlab = "B. With covariates (nt)")

#f1 <- ggdid(agg.es2, ylab = "effect", xlab = "relative year") + geom_point(size = 4, pch = 1) + 
#geom_vline(xintercept = -1, linetype = "dotted") + ggtitle("B. With covariates (never treated as comparison)")


ggsave(ggarrange(f0, f1, ncol = 1, nrow = 2),
file =  file.path(outpath, "CS_eventstudy_ver02_bal.pdf"), height = 10, width = 6)



###############################################
#
# Part B. Not yet treated as comparison
#
###############################################

out <- att_gt(yname = "current_smoker",
              gname = "year_change_per",
              idname = "msa_id",
              tname = "year",
              xformla = ~ 1,
              data = subset(dat),
              panel = TRUE,
              bstrap = TRUE,
              anticipation = 0, 
              control_group = "notyettreated", 
              weightsname = NULL,            
              allow_unbalanced_panel = "TRUE",
              alp = 0.05,
              cband = TRUE,
              biters = 1000,
              clustervars = NULL,
              est_method = "dr",
              base_period = "universal" #varying
              )

              ggdid(out)


              ggdid(out)


agg.es <- aggte(out, type = "dynamic", na.rm = TRUE, balance_e = 2)

store <- fun_dat(out)
att_nyt1 <- store[[2]]
se_nyt1 <- store[[3]]
f0ntt <- fun_plot(store[[1]], store[[2]], figlab = "C. Baseline specification (nyt)")

#f0 <- ggdid(agg.es, ylab = "effect", xlab = "relative year") + geom_point(size = 4, pch = 1) + 
#geom_vline(xintercept = -1, linetype = "dotted") + ggtitle("A. Baseline specification (not yet treated as comparison)")



# 2. With covariates

out2 <- att_gt(yname = "current_smoker",
              gname = "year_change_per",
              idname = "msa_id",
              tname = "year",
              xformla = ~ log_msapop10 + cbsaunp10 + factor98 + change_cursmk, # nolint
              data = subset(dat),
              panel = TRUE,
              bstrap = TRUE,
              anticipation = 0, 
              control_group = "notyettreated", 
              weightsname = NULL,            
              allow_unbalanced_panel = "TRUE",
              alp = 0.05,
              cband = TRUE,
              biters = 1000,
              clustervars = NULL,
              est_method = "reg",
              base_period = "universal" #varying
              )

ggdid(out2)


agg.es2 <- aggte(out2, type = "dynamic", na.rm = TRUE, balance_e = 2)

store <- fun_dat(out2)
att_nyt2 <- store[[2]]
se_nyt2 <- store[[3]]
f1ntt <- fun_plot(store[[1]], store[[2]], figlab = "D. With covariates (nyt)") # nolint

#f1 <- ggdid(agg.es2, ylab = "effect", xlab = "relative year") + geom_point(size = 4, pch = 1) +   # nolint
#geom_vline(xintercept = -1, linetype = "dotted") + ggtitle("B. With covariates (not yet treated as comparison)")  # nolint


ggsave(ggarrange(f0, f1, ncol = 1, nrow = 2),
file =  file.path(outpath, "CS02_eventstudy_ver02_balance.pdf"), height = 10, width = 6) # nolint


ggsave(ggarrange(f0, f1, f0ntt, f1ntt, ncol = 2, nrow = 2),
file =  file.path(outpath, "CS02_eventstudy_ver02_combined_balance.pdf"), height = 6, width = 6) #nolint 

