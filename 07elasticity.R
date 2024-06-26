# This file runs Callaway and Sant Anna Estimator
library(did)
library(patchwork)

############################################
#
# Set path
#
############################################
user = 2
if (user == 1) {
    datapath <- file.path("/home/user1/Dropbox/cigtaxes_new", "data")
    outpath <-  file.path("/home/user1/Dropbox/cigtaxes_new", "output")
}else{
    datapath <- file.path("/Users/vshrestha/Dropbox/cigtaxes_new", "data")
    outpath <-  file.path("/Users/vshrestha/Dropbox/cigtaxes_new", "output")
}

dat0 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))
dat1 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))
south <- c("AL", "AR", "DE", "DC", "FL", "GA", "KY", "LA", "MD", "MS", "NC", "OK", "SC", "TN", "TX", "VA", "WV")

set.seed(1237)

fun_elas <- function(data, firstyear){
        dat <- dat %>% mutate( # nolint
                      year_change_per = ifelse(is.na(year_change_per) == T, 0, year_change_per),
                      south = ifelse(state_abb %in% south, 1, 0),
                      ) %>%  # nolint
                      filter(year_change_per != firstyear) 
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
              data = data,
              panel = TRUE,
              bstrap = TRUE,
              anticipation = 0, 
              control_group = "notyettreated", 
              weightsname = NULL,            
              allow_unbalanced_panel = "TRUE",
              alp = 0.05,
              cband = FALSE,
              biters = 1000,
              clustervars = NULL,
              est_method = "dr",
              base_period = "universal" #varying
              )

group  <- out[[1]]
time  <- out[[2]]
effect  <- out[[3]]

datgrp   <- data.frame(group, time, effect)  %>% 
            mutate(indi = 1, reltime = time - group)  %>% 
            filter(reltime >= 0) 
            

#dat  <- data.frame(dat  %>% 
#   group_by(group)  %>% 
#    mutate(sumindi = sum(indi), weight = indi / sumindi)
#)

datgrp  <- data.frame(datgrp  %>% 
    group_by(group)  %>% 
    mutate(atthat = mean(effect))  %>%  
    filter(reltime == 0)
)

datquantity  <- data.frame(data  %>% 
            mutate(reltime = year - year_change_per)  %>% 
            filter(reltime == -1)  %>%  
            group_by(year_change_per) %>% 
            mutate(mean_current_smoker = mean(current_smoker), 
            mean_tax_change = mean(tax_change_dose), 
            mean_cigtax = mean(cigtax))  %>% 
            select(c(year_change_per, mean_current_smoker, 
            mean_tax_change, mean_cigtax))  %>% 
            mutate(dup = duplicated(year_change_per)) %>% 
            filter(dup == FALSE)  %>% 
            select(-c(dup))
) 

colnames(datquantity) <- c("group", "base_smoker", "mean_tax_change", "base_tax")

datelas   <- merge(datgrp, datquantity, by = "group")

datelas  <- datelas  %>%  
                mutate(top = atthat / base_smoker, 
                bottom = mean_tax_change / base_tax, 
                elasticity = top / bottom)
return(datelas)
}



#aggte(out, type = "group")
form1  <- ~ log_msapop00 + cbsaunp00 + factor98 + change_cursmk
form2  <-  ~ log_msapop10 + cbsaunp10 + factor98 + change_cursmk

datelas0 <- fun_elas(data = dat0, firstyear = 2004)
f0 <- ggplot(datelas0, aes(x = group, y = elasticity)) + 
geom_col(fill = "white", color = "black") + 
theme_bw() + ylab("elasticity (extensive margin)") + ggtitle("A. 2004-2010") + 
xlab("group by the year \n of tax change")


datelas1 <- fun_elas(data = dat1, firstyear = 2015)
f1 <- ggplot(datelas1, aes(x = group, y = elasticity)) + 
geom_col(fill = "white", color = "black") + 
theme_bw() + ylab("elasticity (extensive margin)") + ggtitle("B. 2015-2020") + 
xlab("group by the year \n of tax change")

f <- f0 + f1

ggsave(filename = file.path(outpath, "elasticity.pdf"), height = 5, width = 10)
