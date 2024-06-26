#############################
#############################
#
#
# Desc: summary statistics
#
#
#############################
#############################
rm(list = ls())
source(file.path(codepath, "000path.r"))

# read in data
dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))
dat2 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

# declare variables
vars <- c("ever_smoke", "current_smoker", "stop_smoking", "cigtax", "pop1990", "pop2000", "unemp1990", "unemp2000", "cig_sales_pc1990", "cig_sales_pc2000", # nolint
          "cig_sales_pc2010", "factor98", "per_barban10", "tax_change") # nolint

datsum1t <- dat %>% 
           select(all_of(vars)) %>%
           filter(tax_change == 1) %>%
           select(-c(tax_change)) 

datsum1u <- dat %>% 
           select(all_of(vars)) %>%
           filter(tax_change == 0) %>%
           select(-c(tax_change)) 

datsum2t <- dat2 %>% 
           select(all_of(vars)) %>%
           filter(tax_change == 1) %>%
           select(-c(tax_change)) 

datsum2u <- dat2 %>% 
           select(all_of(vars)) %>%
           filter(tax_change == 0) %>%
           select(-c(tax_change)) 

fun <- function(x) {
  m <- mean(x, na.rm = T)
  s <- sd(x, na.rm = T)
  ms <- cbind(m,s)
  return(ms)
}

#for treated and untreated states
sumT <- round(sapply(datsum1t, fun), digits = 2)
sumU <- round(sapply(datsum1u, fun), digits = 2)
sumT2 <- round(sapply(datsum2t, fun), digits = 2)
sumU2 <- round(sapply(datsum2u, fun), digits = 2)

varsnam <- c("% ever smoked", "% current smoker", "% stop smoking", "nominal cig tax", "population 1990", "population 2000", "unemployment 1990", "unemployment 2000",  # nolint
            "cig sales percapita (1990)", "cig sales percapita (2000)", "cig sales percapita (2010)",  # nolint
             "smoking sentiment (1998)", "% under bar ban")
  

store <- matrix(NA, ncol = 5, nrow = ncol(sumT) * 2)
store1 <- store
k <- 1
for(i in seq(1, nrow(store) - 1, 2)) {
j <- i + 1
#########################################
#
# For expansion states
#
#########################################
store[i,2] <- round(sumT[1, k], 2)
store[j,2] <- paste("(", round(sumT[2, k], 2), ")", sep = "")

store[i,3] <- round(sumU[1, k], 2)
store[j,3] <- paste("(", round(sumU[2, k], 2), ")", sep = "")

store[i,4] <- round(sumT2[1, k], 2)
store[j,4] <- paste("(", round(sumT2[2, k], 2), ")", sep = "")

store[i,5] <- round(sumU2[1, k], 2)
store[j,5] <- paste("(", round(sumU2[2, k], 2), ")", sep = "")

store[i, 1] <- varsnam[k]
k <- k + 1
}

colnames(store) <- c("variables", "tax change \n (04-10)", "no change \n (04-10)", # nolint
                    "tax change \n (14-19)", "no change \n (14-19)")

store <- xtable(store,  include.rownames = FALSE) 

#latex table 
print(store, include.rownames = FALSE,  paste(outpath, file = "sum_stat.tex", sep = "/"), type = "latex") # nolint


###########################################
#
# Tax change by group 
# (year of first change)
###########################################
change_grp1 <- as.numeric(names(table(dat$year_change)))[-1]
change_grp2 <- as.numeric(names(table(dat2$year_change)))[-1]
change_grp <- c(change_grp1, change_grp2)

store <- matrix(0, nrow = length(change_grp), ncol = 2)
j <- 1

for(i in change_grp){
  if(i <= 2010){
      store[j, 2] <- mean(dat$tax_change_dose[dat$year_change == i], na.rm = T)
  }else{
      store[j, 2] <- mean(dat$tax_change_dose[dat2$year_change == i], na.rm = T)   # nolint
  }
  store[j, 1] <- i
  j <- j + 1
}

###########################################
#
# Dose response
#
###########################################

reg1 <- lm(cig_sales_pc1990 ~ tax_change_dose, subset(dat, year == 2004))

reg2 <- lm(cig_sales_pc2000 ~ tax_change_dose, subset(dat, year == 2004))

reg3 <- lm(factor98 ~ tax_change_dose, subset(dat, year == 2004))

reg4 <- lm(per_barban10 ~ tax_change_dose, subset(dat, year == 2004))

reg1b <- lm(cig_sales_pc1990 ~ tax_change_dose, subset(dat2, year == 2015))

reg2b <- lm(cig_sales_pc2000 ~ tax_change_dose, subset(dat2, year == 2015))

reg3b <- lm(factor98 ~ tax_change_dose, subset(dat2, year == 2015))

reg4b <- lm(per_barban10 ~ tax_change_dose, subset(dat2, year == 2015))


star.out <- writeLines(capture.output(stargazer(reg1, reg2, reg3, reg4,  reg1b, reg2b, reg3b, reg4b,  # nolint 
            digits=2, dep.var.labels=c("\\shortstack{cig sales \\\\ 1990}", "\\shortstack{cig sales \\\\ 2000}", "\\shortstack{anti \\\\ smoking}", "bar ban", # nolint
            "\\shortstack{cig sales \\\\ 1990}", "\\shortstack{cig sales \\\\ 2000}", "\\shortstack{anti \\\\ smoking}", "bar ban"), # nolint
            keep=c("tax_change_dose"
            ), no.space=TRUE, float=FALSE, font.size = "small", column.labels=c("2004-2010", "2015-2020"),
            column.separate = c(4,4), 
            notes.append = FALSE, notes.align = "l", omit.stat=c("f", "ll", "rsq", "adj.rsq", "ser"), # nolint
            covariate.labels=c("tax change dose"
            ))), 
            paste(outpath, "balance_reg.tex", sep = "/")) 
        

