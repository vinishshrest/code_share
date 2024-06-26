####################################
####################################
#
#
# Desc: States with tax changes 
#       by year and MMSA count in BRFSS 
#       SMART
####################################
####################################

#########################################
#
# Collecting States that increased tax 
# between 2004 to 2010
#
#########################################
taxes  <-  fread(file = file.path(datapath, "taxes_for_summary_2004to2010.csv"))
dat  <-  fread(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))

start_year <- 2004
end_year   <- 2010

l <- list()
j <- 1
for(i in start_year:end_year) {
    l[[j]] <- table(taxes$abb[taxes$year_change_per == i])
    j <- j + 1
}

b <- length(l)
store <- matrix(0, nrow = length(l), ncol = 5)
y <- 2004
for (j in 1:b) {
    a <- length(names(l[[j]]))
    for (i in 1:a) {
        if (i == 1) {
            nam <- names(l[[j]][i])
        }else {
        nam_temp <- names(l[[j]][i])
        nam <- paste(nam, nam_temp, sep = ", ")
        }
    }
    store[j, 1] <- y
    store[j, 2] <- nam
    store[j, 3] <- length(table(dat$msa_id[dat$year_change_per == y & dat$tax_change == 1]))
    store[j, 4] <- round(mean(dat$tax_change_dose[dat$year_change_per == y], na.rm = T), 2)
    store[j, 5] <- round(sd(dat$tax_change_dose[dat$year_change_per == y], na.rm = T), 2) # nolint
    y <- y + 1
}

colnames(store) <- c("year", "states", "count MMSAs", "tax increase ($)", "s.d.")
filetosav <- xtable(store)
print.xtable(filetosav, file = file.path(outpath, "tax_changes_msacount.tex"), include.rownames = FALSE) # nolint

# for sample 2015-2020
rm(dat)
rm(taxes)

#########################################
#
# Collecting States that increased tax 
# between 2015 to 2020
#
#########################################
dat  <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))
taxes  <- fread(file.path(datapath, "taxes_for_summary_2015to2020.csv"))

start_year <- 2015
end_year   <- 2020

# get the list of states changing taxes in year i
l <- list()
j <- 1
for(i in start_year:end_year) {
    l[[j]] <- table(taxes$abb[taxes$year_change_per == i])
    j <- j + 1
}

b <- length(l)
store <- matrix(0, nrow = length(l), ncol = 5)
y <- start_year
for (j in 1:b) {
    a <- length(names(l[[j]]))
    for (i in 1:a) {
        if (i == 1) {
            nam <- names(l[[j]][i])
        }else {
        nam_temp <- names(l[[j]][i])
        nam <- paste(nam, nam_temp, sep = ", ")
        }
    }
    store[j, 1] <- y
    store[j, 2] <- nam
    store[j, 3] <- length(table(dat$msa_id[dat$year_change_per == y & dat$treat == 1]))
    store[j, 4] <- round(mean(dat$tax_change_dose[dat$year_change_per == y], na.rm = T), 2) # nolint
    store[j, 5] <- round(sd(dat$tax_change_dose[dat$year_change_per == y], na.rm = T), 2) # nolint
    y <- y + 1
}

colnames(store) <- c("year", "states", "count MMSAs", "tax increase ($)", "s.d.")
filetosav <- xtable(store)
print.xtable(filetosav, file = file.path(outpath, "tax_changes_msacount14to19.tex"), include.rownames = FALSE) # nolint

