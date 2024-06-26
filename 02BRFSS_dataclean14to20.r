rm(list = ls())

library(haven)
library(dplyr)
library(tidyr)
library(tidyselect)
library(plm)
library(ggplot2)
library(maps)
library(ggpubr)
library(gridExtra)
#install.packages("bacondecomp")
library(data.table)
library(janitor)
library(fixest)
library(bacondecomp)
library(ggfixest)
#library(fuzzyjoin)
library(xtable)
library(did)


# declare paths
source(file.path(codepath, "000path.r"))

#############################################################
#############################################################
#
# BFRSS SMART Data Clean: 2014--2020
#
#
#############################################################
#############################################################
l <- list()
i <- 1
j <- 2014
datalist <- c(paste0("MMSA", seq(2014, 2020, 1), ".xpt", sep = ""))

for (filename in datalist) {

    dat <- data.frame(read_xpt(file.path(datapath, "BRFSS_MSA", filename))) # nolint
    dat <- dat %>%
             rename_all(., .funs = tolower)

    dat <- dat %>% select(ends_with("mmsa"), "x_mmsawt", # nolint 
                     contains("rfsmok"), contains("smoke100"), contains("smoker"), contains("stopsmk"),  # nolint
                     contains("drnkany"),   # nolint
                     contains("rfbinge"), contains("alcday"), starts_with("sex"), contains("age_g"), # nolint
                     "x_race", contains("income"), contains("employ"), "marital", "educa") %>% # nolint
                     mutate(year = j)

    a <- ncol(dat)
    print(paste0(filename, "--", a, "cols", sep = ""))
    l[[i]] <- c(colnames(dat))
    i <- i + 1


    colnames(dat) <- c("msa_id", "msawt", "rfsmok", "ever_smoke", "smoker", "stopsmok", # nolint
                    "drnkany", "alcday", "sex", "age", "race2", "income2", "employ", "marital", "educa", "year") # nolint

    write.csv(dat, paste0(datapath, "/BRFSS_MSA/brfss", j, ".csv", sep = ""))
    j <- j + 1
}


##################################################
#
# Append files 2014-2020 brfss
#
##################################################

# 2014-2020
fun_dataconst <- function(start, end) {
    for (i in start:end){
        if (i == start)   {
            dat <- read.csv(paste0(datapath, "/BRFSS_MSA/brfss", i, ".csv", sep = "")) # nolint
        }else {
            dat_new <- read.csv(paste0(datapath, "/BRFSS_MSA/brfss", i, ".csv", sep = "")) # nolint
            dat <- rbind(dat, dat_new)
        }
    }
    return(dat)
}

dat <- fun_dataconst(2014, 2020)
#################################################
#
#  Functions
#
#################################################

fun_cigtaxtracker <- function(dat, loyear, hiyear) {
        cig_data_state <- dat %>% 
                    filter(year >= loyear & year <= hiyear) %>%
                    group_by(abb) %>%
                    summarize(min_cig_tax = min(cigtax), 
                    max_cig_tax = max(cigtax), 
                    numtax_changes = sum(tax_change, na.rm = T)) %>% 
                    mutate(tax_changeamount = max_cig_tax - min_cig_tax)  %>% # nolint
                    select(c(abb, numtax_changes, tax_changeamount))
        cig_data_state <- data.frame(cig_data_state)
        return(cig_data_state)
} 

###################################
#
# Function to construct cigarette 
# tax data
###################################

fun_taxwindow <- function(dat, year0, year1)   {
    # filter data by year of interest and tax change == 1
    cigtax_bef <- dat %>% filter(year <= year1 & year >=
                                    year0 & tax_change == 1) %>%
                          mutate(numtaxinc = 1)

    # pick the year or first year of tax change (if multiple changes) 
    index_bef <- cigtax_bef %>% 
                group_by(fips, abb) %>% # nolint
                summarize(year_change_per =  min(year), 
                tax_change_dose = mean(tax_change_dose, na.rm = T), # nolint
                numtaxinc = sum(numtaxinc))                  # nolint

    return(index_bef)
}


balanced <-function(data, ID, TIME, VARS, required=c("all","shared")) {
    if(is.character(ID)) {
        ID <- match(ID, names(data))
    }
    if(is.character(TIME)) {
        TIME <- match(TIME, names(data))
    }
    if(missing(VARS)) { 
        VARS <- setdiff(1:ncol(data), c(ID,TIME))
    } else if (is.character(VARS)) {
        VARS <- match(VARS, names(data))
    }
    required <- match.arg(required)
    idf <- do.call(interaction, c(data[, ID, drop=FALSE], drop=TRUE))
    timef <- do.call(interaction, c(data[, TIME, drop=FALSE], drop=TRUE))
    complete <- complete.cases(data[, VARS])
    tbl <- table(idf[complete], timef[complete])
    if (required=="all") {
        keep <- which(rowSums(tbl==1)==ncol(tbl))
        idx <- as.numeric(idf) %in% keep
    } else if (required=="shared") {
        keep <- which(colSums(tbl==1)==nrow(tbl))
        idx <- as.numeric(timef) %in% keep
    }
    data[idx, ]
}

#################################################
#################################################
#
# Sample collapsed by MSA-level
#
#################################################
#################################################

start_year <- 2015
end_year   <- 2020
cutoff <- 0.385

# constructing data
dat <- fun_dataconst(start_year, end_year) %>% 
                           filter(smoker <= 4 | ever_smoke <= 2) %>%
                           mutate(current_smoker = ifelse(smoker <= 2, 1, 0),
                                  stop_smoking = ifelse(stopsmok == 2, 0, stopsmok), # nolint
                                  ever_smoke = ifelse(ever_smoke == 2, 0, ever_smoke))  # nolint


# collapse data into MSA using msawt
dat <- data.frame(dat %>% mutate(ind = 1) %>%
             group_by(year, msa_id) %>%
             summarize(ever_smoke = weighted.mean(ever_smoke, na.rm = T, weight = msawt), # nolint
                       current_smoker = weighted.mean(current_smoker,  na.rm = T, weight = msawt), # nolint
                       stop_smoking = weighted.mean(stop_smoking, na.rm = T, weight = msawt), N = sum(ind)) # nolint
                       ) 

# use only the balanced panel of MSAs
# dat <- make.pbalanced(dat) 

dat <- balanced(dat, "msa_id", "year")

dat <- dat %>% arrange(msa_id, year)

#############################################################
#
# Merge with Census Core-Based Statistical Area (CBSA) 
# crosswalk.
# link: https://www.nber.org/research/data/census-core-based-statistical-area-cbsa-federal-information-processing-series-fips-county-crosswalk # nolint
#############################################################

# cbsa to fips crosswalk file 
crosswalk <- read.csv(file.path(datapath, "cbsa_fips_crosswalk.csv"))   # 2016 crosswalk # nolint
crosswalk05 <- read.csv(file.path(datapath, "cbsatocountycrosswalk2005.csv")) # 2005 crosswalk # nolint

# crosswalk: MSA mapped to counties; contains duplicates
temp <- crosswalk
temp$dupcodes <- duplicated(crosswalk$cbsacode)  
temp <- subset(temp, dupcodes == TRUE)
temp <- temp %>% arrange(cbsacode)

#########################################
#
# Yearly change in cigarette taxes file
#
#########################################

cig_data <- read.csv(file.path(datapath, "The_Tax_Burden_on_Tobacco__1970-2019.csv")) %>% # nolint
                    filter(SubMeasureDesc == "State Tax per pack") %>%
                    select("LocationAbbr", "LocationDesc", "Year", "Data_Value") %>%  # nolint
                    rename_all(., .funs = tolower) %>%
                    filter(year >= 2010 & year <= 2020) 

colnames(cig_data) <- c("abb", "statename", "year", "cigtax")
cig_data$cigtax <- as.numeric(cig_data$cigtax) 

state_dat <- state.fips %>%
             select(abb, fips) %>%
             filter(!duplicated(abb))
state_dat <- rbind(state_dat, c("HW", 15), c("AK", 2)) # adding Hawai and Alaska

cig_data <- cig_data %>% merge(state_dat, by = "abb", all.x = T) %>%
                arrange(fips, year) %>%
                mutate(year = year - 1)

# Read cigarette tax data as panel
cig_data <- pdata.frame(cig_data, index = c("fips", "year"))
cig_data$cigtax <- round(cig_data$cigtax, 3)

# Create lag of cigarette tax 
cig_data <- cig_data %>% group_by(fips) %>%
                mutate(lagcigtax = dplyr::lag(cigtax))

# check to see if there was tax change in year t
cig_data <- cig_data %>% mutate(tax_change_dose = cigtax - lagcigtax,
                                tax_change = ifelse(tax_change_dose != 0, 1, 0))

# calculate the cumulative number of tax changes
for (i in 1:10) {
    if (i == 1) {
        cig_data <- cig_data %>%
                group_by(fips) %>%
                mutate(taxchangelag = dplyr::lag(tax_change),
                taxchangelag = ifelse(is.na(taxchangelag) == TRUE, 0, taxchangelag), # nolint
                numer_change = tax_change + taxchangelag)
    }
    else{
        cig_data <- cig_data %>% 
                group_by(fips) %>% 
                mutate(numchangelag = dplyr::lag(numer_change))

        cig_data <- cig_data %>%
                mutate(numchangelag = ifelse(is.na(numchangelag) == TRUE, 0, numchangelag)) # nolint

        cig_data <- cig_data %>% 
                group_by(fips) %>% 
                mutate(numer_change = tax_change + numchangelag)
    }
}

cig_data <- cig_data %>% select(-c(taxchangelag, numchangelag))
i <- c(which(colnames(cig_data) == "year"), which(colnames(cig_data) == "fips"))
cig_data[, i] <- apply(cig_data[, i], 2, 
                        function(x) as.numeric(as.character(x)))

# save cleaned version of cig_data
fwrite(cig_data, file.path(datapath, "cigtax2010to2019_clean.csv"))

# read in cigarette data
taxes <- read.csv(file.path(datapath, "cigtax2010to2019_clean.csv"))

# check to see whether states passed taxes in 2002 or 2003 (over $0.5) 
# Note that 2002-2003 are pre-period for the start of the panel
taxes1113 <- data.frame(fun_taxwindow(taxes, 2011, 2013)) %>% 
                        arrange(year_change_per, tax_change_dose) %>%
                        filter(tax_change_dose >= cutoff) %>%
                        group_by(fips, abb) %>%
                        filter(n() == 1) %>%
                        select(fips) %>%
                        mutate(tax_change1113 = 1)

# taxes for the desired panel
taxes <- data.frame(fun_taxwindow(taxes, start_year, end_year)) 

# merge desired panel with tax change 11-13
taxes <- taxes %>% merge(taxes1113, by = c("fips", "abb"), all.x = T) %>%
                        mutate(tax_change1113 = ifelse(is.na(tax_change1113) == T, 0, tax_change1113))  # nolint

taxes <- taxes %>% arrange(year_change_per, abb)

taxes$year_change_per[taxes$fips == 1] <- 2016
taxes$year_change_per[taxes$fips == 9] <- 2016
#taxes$year_change_per[taxes$fips == 27] <- 2015
taxes$year_change_per[taxes$fips == 41] <- 2014
taxes$year_change_per[taxes$fips == 6] <- 2017
taxes$year_change_per[taxes$fips == 10] <- 2018

tax_add <- data.frame(fips = c(17, 35, 51), abb = c("IL", "NM", "VA"), year_change_per = c(2019, 2019, 2020), tax_change_dose = c(1, 0.34, 0.3),  # nolint
                     numtaxinc = c(1, 1, 1) , tax_change1113 = c(0, 0, 0))

taxes <- rbind(taxes, tax_add) %>%
              filter(tax_change_dose >= 0.1)

fwrite(taxes, file.path(datapath, "taxes_for_summary_2015to2020.csv"))

# collect treated states by year 
l <- list()
j <- 1
for(i in start_year:end_year) {
    l[[j]] <- table(taxes$abb[taxes$year_change_per == i])
    j <- j + 1
}


# merge dat with msa_id to fips code using the crosswalk
# make sure to exclude duplicates in crosswalk
# while merging
dat <- dat %>% merge(crosswalk[!duplicated(crosswalk$cbsacode),], 
                by.x = "msa_id", by.y = "cbsacode", all.x = T) 

table(dat$msa_id[is.na(dat$fipsstatecode) == TRUE])

##########################################################
#
#  Fixing MSA codes that were not mapped to FIPS state
#  i) use crosswalk2005; ii) manual fix 
##########################################################
miss_msa <- data.frame(msa_id = as.numeric(names(table(dat$msa_id[is.na(dat$fipsstatecode) == TRUE]))))  # nolint
miss_msa <- miss_msa %>% merge(crosswalk05[!duplicated(crosswalk05$cbsacode),], by.x = "msa_id",  # nolint
                                by.y = "cbsacode", all.x = TRUE) %>%
                                select(c("msa_id", fipsstatecode)) %>%
                                rename(fipsstatecode2 = fipsstatecode) 

# merge msas that are unmapped with fips using miss_msa file 

dat <- dat %>%
           merge(miss_msa, by = "msa_id", all.x = TRUE) %>%
           mutate(fipsstatecode = ifelse(is.na(fipsstatecode) == T, fipsstatecode2, fipsstatecode)) %>% # nolint
           select(-c("fipsstatecode2"))   # nolint

miss <- names(table(dat$msa_id[is.na(dat$fipsstatecode) == TRUE]))

# manually fix missing msa_id to state mapping
dat$statename[dat$msa_id == 14454] <- "Massachussets" 
dat$fipsstatecode[dat$msa_id == 14454] <- 25 
dat$statename[dat$msa_id == 33874] <- "Pennsylvania" 
dat$fipsstatecode[dat$msa_id == 33874] <- 42 
dat$statename[dat$msa_id == 35614] <- "New York" 
dat$fipsstatecode[dat$msa_id == 35614 ] <- 36 
dat$statename[dat$msa_id == 43524 ] <- "Maryland" 
dat$fipsstatecode[dat$msa_id == 43524 ] <- 24  
dat$statename[dat$msa_id == 47664 ] <- "Michigan" 
dat$fipsstatecode[dat$msa_id == 47664 ] <- 26  

# merging with cigarette tax
dat <- dat %>% merge(taxes, by.x = c("fipsstatecode"), by.y = c("fips"), all.x = T) %>% # nolint
                           mutate(tax_change1113 = ifelse(is.na(tax_change1113) == T, 0, tax_change1113)) # nolint


#########################################################
#########################################################
#
#  Setting variables for estimation
#
#
#########################################################
#########################################################

# DEFINITIONS
# year_change_per: year of the first tax change in the panel 
        # note that if year_change_per is NA, then the state is never_treated
        # in this case give it a value 0.

# year_around: This is relative time in Sun and Abraham 
        # for never_treated, year_around = 0

# treat: tracks whether the unit is treated within the desired panel

# tax_change: whether unit i is treated at time t

dat <- dat %>% 
            mutate(year_change_per = ifelse(is.na(year_change_per) == TRUE, 0, year_change_per),  # nolint
            year_around = ifelse(year_change_per != 0, year - year_change_per, 0), # nolint
            treat = ifelse(year_change_per != 0, 1, 0),  # nolint
            tax_change = ifelse(year_around >= 0 & year_change_per != 0, 1, 0)) %>% # nolint
            arrange(msa_id, year)


################################################################
################################################################
#
# Merging with covariates
#
################################################################
################################################################

covdat <- read.csv(file.path(datapath, "covariates.csv"))
barban <- read.csv(file.path(datapath, "sf_airlaws_bar.csv"))
msa_pop <- read.csv(file.path(datapath, "MSA_pop_clean.csv")) 

dat <- dat %>% merge(covdat, by.x = c("fipsstatecode"), 
            by.y = c("fips"), all.x = T) %>% 
            mutate(current_smoker = current_smoker * 100)

# merging with bar bans and msa population
dat <- dat %>% merge(barban, by.x = c("state_abb", "year"), by.y = c("state", "bar_year"), all.x = T) %>% # nolint
               merge(msa_pop, by.x = "msa_id", by.y = "cbsa_code", all.x = T) %>% # nolint
               select(-c(X, X.y, mmsa_name)) 
               
################################################
#
# Fixing the missing msa population manually
# use: https://s4.ad.brown.edu/Projects/Diversity/segregation2010/msa.aspx?metroid=14484 metroid = msa_id # nolint
################################################
miss <- names(table(dat$msa_id[is.na(dat$msa_pop00) == T]))

dat$msa_pop00[dat$msa_id == miss[1]] <- 4391344
dat$msa_pop10[dat$msa_id == miss[1]] <- 4552402
dat$msa_pop00[dat$msa_id == miss[2]] <- 1465396
dat$msa_pop10[dat$msa_id == miss[2]] <- 1503085
dat$msa_pop00[dat$msa_id == miss[3]] <- 1186999
dat$msa_pop10[dat$msa_id == miss[3]] <- 1250679
dat$msa_pop00[dat$msa_id == miss[4]] <- 3451226
dat$msa_pop10[dat$msa_id == miss[4]] <- 4235751
dat$msa_pop00[dat$msa_id == miss[5]] <- 1710318
dat$msa_pop10[dat$msa_id == miss[5]] <- 2136022
dat$msa_pop00[dat$msa_id == miss[6]] <- 12365627
dat$msa_pop10[dat$msa_id == miss[6]] <- 12828837

# hand-edit more missing msa values 
idnam <- c(35004, 35614, 37964, 40484, 42644, 47664, 47894, 48864)
handpop <- c(2832882, 11576251, 2084985, 418366, 2644584, 2475666, 4444378, 705670) #nolint

for(i in 1:length(idnam)) {
    dat$msa_pop10[dat$msa_id == idnam[i] ] <- handpop[i]
}

# states without free-standing bars are given 0.
# effects observed by st. fixed effects
dat <- dat %>% 
            mutate(per_barban00 = ifelse(is.na(per_barban00) == TRUE, 0, per_barban00), # nolint
            per_barban10 = ifelse(is.na(per_barban10) == TRUE, 0, per_barban10),
            per_barban05 = ifelse(is.na(per_barban05) == TRUE, 0, per_barban05),
            log_msapop00 = log(msa_pop00), 
            log_msapop10 = log(msa_pop10)
             ) 

cig_data <- read.csv(file.path(datapath, "The_Tax_Burden_on_Tobacco__1970-2019.csv")) %>% # nolint
                    filter(SubMeasureDesc == "State Tax per pack") %>%
                    select("LocationAbbr", "Year", "Data_Value") %>%  # nolint
                    rename_all(., .funs = tolower) %>%
                    filter(year >= 2010 & year <= 2020) 

colnames(cig_data) <- c("abb", "year", "cigtax")
cig_data$cigtax <- as.numeric(cig_data$cigtax) 

dat <- dat %>% merge(cig_data, by.x = c("state_abb", "year"), by.y = c("abb", "year"), all.x = T) # nolint

############ merge with morg2010 file ##################
morg10 <- fread(file.path(datapath, "morg10.csv"))
dat <- dat %>% merge(morg10, by.x = c("msa_id", "fipsstatecode"), by.y = c("cbsafips", "stfips") , all.x = TRUE) # nolint
table(is.na(dat$cbsaunp10))

dat <- dat %>% 
       mutate(cbsaunp10 = cbsaunp10 * 100, 
       cbsaunp10 = ifelse(is.na(cbsaunp10 == TRUE), unemp2010, cbsaunp10),    # nolint
       cbsaunp10 = ifelse(is.na(cbsaunp10 == TRUE), mean(unemp2010, na.rm = T) , cbsaunp10)) # nolint

############ merge with wage index 2005 ################
wageind <- fread(file.path(datapath, "wage_index05.csv"))

dat <- dat %>% merge(wageind, by.x = "msa_id", by.y = "cbsa", all.x = TRUE)

dat <- dat %>% 
        rename(wage_index = wi) %>%
        mutate(wage_index = ifelse(is.na(wage_index == T), mean(wage_index, na.rm = T), wage_index)) # nolint

################# smoking sentiments ######################
senti <- read.csv(file.path(datapath,  "smoking_sentiments.csv")) %>%
                 select(-c(year.x, year.y))

dat <- dat %>% merge(senti, by.x = "msa_id", by.y = "cbsacode", all.x = TRUE) %>% # nolint
               mutate(factor98 = ifelse(is.na(factor98 == T), mean(factor98, na.rm = T), factor98),  # nolint
               factor01 = ifelse(is.na(factor01 == T), mean(factor01, na.rm = T), factor01), #nolint
               current_smoker98 = ifelse(is.na(current_smoker98 == T), mean(current_smoker98, na.rm = T), current_smoker98), #nolint
               current_smoker01 = ifelse(is.na(current_smoker01 == T), mean(current_smoker01, na.rm = T), current_smoker01),  #nolint
               change_cursmk = ifelse(is.na(change_cursmk == T), mean(change_cursmk, na.rm = T), change_cursmk)  #nolint
               )

write.csv(dat, file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))


#########################################
#
# Collecting States that increased tax 
# between 2015 to 2020
#########################################
dat  <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

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

