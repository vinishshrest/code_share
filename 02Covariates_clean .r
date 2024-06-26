# This file collects necessary covariates

# 1. 1990 and 2000 population (state level)

popdat <- read.csv(file.path(datapath, "state_population1990to2000.csv"))

popdat <- popdat %>% select(c(fips, X1990, X2000)) %>%
                     mutate(X1990 = as.numeric(gsub(",", "", X1990)), 
                     X2000 = as.numeric(gsub(",", "", X2000)),
                     fips <- as.numeric(fips)) 

colnames(popdat) <- c("fips",  "pop1990", "pop2000", "del")

# 2. 1990 and 2000 cigarette sales (state level)

cigdat <- read.csv(file.path(datapath, "cig_sales_1990to2010.csv")) %>%
                            select(-c(X, year_change_per1, year_around, tax_change, treat)) %>%  # nolint
                            filter(year == 1990 | year == 2000 | year == 2010)

cigdat1990 <- cigdat %>% filter(year == 1990) %>%
                         select(state_abb, state, cigconsump_pc) %>%
                         rename(cigconsump_pc1990 = cigconsump_pc)

cigdat2000 <- cigdat %>% filter(year == 2000) %>%
                         select(-c(year, log_cigconsump_pc)) %>%
                         rename(cigconsump_pc2000 = cigconsump_pc)

cigdat2010 <- cigdat %>% filter(year == 2010) %>%
                         select(state_abb, state, cigconsump_pc) %>%
                         rename(cigconsump_pc2010 = cigconsump_pc)

cigsales <- merge(cigdat2000, cigdat1990, by = c("state", "state_abb"), all.x = T) %>%  # nolint
                 merge(cigdat2010, by = c("state", "state_abb"), all.x = T) %>%
                 rename(cig_sales_pc2000 = cigconsump_pc2000, cig_sales_pc1990 = cigconsump_pc1990, # nolint
                        cig_sales_pc2010 = cigconsump_pc2010)

# 3. state unemployment 1990 and 2000 (state level)

emp <- read.csv(file.path(datapath, "emp_unemployment_state.csv")) %>% 
                         filter(fips != "0") %>%
                         select(c(fips, statename, X1990, X2000, X2010)) %>%
                         mutate(fips = as.numeric(fips), fips = fips/1000) %>%
                         filter(is.na(fips) == FALSE)

colnames(emp) <- c("fips", "statename", paste("unemp", seq(1990, 2010, 10), sep = "")) # nolint


# 4. MSA population
msa_pop <- read.csv(file.path(datapath, "cbsa_MSA_population.csv")) %>%
           select( c(cbsa_code, mmsa_name, msa_pop10, msa_pop00)) %>%
           mutate(msa_pop00 = as.numeric(gsub(",", "", msa_pop00)), 
                  msa_pop10 = as.numeric(gsub(",", "", msa_pop10)),
                  cbsa_code= as.numeric(cbsa_code))

write.csv(msa_pop, file.path(datapath, "MSA_pop_clean.csv")) 




# 5. smokefree air laws

sfa <- read.csv(file.path(datapath, "smokefree_airlaws.csv"))

fun_sfa <- function(start_year, end_year, dat) {
       sfadat <- dat %>%
                 rename_all(., .funs = tolower) %>%
                 filter(year >= start_year & year <= end_year & quarter == 3) %>%  # nolint
                 select(year, locationabbr, locationdesc, 
                 private_worksites, restaurants, bars) %>%
                 arrange(locationabbr, year)

       return(sfadat)
}

sfadat04to10 <- fun_sfa(2004, 2010, sfa)


covdat <- popdat %>% merge(emp, by = c("fips"), all.x = T) %>%
                     merge(cigsales, by.x = "statename", by.y = "state", all.x = T) %>%  # nolint
                     select(-c(del,  statename)) %>%
                     mutate(log_cigsales90 = log(cig_sales_pc1990), 
                     log_cigsales00 = log(cig_sales_pc2000),
                     log_cigsales10 = log(cig_sales_pc2010), 
                     log_pop90 = log(pop1990), log_pop00 = log(pop2000), 
                     change90to00 =  cig_sales_pc1990 - cig_sales_pc2000,
                     change00to10 =  cig_sales_pc2000 - cig_sales_pc2010,
                     log_change_sales = log(change90to00),  # some changes are negative, perhaps due to smuggling  # nolint
                     log_change_sales00to10 = log(change00to10)
                     )

write.csv(covdat, file.path(datapath, "covariates.csv"))



###############################################
###############################################
#
# Cleaning Smokefree air laws (percent ban)
# Smoke free data: https://no-smoke.org/wp-content/uploads/pdf/EffectivePopulationList.pdf  # nolint
###############################################
###############################################

# read in bar ban data
barban <- read.csv(file.path(datapath, "smokefreeairlawsANSR_clean.csv"))

colnames(barban) <- c("munici", "state", "npw_ban", "rest_ban", "bar_ban", "sf_pop15")  # nolint

year <- c(paste(seq(1990, 2020, by = 1), ":", sep = ""))

barban <- barban %>%
          filter(!(munici %in% year) & is.na(state) == FALSE) %>%
          separate(npw_ban, c("npw_month", "npw_day", "npw_year")) %>%
          separate(rest_ban, c("rest_month", "rest_day", "rest_year")) %>%
          separate(bar_ban, c("bar_month", "bar_day", "bar_year")) 

# read in useful state indicators for merge
statedat <- data.frame(cbind(state.abb, state.name)) 
colnames(statedat) <- c("state", "stname")

# read in city population data
citypop <- read.csv(file.path(datapath, "city_township_pop.csv")) %>%
                     rename_all(., .funs = tolower) %>%
                     select(-c(sumlev, state, county, place, cousub)) %>%
                     rename(munici = name)

citypop <- merge(citypop, statedat, by.x = "stname", by.y = "stname", all.x = T)

citypop_short <- citypop %>% 
                       filter(is.na(state) == FALSE) %>%
                       select(c(state, munici))


# list of states in bar ban data
statenam <- names(table(barban$state))[2:length(table(barban$state))]

# function to do fuzzy matching
funn <- function(dat1, dat2)       {
for(i in 1:dim(dat1)[1]) {
       x <- agrep(dat1$munici[i], c(dat2$munici), ignore.case = TRUE, value = TRUE,  # nolint
       max.distance = 0.1, useBytes = TRUE)
       x <- paste0(x, "")
       dat1$munici.y[i] <- x
}
return(dat1)
}

# write a loop that subsets each state and performs fuzzy matching 

for (j in 1:length(statenam)) {
barban_key <- barban %>% filter(state == statenam[j])
citypop_key <- citypop_short %>% filter(state == statenam[j])
citypop_key <- citypop_key[-1, ]

       if (j == 1) {
              barban_ori <- barban_key
              barban_ori$munici.y <- ""
              barban_ori <- funn(barban_ori, citypop_key)
       }else{
              barban_tmp <- barban_key
              barban_tmp$munici.y <- ""
              barban_tmp <- funn(barban_tmp, citypop_key)
              barban_ori <- rbind(barban_ori, barban_tmp)
       }  
}

barban_ori <- barban_ori %>%
              arrange(munici.y) %>%
              filter(is.na(munici.y) == FALSE) %>%
              select(-c(munici))
              
a <- merge(barban_ori, citypop, by.x = c("munici.y", "state"), 
              by.y = c("munici", "state")) # nolint

a  <- a %>% mutate(dup = duplicated(a)) %>%
               filter(dup == "FALSE") %>%
               filter(is.na(bar_year)==FALSE) %>%
               select(-c(npw_month, npw_day, npw_year, 
               rest_month, rest_day, rest_year, sf_pop15)) %>%
               arrange(state, bar_year, munici.y) %>%
               select(-c(dup))

sf_dat <- data.frame(a %>% 
        group_by(state, stname, bar_year) %>%
        summarize(sf_pop10 = sum(census2010pop, na.rm = T), 
        sf_pop05 = sum(popestimate2005, na.rm = T))    
)

statenam <- names(table(sf_dat$state))

# skeleton
skel <- data.frame(state = rep(statenam, each = 33), 
        bar_year = rep(seq(1990, 2022), length(statenam)) )

# merge with skeleton
sf_dat <- skel %>% merge(sf_dat, by = c("state", "bar_year"), all.x = T) %>%
                   select(- stname) %>%
                   mutate(sf_pop10= ifelse(is.na(sf_pop10) == TRUE, 0, sf_pop10), # nolint
                   sf_pop05 = ifelse(is.na(sf_pop05) == TRUE, 0, sf_pop05))


for(j in 1:length(statenam)){
    sf_dat_key <- sf_dat %>%
                    filter(state == statenam[j]) %>%
                    mutate(cum_sf_pop10 = sf_pop10, cum_sf_pop05 = sf_pop05) 
    if(j == 1) {
        sf_dat_ori <- sf_dat_key
        for(i in 2:dim(sf_dat_ori)[1]){
            sf_dat_ori$cum_sf_pop10[i] <- sf_dat_ori$cum_sf_pop10[i-1] + sf_dat_ori$sf_pop10[i]  # nolint
            sf_dat_ori$cum_sf_pop05[i] <- sf_dat_ori$cum_sf_pop05[i-1] + sf_dat_ori$sf_pop05[i]  # nolint
        }
    }else{
        sf_dat_temp <- sf_dat_key
            if(dim(sf_dat_temp)[1] == 1){
                    sf_dat_temp <- sf_dat_temp
            }else{
                    for(i in 2:dim(sf_dat_temp)[1]){
                    sf_dat_temp$cum_sf_pop10[i] <- sf_dat_temp$cum_sf_pop10[i-1] + sf_dat_temp$sf_pop10[i]  # nolint
                    sf_dat_temp$cum_sf_pop05[i] <- sf_dat_temp$cum_sf_pop05[i-1] + sf_dat_temp$sf_pop05[i]  # nolint
                }
            }
        sf_dat_ori <- rbind(sf_dat_ori, sf_dat_temp)
    }
}

statepop <- data.frame(a %>%
            group_by(state) %>%
            summarize(popestimate2000 = sum(popestimate2000, na.rm = T),
            popestimate2005 = sum(popestimate2005, na.rm = T),
            popestimate2010 = sum(census2010pop, na.rm = T)
            )
)

sf_dat_ori <- sf_dat_ori %>% merge(statepop, by = "state", all.x = T)

sf_dat_ori <- sf_dat_ori %>% 
                mutate(per_barban05 = (cum_sf_pop05/popestimate2005) * 100,
                per_barban10 = (cum_sf_pop10/ popestimate2010) * 100,
                per_barban00 = (cum_sf_pop05/popestimate2000) * 100) %>%
                select(c(state, bar_year, per_barban00, per_barban05, per_barban10)) # nolint

sfplot <- data.frame(sf_dat_ori %>%
            group_by(bar_year)  %>%
            summarize(per_barban05 = mean(per_barban05, na.rm = T),
            per_barban10 = mean(per_barban10, na.rm = T))  %>% 
            mutate(per_barban = ifelse(bar_year < 2010, per_barban05, NA), 
            per_barban = ifelse(bar_year >= 2010, per_barban10, per_barban))
)

ggplot(subset(sfplot, year >1999), aes(bar_year, per_barban05)) + geom_point(size = 2.5) + geom_line() +
        theme_bw() + xlab("year") + ylab("percent under smoking ban in bar") + # nolint
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # nolint
        panel.background = element_blank(),  axis.line = element_line(color = "black"), # nolint
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1),
        text = element_text(size = 17))

ggsave(filename = file.path(outpath, "fig_barbans2000to2020.pdf"))


write.csv(sf_dat_ori, file.path(datapath, "sf_airlaws_bar.csv"))


####################################################
####################################################
#
#  Morg files 2000 and 2010
####################################################
####################################################
library(haven)

# morg2000 (for some reason it say this version is not supported for 2000)
morg00 <- read_dta(file.path(datapath, "morg00.dta"))

morg00 <- data.frame(morg00) %>%
          select(c(stfips, msafips, race, lfsr94)) %>%
          filter(lfsr94 <= 4) %>%  # subsetting to those in labor force
          mutate(unemployed = ifelse(lfsr94 >= 3, 1, 0), 
          msafips = as.numeric(msafips), 
          stfips = as.numeric(stfips)) 

morg00 <- morg00 %>%
          mutate(fakepersonid = 1:nrow(morg00))

# cbsa to fips crosswalk file 
crosswalk <- read.csv(file.path(datapath, "cbsatocountycrosswalk2005.csv")) # 2005 crosswalk # nolint

# crosswalk: MSA mapped to counties; contains duplicates
temp <- crosswalk
temp <- temp %>% arrange(cbsacode) %>%
            select(c(msa, cbsacode, fipsstatecode)) %>%
            distinct(msa, cbsacode, fipsstatecode, .keep_all = TRUE)

morg00 <- morg00 %>% merge(temp, by.x = c("msafips", "stfips"), by.y = c("msa", "fipsstatecode"), all.x = T ) # nolint

# drop duplicates by person id 
morg00 <- morg00 %>% 
            distinct(fakepersonid, .keep_all = TRUE)

morg00 <- data.frame(morg00 %>%
          group_by(cbsacode, stfips) %>%
          summarize(cbsaunp00 = mean(unemployed)) 
    )

fwrite(morg00, file.path(datapath, "morg00.csv"))


# morg10 (already has the cbsa code so no need to merge with the cross-walk)

morg10 <- read_dta(file.path(datapath, "morg10.dta"))

morg10 <- morg10 %>%
          select(c(stfips, cbsafips, race, lfsr94)) %>%
          filter(lfsr94 <= 4) %>%
          mutate(unemployed = ifelse(lfsr94 >= 3, 1, 0), 
          cbsafips = as.numeric(cbsafips), 
          stfips = as.numeric(stfips))

morg10 <- data.frame(morg10 %>%
          group_by(cbsafips, stfips) %>%
          summarize(cbsaunp10 = mean(unemployed)) 
    )

fwrite(morg10, file.path(datapath, "morg10.csv"))


##################################################
#
# CBSA Statistical Area Wage index
#
##################################################

datwage <- fread(file.path(datapath, "wageindexcbsa2005.csv"))

sapply(datwage, class)

datwage <- datwage %>% 
           filter(datestr == 20041001) %>%
           select(cbsa, wi) %>%
           mutate(cbsa = as.numeric(cbsa)) %>%
           filter(is.na(cbsa) == FALSE)

table(duplicated(datwage$cbsa))

fwrite(datwage, file.path(datapath, "wage_index05.csv"))


###################################################
#
#  cpsjan00 (smoking sentiments data)
#
###################################################
library(readr)

senti00 <- data.frame(read_fwf(file.path(datapath, "cpssep98.dat"), 
          fwf_positions(c(93, 95, 97, 101, 861, 969, 977, 979, 981, 983, 985, 987, 989, 993, 995, 857), # nolint start position 
          c(94, 96, 100, 103, 862, 970, 978, 980, 982, 984, 986, 988, 990, 994, 996, 858), # nolint end position
          c("fips", "cmsafips", "msa", "fipscounty",
          "current_smoker", "senti_public", "senti_rest", 
          "senti_hosp", "senti_indoor", "senti_bars", 
          "senti_sport", "senti_malls", "smoke_home", "free_samp", "advertise", "ever_smoke"))) #nolint
)
senti00$year <- 1998

senti01 <- data.frame(read_fwf(file.path(datapath, "CPSFeb02.dat"), 
          fwf_positions(c(93, 95, 97, 101, 863, 971, 979, 981, 983, 985, 987, 989, 991, 993, 995, 859), # nolint start position 
          c(94, 96, 100, 103, 864, 972, 980, 982, 984, 986, 988, 990, 992, 994, 996, 860), # nolint end position
          c("fips", "cmsafips", "msa", "fipscounty",
          "current_smoker", "senti_public", "senti_rest", 
          "senti_hosp", "senti_indoor", "senti_bars", 
          "senti_sport", "senti_malls", "smoke_home", "free_samp", "advertise", "ever_smoke"))) #nolint
)
senti01$year <- 2001

senti <- rbind(senti00, senti01)

crosswalk <- read.csv(file.path(datapath, "cbsatocountycrosswalk2005.csv"))
crosswalk <- crosswalk %>% 
             filter(msa != 1) %>%
             distinct(msa, .keep_all = TRUE)

sapply(senti, class)

senti <- senti %>% merge(crosswalk, by.x = "msa", by.y = "msa", all.x = T)

smoke <- senti %>%
             select(msa, ever_smoke, current_smoker, cbsacode, year) %>%
             filter(ever_smoke > 0) %>%
             mutate(ever_smoke = ifelse(ever_smoke == 2, 0, 1), 
             current_smoker = ifelse(ever_smoke == 0, 0, current_smoker),
             current_smoker = ifelse(current_smoker == 2, 1, current_smoker),
             current_smoker = ifelse(current_smoker == 3, 0, current_smoker)) %>%  #nolint 
             filter(msa != 0 & current_smoker >= 0)

smoke <- data.frame(smoke %>%
            group_by(cbsacode, year) %>%
            summarize(current_smoker = mean(current_smoker), 
            ever_smoke = mean(ever_smoke))
)


senti <- senti %>%
         filter(msa != 0) %>%
         filter(senti_hosp > 0 & senti_rest > 0 & senti_bars > 0 
         & senti_malls > 0 & senti_sport > 0 & senti_indoor > 0 
         & smoke_home > 0 & free_samp > 0 & advertise > 0)

# 1 refers to not allowed at all 
senti$senti_hosp <- ifelse(senti$senti_hosp == 3, 1, 0) 
senti$senti_rest <- ifelse(senti$senti_rest == 3, 1, 0)
senti$senti_indoor <- ifelse(senti$senti_indoor == 3, 1, 0)
senti$senti_bars <- ifelse(senti$senti_bars == 3, 1, 0)
senti$senti_sport <- ifelse(senti$senti_sport == 3, 1, 0)
senti$senti_malls <- ifelse(senti$senti_malls == 3, 1, 0)
senti$smoke_home <- ifelse(senti$smoke_home == 1, 1, 0) # nolint noone allowed to smoke anywhere 1
senti$free_samp <- ifelse(senti$free_samp == 3, 1, 0)
senti$advertise <- ifelse(senti$advertise == 3, 1, 0)

library(psych)

root <- principal(senti[, c(7, 8, 9:15)], nfactors = 1, rotate = "varimax")

senti <- cbind(senti, root$scores)

senti <- data.frame(senti %>% 
         group_by(cbsacode, year) %>%
         summarize(factor1 = mean(PC1))
)


senti <- senti %>% merge(smoke, by = c("cbsacode", "year"), all.x = TRUE)

senti00 <- senti %>% filter(year == 1998) %>%
                    rename(current_smoker98 = current_smoker,
                    ever_smoke98 = ever_smoke, 
                    factor98 = factor1)
senti01 <- senti %>% filter(year == 2001) %>%
                    rename(current_smoker01 = current_smoker,
                    ever_smoke01 = ever_smoke, 
                    factor01 = factor1)
senti <- senti00 %>% merge(senti01, by = "cbsacode", all.x = T) %>%
                     mutate(change_cursmk = current_smoker98 - current_smoker01,
                     change_evrsmk = ever_smoke98 - ever_smoke01)

f0 <- ggplot(senti, aes(factor98, ever_smoke98)) + geom_point() +
geom_smooth(method = "lm", se = F) + theme_bw() + xlab("anti-smoking sentiment 1998") +  #nolint
ylab("proportion ever smoked 1998 \n (at least 100 cigarettes)") + ggtitle("Panel A")

f1 <- ggplot(senti, aes(factor98, current_smoker98)) + geom_point() +
geom_smooth(method = "lm", se = F) + theme_bw() + xlab("anti-smoking sentiment 1998") +  #nolint
ylab("proportion current smoker 1998") + ggtitle("Panel B")

f2 <- ggplot(senti, aes(factor98, change_cursmk)) + geom_point() +
geom_smooth(method = "lm", se = F) + theme_bw() + xlab("anti-smoking sentiment 1998") +  #nolint
ylab("proportion change 1998-2001 \n (current smoker)") + ggtitle("Panel C")

ggsave(ggarrange(f0, f1, f2, ncol = 1, nrow = 3),
file =  file.path(outpath, "anti_smoking&smoking.pdf"), height = 10, width = 6)


write.csv(senti, file.path(datapath, "smoking_sentiments.csv"))


















