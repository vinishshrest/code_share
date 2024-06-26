rm(list = ls())
#install.packages("bacondecomp")
library(data.table)
library(dplyr)
library(janitor)
library(fixest)
library(bacondecomp)
library(ggplot2)
library(ggiplot)
library(gridExtra)
library(did)

#lintr::use_lintr(type = "tidyverse")
#usethis::use_github_action("lint-project")
#lintr::lint_dir("/home/user1/Dropbox/cigtaxes_new")

datapath <- file.path("/home/vinish/Dropbox/cigtaxes_new", "data")

################################################
#
# 1. Cigarette Consumption file
#
################################################
data <- fread(file.path(datapath, "The_Tax_Burden_on_Tobacco__1970-2019.csv"))

head(data)
data %>% tabyl(SubMeasureDesc)
# keep cigarette sales per pack
data <- data %>% filter(SubMeasureDesc == 
                "Cigarette Consumption (Pack Sales Per Capita)") %>% 
                select(c(LocationAbbr, LocationDesc, Year, Data_Value)) %>%
                arrange(LocationAbbr, Year) 

colnames(data) <- c("state_abb", "state", "year", "cigconsump_pc") 

data <- data %>% mutate(cigconsump_pc = as.numeric(cigconsump_pc)) %>%
                filter(year >= 1986 & year <= 2014)
head(data)
data <- data.frame(data)


###############################################
#
# 2. Cigarette Tax Files
#
###############################################

cigtax <- fread(file.path(datapath, "cigtax85to2014_clean.csv"))

# year when states passed the max cig tax increase between 1990 and 1997

fun_window <- function(dat, year0, year1)   {
    cigtax_bef <- cigtax %>% filter(year <= year1 & year >=
                                    year0 & tax_change == 1)

    index_bef <- cigtax_bef %>% 
                group_by(abb) %>%
                summarize(year_change_per = year[which(tax_change_dose 
                                                == max(tax_change_dose))]) 
    index_bef <- index_bef %>% group_by(abb) %>%
                            filter(n() == 1)
    return(index_bef)
}


fun_datconstruct <- function(begin, end, dat) {
index0 <- data.frame(fun_window(cigtax, begin, end))
colnames(index0) <- c("abb", "year_change_per1")

tempdat <- merge(dat, index0, by.x = c("state_abb"), by.y = c("abb"), all.x = T, all.y = T) %>%
        mutate(log_cigconsump_pc = log(cigconsump_pc))

tempdat <- tempdat %>% filter(year >= begin & year <= end) %>% 
                   mutate(year_change_per1 = ifelse(is.na(year_change_per1) == T, 99999, year_change_per1), 
                   year_around = ifelse(year_change_per1 != 99999, year - year_change_per1, 0) ,
                   tax_change = ifelse(year >= year_change_per1, 1, 0)) %>%
                   arrange(state_abb, year)

return(tempdat)
}


data94t04 <- fun_datconstruct(1994, 2004, data)
data96t06 <- fun_datconstruct(1996, 2006, data)
data98t08 <- fun_datconstruct(1998, 2008, data)
data00t10 <- fun_datconstruct(2000, 2010, data)
data02t12 <- fun_datconstruct(2002, 2012, data)
data04t14 <- fun_datconstruct(2004, 2014, data)



datsum <- data96t06 %>%
         mutate(group = year_change_per1) %>%
         filter(year_change_per1 != 99999)

datasum <- data.frame(datsum %>%
         group_by(year_around, group) %>%
         summarize(cig_consump = log(mean(cigconsump_pc)))
)

ggplot(datasum, aes(year_around,  cig_consump, 
group = factor(group), color = factor(group), shape = factor(group))) + 
geom_point(size = 4) + ylab("log of cigarette sales \n (per capita)") + 
theme_bw() + xlab("year around the policy year") + 
theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5)) +
           geom_vline(xintercept = 0, linetype = "dashed")



###################################################################
###################################################################
###################################################################
#
#  Bacon Decomposition for various panel years
#
#
###################################################################
###################################################################
###################################################################
# 1.

datalist <- c("data94t04", "data96t06", "data98t08", "data00t10", "data02t12", "data04t14")
titles <- c("a. 1994 to 2004", "b. 1996 to 2006", "c. 1998 to 2008", "d. 2000 to 2010", "e. 2002 to 2012", "f. 2004 to 2014")

p <- list()
b <- list()
i <- 1
j <- 1994
for(filename in datalist)  {
dat <- get(filename)
es0 <- feols(log_cigconsump_pc ~ i(year_around, -1, bin = "bin::1") | 
                 year + state,
               subset(dat, year_change_per1 != j), cluster = c("state")) # nolint

f0 <- ggiplot(es0, ci_level = 0.95, geom = "errorbar", pt.join = TRUE, size = 0.5, shape = 2)  +  geom_errorbar( width = 0.2)  + # nolint
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), # nolint
          text = element_text(size = 15),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') +  geom_line(linetype = "dashed") + xlab("year") +  # nolint
          ylim(c(-0.6,0.7)) + ggtitle(titles[i]) + geom_line(linetype = "dashed")  # nolint


# Bacon decomposition (3 groups: 1. early, 2. later, 3. untreated)
df_bacon0 <- bacon(log_cigconsump_pc ~ tax_change,
                  data = subset(dat, year_change_per1 != j),
                  id_var = "state_abb",
                  time_var = "year")


b0 <- ggplot(df_bacon0) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point(size = 3) + theme_bw() + ggtitle(titles[i])

p[[i]] <- f0
print(p[[i]])
b[[i]] <- b0
i <- i + 1
j <- j + 2
}


grid.arrange(b[[1]], b[[2]], b[[3]], b[[4]], b[[5]], b[[6]],  ncol = 3, nrow = 2, respect = TRUE) # nolint


plot(1:10, 1:10)



#2. 
es0 <- feols(log_cigconsump_pc ~ i(year_around, -1, bin = "bin::1") | 
                 year + state,
               subset(data96t06, year_change_per1 != 1994), cluster = c("state")) # nolint

f0 <- ggiplot(es0, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  + # nolint
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), # nolint
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") +  # nolint
          ylim(c(-0.65,0.35)) + ggtitle("A. All States") + geom_line(linetype = "dashed") + geom_point(size = 4) # nolint


# Bacon decomposition (3 groups: 1. early, 2. later, 3. untreated)
df_bacon0 <- bacon(log_cigconsump_pc ~ tax_change,
                  data = subset(data96t06, year_change_per1 != 1996),
                  id_var = "state_abb",
                  time_var = "year")


ggplot(df_bacon0) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point(size = 4) + theme_bw()












































# All states
es0a <- feols(log_cigconsump_pc ~ i(year_around, -1, bin = "bin::1") | 
                 year + state,
               subset(data0), cluster = c("state"))

f0a <- ggiplot(es0a, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.4,0.2)) + ggtitle("A. All States") + geom_line(linetype = "dashed") + geom_point(size = 4)











# Only early and untreated states
es1a <- feols(log_cigconsump_pc ~ i(year_around, -1, bin = "bin::1") | 
                 year + state,
               subset(data0, year_change_per1 == 99999 | year_change_per1 <=1994), cluster = c("state"))


f1a <- ggiplot(es1a, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.25,0.25)) + ggtitle("B. Early Treated (bef. 2001) \n vs. Untreated") + geom_line(linetype = "dashed") + geom_point(size = 4)


es1b <- feols(log_cigconsump_pc ~ i(year_around, -1, bin = "bin::1") | 
                 year + state,
               subset(dataA, year_change_per1 == 99999 | year_change_per1 <=2005), cluster = c("state"))


f1b <- ggiplot(es1b, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.65,0.3)) + ggtitle("B. Early Treated (bef. 2001) \n vs. Untreated") + geom_line(linetype = "dashed") + geom_point(size = 4)


# Only late and untreated states
es2a <- feols(log_cigconsump_pc ~ i(year_around, -1, bin = "bin::1") | 
                 year + state,
               subset(data0, year_change_per1 == 99999 | 
               (year_change_per1 >= 1995)), cluster = c("state"))


f2a <- ggiplot(es2a, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.65,0.3)) + ggtitle("C. Late Treated (after 2000) \n vs. Untreated") + 
          geom_line(linetype = "dashed") + geom_point(size = 4)



es2b <- feols(log_cigconsump_pc ~ i(year_around, -1, bin = "bin::1") | 
                 year + state,
               subset(dataA, year_change_per1 == 99999 | 
               (year_change_per1 > 2004)), cluster = c("state"))


f2b <- ggiplot(es2b, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.65,0.4)) + ggtitle("C. Late Treated (after 2000) \n vs. Untreated") + 
          geom_line(linetype = "dashed") + geom_point(size = 4)



# Only late and early states (as the comparison) [BAD COMPARISON]
data_test <- data0 %>% mutate(year_around = ifelse(year_change_per1 <= 1993, 0, year_around))

es3a <- feols(log_cigconsump_pc ~   i(year_around, c(-1), bin = "bin::1") | 
                 year + state,
               subset(data_test, year_change_per1 != 99999 & year > 1994), cluster = c("state"))


f3a <- ggiplot(es3a, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.35,0.2)) + ggtitle("D. Late vs. Early \n (as comparison)") + geom_line(linetype = "dashed") + geom_point(size = 4)






data_test <- dataA %>% mutate(year_around = ifelse(year_change_per1 <= 2005, 0, year_around))

es3b <- feols(log_cigconsump_pc ~   i(year_around, c(-1) , bin = "bin::1") | 
                 year + state,
               subset(data_test, year_change_per1 != 99999 & year > 2005), cluster = c("state"))


f3b <- ggiplot(es3b, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.35,0.2)) + ggtitle("D. Late vs. Early \n (as comparison)") + geom_line(linetype = "dashed") + geom_point(size = 4)


# early states acting treated and late states not yet treated 
data_test <- dataA %>% mutate(year_around = ifelse(year_change_per1 > 2005, 0, year_around))

es3 <- feols(log_cigconsump_pc ~ i(year_around, c(-1), bin = "bin::1") | 
                 year + state,
               subset(data_test, year_change_per1 != 99999 & year <= 2005), cluster = c("state"))


ggiplot(es3, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.35,0.2)) + ggtitle("E. Early (treated) vs. Late (as comparison)") + geom_line(linetype = "dashed") + geom_point(size = 4)





# Bacon decomposition (3 groups: 1. early, 2. later, 3. untreated)
df_baconA <- bacon(log_cigconsump_pc ~ tax_change,
                  data = subset(dataA),
                  id_var = "state_abb",
                  time_var = "year")


ggplot(df_baconA) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point(size = 4) + theme_bw()


# Bacon decompostition (limiting to just time variation)
df_baconB <- bacon(log_cigconsump_pc ~ tax_change,
                  data = subset(dataA, year_change_per1 != 99999),
                  id_var = "state_abb",
                  time_var = "year")


ggplot(df_baconB) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point(size = 4) + theme_bw()









es <- feols(log_cigconsump_pc ~   i(year_around, -1, bin = "bin::1") | 
                 year + state,
               subset(dataA, year_change_per1 <= 2002 | year_change_per1 == 99999), cluster = c("state"))


   ggiplot(es, ci_level = 0.95, geom = "errorbar", pt.join = TRUE)  +  geom_errorbar( width = 0.2)  +
          theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
          text = element_text( size = 24),
          plot.title = element_text(hjust = 0.5),
          legend.position = 'none') + ggtitle("") +  geom_line(linetype = "dashed") + xlab("year") + 
          ylim(c(-0.65,0.2)) + ggtitle("") + geom_line(linetype = "dashed") + geom_point(size = 4)






data08 <- data %>% filter(year >= 1998 & year <= 2008) %>% 
                   mutate(year_change_per2 = ifelse(is.na(year_change_per2) == T, 99999, year_change_per2),
                   tax_change = ifelse(year >= year_change_per2, 1, 0)) %>%
                   arrange(state_abb, year)


data14 <- data %>% filter(year >= 2009 & year <= 2014) %>% 
                   mutate(year_change_per3 = ifelse(is.na(year_change_per3) == T, 99999, year_change_per3),
                   tax_change = ifelse(year >= year_change_per3, 1, 0)) %>%
                   arrange(state_abb, year)


data <- merge(data, cigtax, by.x = c("state_abb", "year"), by.y = c("abb", "year"), all.x = T)




reg <- feols(cigconsump_pc ~  cigtax | 
                      year + fips,
                    subset(data, year > 1997), cluster = c("fips"))



reg98 <- feols(cigconsump_pc ~  tax_change | 
                      year + state_abb,
                    subset(data98), cluster = c("state_abb"))


reg08 <- feols(cigconsump_pc ~  tax_change | 
                      year + state_abb,
                    subset(data08), cluster = c("state_abb"))


reg14 <- feols(cigconsump_pc ~  tax_change | 
                      year + state_abb,
                    subset(data14), cluster = c("state_abb"))



df_bacon98 <- bacon(log_cigconsump_pc ~ tax_change,
                  data = data98,
                  id_var = "state_abb",
                  time_var = "year")


ggplot(df_bacon98) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point(size = 3)


df_bacon08 <- bacon(log_cigconsump_pc ~ tax_change,
                  data = data08,
                  id_var = "state_abb",
                  time_var = "year")


ggplot(df_bacon08) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point(size = 3)


df_bacon14 <- bacon(log_cigconsump_pc ~ tax_change,
                  data = data08,
                  id_var = "state_abb",
                  time_var = "year")


ggplot(df_bacon14) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point(size = 3)

  data96t06


# install.packages("devtools")
# devtools::install_github("bcallaway11/did")
library(did)
data00t10$stateid <- rep(1:51, each = 11)

out <- att_gt(yname = "log_cigconsump_pc",
              gname = "year_change_per1",
              idname = "stateid",
              tname = "year",
              xformla = ~1,
              data = data00t10,
              est_method = "reg", control_group="notyettreated"   #"nevertreated"
              )

              ggdid(out, ylim = c(-.25,.3))

es <- aggte(out, type = "dynamic")

ggdid(es)

group_effects <- aggte(out, type = "group")
summary(group_effects)