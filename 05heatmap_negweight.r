#######################################
#
# Desc: make heat map for weights
#       Figure 2 
#
#######################################

library('dplyr')
library(ggplot2)

source(file.path(codepath, "000path.r"))

###############################
#
# read in data
#
###############################

# early sample
dat1 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))
# late sample
dat2 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

fun_heat  <- function(dat, sample){

    if(sample == "early"){
        plot_title  <- "A. Weights in TWFE, 2004-2010"
        cols  <- c("grey24", "grey50", "white")
    }else{
        plot_title  <- "B. Weights in TWFE, 2015-2020"
        cols  <- c("grey50", "white", "white") 
    }

# regress treatment dummy on fixed effects
reg  <- lm(tax_change ~ factor(state_abb) + factor(year), data = dat)
summary(reg)

# get the residuals
dat  <- dat  %>% 
        mutate( resid01 = tax_change - predict(reg) )


count  <-  1 # count to keep tract of num of small data
datalist  <- list() # a list to store data

for (j in 2005:2010){ #vary treatment year
    store  <- data.frame( year = seq(from = 2004, to = 2010, by = 1),
                    treatyear = rep(j, 7), 
                    negweight = rep(0, 7) )
    k  <- j - 2003
    for (i in j:2010) { #vary calendar year
        neg  <- min(dat$resid01[dat$year == i & dat$year_change_per == j], na.rm = T) < 0
        store$negweight[k]  <- ifelse(neg == TRUE, TRUE, FALSE)
        k  <- k + 1
        print(paste0("year", i))
        print(neg)
    } 

datalist[[count]]  <- store
count  <- count + 1
print(store)
} 

bigstore  <-  do.call(rbind, datalist)

bigstore  <- bigstore  %>% 
                mutate(type = NA,
                type = ifelse(year < treatyear, 'untreated', type),
                type = ifelse(year >= treatyear & negweight == 0, 'positive weight', type),
                type = ifelse(year >= treatyear & negweight ==1, 'negative weight', type))


f  <- ggplot(bigstore, aes(y = factor(year), x = factor(treatyear))) + 
geom_tile(mapping = aes(fill = type, width = 0.7, height = 0.7), color = "black", lwd = 1) + theme_classic() + 
scale_fill_manual(values=cols) + xlab("treatment year") + ylab("calendar year") + 
theme(text = element_text(size = 17)) + ggtitle(plot_title)

return(f)
}

f0  <- fun_heat(dat = dat1, sample = "early")
# ggsave(filename = file.path(outpath, "heatmap_negweights.pdf"))
f1  <- fun_heat(dat = dat2, sample = "late")

ggsave(ggarrange(f0, f1, ncol = 1, nrow = 2), 
       filename = file.path(outpath, "heatmap_negweights_combine.pdf"), height = 10, width = 8)


