##################
# last modified: April 18, 23

rm(list = ls())
# load libraries 
library("pacman")
pacman::p_load(dplyr, ggplot2, parallel, patchwork, did, stringr)

RNGkind("L'Ecuyer-CMRG")

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
########################
#
# Set seed/  
# cores for parallel prog
#
########################
iseed = 125789
set.seed(iseed)
numcores = detectCores()

###############################
#
# 2004 to 2010 Data
#
###############################

# read in data and keep selected variables 
dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))
datsim  <- dat  %>% 
            select(msa_id, fipsstatecode, year, state_abb, 
            tax_change, year_around, year_change_per)  %>% 
            filter(year_change_per != 2004)

# Declare the unit and year ids
genFE <- function(data) {
    # 1. unit ids
    nunits  <- length(table(datsim$msa_id))
    namunit  <- as.numeric(names(table(datsim$msa_id)))
    # 1. generate unit fixed effect based on nunits
    unitfe  <- rnorm(nunits, mean = 0, s= 20)
    unitfe  <- data.frame(msa_id = namunit, unitfe = unitfe)

    # 2. year ids 
    nyears  <- length(table(datsim$year))
    namyear  <- as.numeric(names(table(datsim$year)))
    # 2. generate year fixed effect based on year ids
    yearfe  <- rnorm(nyears, 0, 1)
    yearfe  <- data.frame(year = namyear, yearfe = yearfe)  %>% 
                    mutate(yearfe = yearfe - (year - min(year) + 1))

    datsim  <- datsim  %>% 
        left_join(unitfe, by = 'msa_id')  %>% 
        left_join(yearfe, by = 'year')      
    
    return(datsim)        
}
att  <- -1.2 # homogeneous ATT

# CS estimator
GTatt  <- function(data) { 

    datsim  <- genFE(dat = datsim)
    datsim  <- datsim  %>% 
        mutate(err = rnorm(nrow(datsim), 0, 1), 
        Y = tax_change * att + yearfe + unitfe + err)   

        out <- att_gt(yname = "Y",
              gname = "year_change_per",
              idname = "msa_id",
              tname = "year",
              xformla = ~ 1,
              data = datsim,
              panel = TRUE,
              bstrap = TRUE,
              anticipation = 0, 
              control_group = "nevertreated", 
              weightsname = NULL,            
              allow_unbalanced_panel = "TRUE",
              alp = 0.05,
              cband = FALSE,
              biters = 1000,
              clustervars = NULL,
              est_method = "dr",
              base_period = "universal"
              )

    aggall  <- aggte(out, type = "dynamic", na.rm = TRUE)
    overallatt  <- aggall$overall.att # gets the overall att
    esatt  <- aggall$att.egt # gets the event study type estimates
    grpatt  <- aggte(out, type = "group", na.rm = TRUE)[[5]] # gets the group specific att
    return(list(overallatt, esatt, grpatt))
}

#########################
# CASE 1 = Homogeneous 
#########################
reps  <- seq(1, 5000)
storelist  <- mclapply(reps, GTatt, mc.cores = numcores, mc.set.seed=TRUE)
store0  <- rep(0, length(reps))

for (i in 1:length(reps)) {
    store0[i]  <- storelist[[i]][[1]]
}

store0  <- data.frame(X1 = store0)
lab  <-  paste0("true effect:", att,   "\n rep mean:", round(mean(store0[,1]), 3) , sep = "")
p0 <- ggplot(data = store0, aes(X1)) + geom_histogram(fill = 'white', color = 'grey') +
                        geom_vline(xintercept = c(mean(store0[,1]), att), color = c('red', 'blue'), lty = c('dashed', 'dotted'), size = 1) +
                        theme(text = element_text(size = 17)) +
                        theme_classic() + annotate(geom = 'text', x = quantile(store0$X1, 0.007), y = 500, label = lab, size = 3.75) + ylim(c(0, 600)) + 
                        xlab("att estimate") + ylab("frequency") + ggtitle("Performance of CS Estimator in homogeneous ATT")

ggsave(filename = file.path(outpath, '02simulation_resultA.pdf'))

####################################################
# CASE 2 = heterogeneous treatment effect by time 
####################################################
yeararound  <- sort(unique(datsim$year_around))
attvals  <- round(c(runif(5, -0.5, 0.5), 0, -0.3077, -1.2863, -0.8405, -1.3728, -1.5916, -1.8456), 3)
attdf  <- data.frame(year_around = yeararound, att = attvals)
datsim  <- datsim %>% 
            left_join(attdf, by = "year_around")

reps  <- seq(1, 5000)
meanatt2  <- mean(attvals[7:13])
storelist2  <- mclapply(reps, GTatt, mc.cores = numcores, mc.set.seed=TRUE)
store  <- matrix(0, nrow = length(reps), ncol = length(attvals))
for (i in 1:length(reps)){
    store[i, ]  <- storelist2[[i]][[2]]
} 

storedat  <- data.frame(store[, 7:12]) 
repmeans  <- sapply(storedat, mean)
trueatt  <- attvals[7:12]
period  <- paste(seq(0, 5, 1), "yr", sep = " ")

plist  <- list()

for (i in 1:ncol(storedat)) {
    storedat2  <- data.frame(X1 = storedat[,i])
    lab  <-  paste0("true effect:", trueatt[i],   "\n rep mean:", round(repmeans[i], 3) , sep = "")
    p <- ggplot(data = storedat2, aes(X1)) + geom_histogram(fill = 'white', color = 'grey') +
                        geom_vline(xintercept = c(repmeans[i], trueatt[i]), color = c('red', 'blue'), lty = c('dashed', 'dotted'), size = 1) +
                        theme(text = element_text(size = 0.75)) +
                        theme_classic() + annotate(geom = 'text', x = quantile(storedat2$X1, 0.007), y = 500, label = lab, size = 1.75) + ylim(c(0, 600)) + 
                        xlab(str_c("att est ", "(", period[i], ")" )) + ylab("frequency") 
    plist[[i]]  <- p  
}

 f  <- (plist[[1]] + plist[[2]]) / (plist[[3]] + plist[[4]]) /  (plist[[5]] + plist[[6]])
 ggsave(filename = file.path(outpath, '02simulation_resultB.pdf'), height = 5.5, width = 4.5, units = "in")


####################################################
# CASE 3 = heterogeneous treatment effect by group 
####################################################
datsim  <- datsim  %>% 
            select(-att)

attvals  <- round(c(0, -1.3961822, -2.6846268,  0.2259470, -0.7944891, -0.7190134, -0.6610231), 3) #true atts
group  <- as.numeric(names(table(datsim$year_change_per)))
att  <- data.frame(year_change_per = group, att = attvals)
datsim  <- datsim %>% 
            left_join(att, by = "year_change_per")

attvals  <- attvals[-1] # get rid of 0
storelist3  <- mclapply(reps, GTatt, mc.cores = numcores, mc.set.seed=TRUE)

store  <- matrix(0, nrow = length(reps), ncol = length(attvals))
for (i in 1:length(reps)){
    store[i, ]  <- storelist3[[i]][[3]]
} 

storedat  <- data.frame(store) 
repmeans  <- sapply(storedat, mean)
trueatt  <- attvals
period  <- paste(seq(2005, 2010, 1), "grp", sep = " ")

plist  <- list()

for (i in 1:ncol(storedat)) {
    storedat3  <- data.frame(X1 = storedat[,i])
    lab  <-  paste0("true effect:", trueatt[i],   "\n rep mean:", round(repmeans[i], 3) , sep = "")
    p <- ggplot(data = storedat3, aes(X1)) + geom_histogram(fill = 'white', color = 'grey') +
                        geom_vline(xintercept = c(repmeans[i], trueatt[i]), color = c('red', 'blue'), lty = c('dashed', 'dotted'), size = 1) +
                        theme(text = element_text(size = 0.75)) +
                        theme_classic() + annotate(geom = 'text', x = quantile(storedat3$X1, 0.99), y = 500, label = lab, size = 1.75) + ylim(c(0, 600)) + 
                        xlab(str_c("att est ", "(", period[i], ")" )) + ylab("frequency") 
    plist[[i]]  <- p  
}


 f  <- (plist[[1]] + plist[[2]]) / (plist[[3]] + plist[[4]]) /  (plist[[5]] + plist[[6]])
 ggsave(filename = file.path(outpath, '02simulation_resultC.pdf'), height = 5.5, width = 4.5, units = "in")

