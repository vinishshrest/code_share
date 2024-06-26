##################
# last modified: April 18, 23

rm(list = ls())
# load libraries 
library("pacman")
pacman::p_load(dplyr, ggplot2, parallel, patchwork)

RNGkind("L'Ecuyer-CMRG")

############################################
#
# Set path
#
############################################
source(file.path(codepath, "000path.r"))

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
            tax_change, year_around, year_change_per)   %>%  
            filter(year_change_per != 2004)

# Function to declare the unit and year ids

genFE  <- function(data){ 
# 1. unit ids
    nunits  <- length(table(datsim$msa_id))
    namunit  <- as.numeric(names(table(datsim$msa_id)))
    # 1. generate unit fixed effect based on nunits
    unitfe  <- rnorm(nunits, mean = 0, sd = 20)
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

ggplot(data = genFE(datsim), aes(x = year, y = yearfe)) + geom_point()
##########################
#
# Write a generic function
# to fit all 3 cases
##########################
funsimtwfe  <- function(reps) { 

    # get the data with the unit and year FEs
    datsim  <-  genFE(data = datsim)

    # generate the error term and output
    datsim  <- datsim  %>% 
        mutate(err = rnorm(nrow(datsim), 0, 1), 
        Y = tax_change * att + yearfe + unitfe + err) 

    # TWFE regression
    reg  <- lm(Y ~ tax_change + factor(msa_id) + factor(year) -1, dat = datsim)
    store  <-  coefficients(reg)[[1]]

    #datsim  <- datsim %>% 
    #            select(-c(unitfe, yearfe))

    return(store)
}

#########################################
# Case 1. Homogeneous Treatment Effect 
#########################################
att  <- -1.2 
reps  <- seq(1, 5000, 1)

# simulation results from homogeneous treatment effect
#start_time  <- Sys.time()

makeplot  <- function(att, annox, annoy, title) {
    storelist  <- mclapply(reps, funsimtwfe, mc.cores = numcores, mc.set.seed=TRUE)
    # storelist  <- lapply(reps, funsimtwfe)
    store  <- unlist(storelist)
    store  <- data.frame(twfe = store)
    meanattreps  <- mean(store$twfe)
    lab  <-  paste0("average effect:", att,   "\n replication mean:", round(meanattreps, 3) , sep = "")

    f  <- ggplot(store, aes(twfe)) + geom_histogram(color = 'grey', fill = 'white') + 
            geom_vline(xintercept = c(meanattreps, att), color = c('red', 'blue'), lty = c('dashed', 'dotted'), size = 1) + 
            theme_classic() +
            xlab('twfe estimate') + ylab('frequency') +
            annotate(geom = 'text', x = annox, y = annoy, label = lab) + 
            ggtitle(title)
    return(f)
}
#end_time  <- Sys.time()
#tt  <- end_time - start_time
#print(tt)
f0  <- makeplot(att = att, annox = -1.5, annoy = 400, title = 'A. Homogeneous ATT')

##################################################
# Case 2. Heterogeneous treatment effect by unit
##################################################

attvals  <- c(0, -1.3961822, -2.6846268,  0.2259470, -0.7944891, -0.7190134, -0.6610231) #true atts
group  <- as.numeric(names(table(datsim$year_change_per)))
att  <- data.frame(year_change_per = group, att = attvals)
datsim  <- datsim %>% 
            left_join(att, by = "year_change_per")

reps <- seq(1, 5000)
meanatt2  <- mean(attvals[-1])

f1  <- makeplot(att = round(meanatt2, 3), annox = -1.05, annoy = 400, title = 'B. Heterogeneous ATT by unit')


#################################################
# Case 3: Heterogeneous treatment effect by time
#################################################
datsim  <- datsim  %>% 
            select(-att)

yeararound  <- sort(unique(datsim$year_around))
attvals  <- round(c(runif(5, -0.5, 0.5), 0, -0.3077, -1.2863, -0.8405, -1.3728, -1.5916, -1.8456), 3)
attdf  <- data.frame(year_around = yeararound, att = attvals)
datsim  <- datsim %>% 
            left_join(attdf, by = "year_around")

meanatt3  <- round(mean(attvals[-(1:6)]), 3)

f2 <- makeplot(att = meanatt3, annox = -1, annoy = 400, title = 'C. Heterogeneous ATT by time')

fig <- f0 / f1/  f2 
ggsave(filename = file.path(outpath, '01simulation_result.pdf'))
