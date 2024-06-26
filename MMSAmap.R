  rm(list = ls())
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)
  library("rgdal")
  library(foreign)
  library(rgeos)
  library(maptools)
  library(raster)
  library("ggpubr")
  library(gpclib)

user <- 2
if (user == 1) {
        datapath <- file.path("/home/user1/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/home/user1/Dropbox/cigtaxes_new", "output")
}else{
        datapath <- file.path("/home/vinish/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/home/vinish/Dropbox/cigtaxes_new", "output")
}


########################
# cbsa file 
########################
cbsa <- readOGR("/home/vinish/Dropbox/cigtaxes_new/data/tl_2019_us_cbsa", layer = "tl_2019_us_cbsa")  # nolint
head(cbsa@data)
cbsa <- fortify(cbsa, region = "GEOID")
cbsa <- subset(cbsa, long > -125 & lat > 23)


filenam <- c("BRFSS_SMART04to10.csv", "BRFSS_SMART14to19.csv")

for (i in 1:length(filenam))  {

    dat <- read.csv(file.path(datapath, "BRFSS_MSA", filenam[i])) 

    dat <- dat %>% dplyr::select(msa_id) %>%
                dplyr::mutate(ind = 1) %>%
                dplyr::distinct(msa_id, .keep_all = TRUE)

    dat_cbsa <- cbsa %>% merge(dat, by.x = "id", by.y = "msa_id", all.x = TRUE)
    dat_cbsa <- dat_cbsa[order(dat_cbsa$order),]
    dat_cbsa <- dat_cbsa %>% dplyr::mutate(ind = ifelse(is.na(ind) == TRUE, 0, ind)) # nolint

  if (i == 1) {
    map_cbsa1 <- ggplot(data = dat_cbsa, aes(x = long, y = lat, group = group))
    map_cbsa1 <- map_cbsa1 + geom_path() + theme_bw()
    map_cbsa1 <- map_cbsa1 + geom_polygon(aes(fill = factor(ind)), color = "black", size = 0.05) + # nolint
            theme(legend.position = "none") + ggtitle("Panel A. MMSAs in BRFSS SMART balanced panel 2004-2010") # nolint
  } else{
    map_cbsa2 <- ggplot(data = dat_cbsa, aes(x = long, y = lat, group = group))
    map_cbsa2 <- map_cbsa2 + geom_path() + theme_bw()
    map_cbsa2 <- map_cbsa2 + geom_polygon(aes(fill = factor(ind)), color = "black", size = 0.05) + # nolint
            theme(legend.position = "none") + ggtitle("Panel B. MMSAs in BRFSS SMART balanced panel 2015-2020") # nolint

  }
}

ggsave(ggarrange(map_cbsa1, map_cbsa2, ncol = 1, nrow = 2),
file =  file.path(outpath, "map_MMSA.pdf"), height = 10, width = 6)


#################### Smoking Sentiments Map #########################

########################
# cbsa file 
########################
cbsa <- readOGR("/home/vinish/Dropbox/cigtaxes_new/data/tl_2019_us_cbsa", layer = "tl_2019_us_cbsa")  # nolint
head(cbsa@data)
cbsa <- fortify(cbsa, region = "GEOID")
cbsa <- subset(cbsa, long > -125 & lat > 23)

senti <- read.csv(file.path(datapath, "smoking_sentiments.csv"))

dat_senti <- cbsa %>% merge(senti, by.x = "id", by.y = "cbsacode", all.x = TRUE)

map_cbsa1 <- ggplot(data = dat_senti, aes(x = long, y = lat, group = group))
map_cbsa1 <- map_cbsa1 + geom_path() + theme_bw()
map_cbsa1 <- map_cbsa1 + geom_polygon(aes(fill = factor1), color = "black", size = 0.05) + # nolint
            theme(legend.position = "none") + ggtitle("Panel A. MMSAs in BRFSS SMART balanced panel 2002-2010")