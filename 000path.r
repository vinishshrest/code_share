# path

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
library(ggfixest) # for ggiplot
#library(fuzzyjoin)
library(xtable)
library(did)
library(stargazer)
library(grid)


user <- 2
# ubuntu office path
if (user == 1) {
        datapath <- file.path("/home/user1/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/home/user1/Dropbox/cigtaxes_new", "output")
} # mac path 
if  (user == 2){
        datapath <- file.path("/Users/vshrestha/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/Users/vshrestha/Dropbox/cigtaxes_new", "output") 
}else{ # ubuntu home path
        datapath <- file.path("/home/vinish/Dropbox/cigtaxes_new", "data")
        outpath <-  file.path("/home/vinish/Dropbox/cigtaxes_new", "output")
}

