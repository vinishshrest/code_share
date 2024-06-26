# BRFSS Smart data clean

library(RJSONIO)
library(jsonlite)

# Link Location SMART: BRFSS City and County Data and Documentation

date <- c(2002:2010, 2014:2020)

f1 <- paste("MMSA0", seq(2, 9), "XPT.ZIP", sep = "")   # name of the files
f2 <- paste("MMSA", c(10, 14), "XPT.ZIP", sep = "")   # name of the files
f3 <- paste("mmsa", c(2015), "_XPT.ZIP", sep = "")
f4 <- paste("MMSA", c(seq(2016, 2019)), "_XPT.ZIP", sep = "")
f5 <- "MMSA20_XPT.zip"
filenam <- c(f1, f2, f3, f4, f5)
outdir <- "/home/user1/Dropbox/cigtaxes_new/data/BRFSS_MSA" # file where unzipped file will be stored in 

#shut this off after running once
  for(i in 1:length(filenam)) {
    if(date[i] <= 2016){
      destfile <- paste("/home/user1/Dropbox/cigtaxes_new/data/BRFSS_MSA/BRFSS", date[i], sep="_") #path where the file is saved
      # url <- paste("https://www.cdc.gov/brfss/smart/",i,"CNTY02XPT.ZIP", sep = "/") #url link where the file is located in cloud
      url <- paste("https://www.cdc.gov/brfss/smart", date[i], filenam[i], sep = "/")
      download.file(url, destfile)
      unzip(paste("/home/user1/Dropbox/cigtaxes_new/data/BRFSS_MSA/BRFSS", date[i], sep="_"), exdir = outdir)
    }else{
      destfile <- paste("/home/user1/Dropbox/cigtaxes_new/data/BRFSS_MSA/BRFSS", date[i], sep="_") #path where the file is saved
      # url <- paste("https://www.cdc.gov/brfss/smart/",i,"CNTY02XPT.ZIP", sep = "/") #url link where the file is located in cloud
      url <- paste("https://www.cdc.gov/brfss/annual_data", date[i], "files", filenam[i], sep = "/")
      download.file(url, destfile)
      unzip(paste("/home/user1/Dropbox/cigtaxes_new/data/BRFSS_MSA/BRFSS", date[i], sep="_"), exdir = outdir)
    }
  }
  
  
