###############################################
############################################################
#
# Description: Master file for Revisting the cigarette 
#              taxation
# 
############################################################
###############################################

# create a new environment and store codepath there 
# so that codepath is not cleared by rm() in the beginning of each code

e <- new.env()
e$codepath <- "/Users/vshrestha/Dropbox/cigtaxes_new/code"
e$start.time  <- Sys.time()
attach(e, name = 'myvars')

rm(list = ls()) # Remove all in global env
codepath # Still there!


# 0. data clean files for BRFSS SMART 2004-2010 and 2015-2020 samples
# don't run as of now
# source(file.path(codepath, "02BRFSS_dataclean02to10 .r"))
# source(file.path(codepath, "02BRFSS_dataclean14to20.r"))


# 1. map 
# source(file.path(codepath, "MMSAmap.R"))

# 2. heat map for weights 
source(file.path(codepath, "05heatmap_negweight.r"))

# 3. Simulation -- performance of TWFE
source(file.path(codepath, "05simulation.r")) 

# 4. Bacon decomposition 
source(file.path(codepath, "03Bacon_decom.r"))

# 5 & 6. Event Study estimates 
source(file.path(codepath, "03BEstimation_ES.r"))

# 7. CS event-study type estimator 2004-2010 
source(file.path(codepath, "03CEstimation_CS.r"))

# 8 & 9. CS event-study type estimator 2015-2020 
source(file.path(codepath, "03CEstimation_CS_part2.r"))

# Appendix results 

#  only consist of units
# that are exposed to the treatment for at least 3 years following the treatment year

source(file.path(codepath, "03AEstimationTWFE_robustness.r"))

source(file.path(codepath, "04CS_balance.r"))

source(file.path(codepath, "04CS_balance.r"))

# simulation to assess the performance of CS estimator
source(file.path(codepath, "06simulation.r"))

# elasticity estimate by the group defined as the year of tax change
source(file.path(codepath, "07elasticity.R"))

end.time  <-  Sys.time() 

print(paste("time taken: ", end.time - start.time))