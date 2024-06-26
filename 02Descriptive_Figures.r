dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))

datsum <- dat %>% mutate(group = ifelse(treat == 0 & tax_change0203 == 0, "never treated (2002-10)", "A"), # nolint
                      group = ifelse(treat == 1 & year_change_per < 2006, "early treated (2004-05)", group), # nolint
                      group = ifelse(treat == 1 & year_change_per >= 2006, "late treated (2006-10)", group), # nolint
                      group = ifelse(tax_change0203 == 1, "treated (2002-03)", group)) %>% # nolint
                  group_by(year, group) %>%
                  summarize(current_smoker = mean(current_smoker, na.rm = T)) # nolint

datsum <- data.frame(datsum)

ggplot(datsum, aes(year, current_smoker, group = group, color = group, shape = group)) + geom_point(size = 4) + # nolint
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  # nolint
panel.background = element_blank(),  axis.line = element_line(color = "black"),
axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1), 
legend.position = c(0.8, 0.8), text = element_text(size = 17)) +
ylab("% current smokers") 


