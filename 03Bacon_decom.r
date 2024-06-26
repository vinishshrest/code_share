#################################
#################################
#
# Desc: Bacon decomposition
# generate table for weights used for the components of TWFE: 
# i) earlier vs. later treated; ii) later vs. always treated; 
# iii) later vs. earlier treated; iv) treated vs. untreated
#
# generate figure to denote the fraction of time a group (defined 
# by the treatment year) serves as treated vs. control group 
#################################
#################################

# read in data
dat <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART04to10.csv"))
dat2 <- read.csv(file.path(datapath, "BRFSS_MSA", "BRFSS_SMART14to19.csv"))

# run bacon decomposition 2004 to 2010      
df_bacon0 <- bacon(current_smoker ~ tax_change,
                  data = subset(dat),
                  id_var = "msa_id",
                  time_var = "year")

ggplot(df_bacon0) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()

# run bacon decomposition 2015 to 2020   
df_bacon1 <- bacon(current_smoker ~ tax_change,
                  data = subset(dat2),
                  id_var = "msa_id",
                  time_var = "year")

ggplot(df_bacon1) +
  aes(x = weight, y = estimate, shape = factor(type)) +
  labs(x = "Weight", y = "Estimate", shape = "Type") +
  geom_point()

# types 
type1 <- names(table(df_bacon0$type))

# function for summary of bacon decomposition
bacon_summary <- function(types, bc) {
    store <- matrix(0, nrow = length(types), ncol = 3)
    for(i in 1:length(types)) {
        twfe_estimate <- sum(bc$weight * bc$estimate)
        EvsL <- sum(bc$weight[bc$type != types[i]] * bc$estimate[bc$type !=  types[i]])  # not type i
        sumwt <- sum(bc$weight[bc$type == types[i]])    # weight for type i

        store[i, 1] <- types[i]
        store[i, 2] <- round(sumwt, digits = 3)
        store[i, 3] <- round((twfe_estimate - EvsL) / sumwt, digits = 3)  # avg. estimate for type i
    }
    colnames(store) <- c("Type", "Weight", "Average Estimate")
    return(data.frame(store))
}

bc1 <- bacon_summary(types = type1, bc = df_bacon0)
bc2 <- bacon_summary(types = type1, bc = df_bacon1)
bc <- merge(bc1, bc2, by = "Type")
colnames(bc) <- c("Type", "weight (04-10)", "avg. estimate (04-10)", "weight (15-20)", "avg. estimate (15-20)") # nolint

bc <- xtable(bc,  type = "latex", align = c("l", rep("c", 5))) # nolint
print(bc, floating = FALSE, size="\\fontsize{11pt}{11pt}\\selectfont", file = paste(outpath, "bacon_decom1.tex", sep = "/")) # nolint


#############################
#
# Treatment-Control Weight 
#
#############################

tc_weight <- function(dat) {
    df1 <- dat
       # nolint

    df1a <- data.frame(df1 %>%
        group_by(treated) %>%
        summarize(weight_t = sum(weight))
    )
    colnames(df1a) <- c("year", "weight_t")

    df1b <- df1 %>%
        group_by(untreated) %>%
        summarize(weight_u = sum(weight))

    colnames(df1b) <- c("year", "weight_u")

    df1 <- merge(df1a, df1b, by = "year") %>%
       mutate(weight_tu = weight_t - weight_u)

return(df1)
}

df <- rbind(tc_weight(dat = df_bacon0), tc_weight(dat = df_bacon1))

# only focus on early vs later and later vs early treated groups 
# to emphasize variation due to the treatment timing
df_bacon0_short <- df_bacon0 %>%
    filter(type != "Later vs Always Treated" & type != "Treated vs Untreated")

df_bacon1_short <- df_bacon1 %>%
    filter(type != "Later vs Always Treated" & type != "Treated vs Untreated")

df1 <- rbind(tc_weight(dat = df_bacon0_short), tc_weight(dat = df_bacon1_short))

store <- matrix(0, ncol =2, nrow = nrow(df))
years <- as.numeric(names(table(df$year)))

j <- min(years)
for(i in 1:length(years)) {
    if(j <= 2010) {
    dat <- dat %>%
       mutate(year_change_dum = ifelse(year_change_per == years[i], 1, 0))
    store[i, 2] <- mean(dat$year_change_dum)
    store[i, 1] <- years[i]
    }else{
    dat <- dat2 %>%
       mutate(year_change_dum = ifelse(year_change_per == years[i], 1, 0))
    store[i, 2] <- mean(dat$year_change_dum)
    store[i, 1] <- years[i]
    }
    j <- j + 1
}

colnames(store) <- c("year", "sample_size")

colnames(df1) <- c("year", "weight_t2", "weight_u2", "weight_tu2")

df <- merge(df, df1, by = "year")

df <- merge(df, store, by = "year") %>% 
            select(c(year, weight_tu, weight_tu2, sample_size)) %>%
            pivot_longer(!year, names_to = "type", values_to = "vals")

df <- data.frame(df) 
df$type[df$type == "weight_tu"] <- "weight diff."
df$type[df$type == "weight_tu2"] <- "weight diff. timing only"
df$type[df$type == "sample_size"] <- "sample_share"

# plot Treated/Untreated Weight Difference
f <- ggplot(df, aes(x = year, y = vals, shape = type)) + geom_point(size = 3.5) +   # nolint
theme_bw() + xlab("treatment year") + ylab("weight for each treatment group \n and sample size") + # nolint
theme(text = element_text(size = 17))

# save
ggsave(f, file =  file.path(outpath, "bacon_decom_fig.pdf"), width = 10, height = 5)  # nolint
