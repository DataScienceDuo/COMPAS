# Based on R code by:
# title: "ProPublica's COMPAS Data Revisited"
# author: "Matias Barenstein"
# Updated on 04JUL2020



# Loading R libraries
# If you are running this yourself, you have to make sure you have these R packages already installed
library(dplyr)  
# library(ggplot2)  
# Not necessary to load ggplot2 library directly when load ggfortify
# but still must have the ggplot2 package installed
library(ggfortify)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidyr)  
library(lattice)
library(caret)
library(survival)
library(pROC)

# Reading in ProPublica's full dataset
df_full_entire <- read.csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores.csv', as.is = TRUE)
length_full_data <- length(unique(df_full_entire$id))
# This dataset contains the full sample of PRETRIAL defendants collected by ProPublica

# Additionally, of the remaining 11001 people
# ProPublica dropped 670 people who did not appear to have good data
# because ProPublica could not find case/arrest information on these people
# ProPublica tagged these with "is_recid" = -1 in their full dataset
# I also drop these 670 people from the full dataset
# to make it more comparable to ProPublica's two-year data

df_full <- df_full_entire[which(df_full_entire$id<11002),]
df_full <- df_full[which(df_full$is_recid!=-1),]

length_full_data_trimmed <- length(unique(df_full$id))

# Thus we end up with 10331 people total in the trimmed full dataset
# 11757 - 756 - 670 = 10331
# 7214 records

# Checking if unique people [this check/code is commented out, but could be run]

# nrow(df_propub_2_yr_csv)
# length(unique(df_propub_2_yr_csv$id))

# # Or if we check for unique combinations of last name, first name, and date of birth
# df_propub_2_yr_csv$name_dob <- paste(df_propub_2_yr_csv$name, df_propub_2_yr_csv$dob)
# length(unique(df_propub_2_yr_csv$name_dob))

# # All of these appear to be unique people



# Reading in ProPublica's two-year general recidivism dataset
df_propub_2_yr_csv <- read.csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/compas-scores-two-years.csv', as.is = TRUE)






#############################

# ProPublica two-year general recidivism dataset - 7214 total 

hi95 <- function(x) { binom.test(sum(x),length(x),0.5)$conf.int[2] }  
lo95 <- function(x) { binom.test(sum(x),length(x),0.5)$conf.int[1] }

X2 <- df_propub_2_yr_csv %>% select(decile_score, two_year_recid, sex)

tibble_propub <- X2 %>% group_by(sex, decile_score) %>%
  summarize(m=mean(two_year_recid),lo=lo95(two_year_recid),hi=hi95(two_year_recid)) 

gg_davies_goel_sex <- ggplot(tibble_propub, aes(x=decile_score, y=m, color = sex)) +
  geom_line(aes(linetype=sex, color=sex))+
  xlab(' ') + ylab('Two-year recidivism rate') +
  ylim(0, 1) + xlim(1, 10) +
  scale_x_continuous(breaks=seq(1,10,1)) + 
  scale_linetype_manual(name  ="sex",
                        labels=c("Female", "Male"),
                        values=c("Female"="solid", "Male"="dotted")) +
  scale_colour_manual(name  ="sex",
                      labels=c("Female", "Male"),
                      values=c("Female"="black", "Male"="black")) +
  theme(legend.position = c(0.25, 0.85)) +
  theme(legend.title=element_blank()) +
  theme(legend.background=element_blank()) + 
  geom_hline(yintercept = mean(X2$two_year_recid), linetype = "dashed") +
  annotate("text", x=3, y=mean(X2$two_year_recid)+.1, 
           label=paste("Mean =", round(mean(X2$two_year_recid),2)), colour="black", angle=0) 

########################################################################################
# Davies-Goel - Figure 2 [@2017arXiv170108230C]
# Recidivism Rate by Race by Score Decile
#	(Also Flores et al Figure 1 is very similar, and has the same issue - [@Flores_et_al]) 
########################################################################################

# ProPublica two-year general recidivism dataset (7214 total all race groups)

X2 <- df_propub_2_yr_csv %>% select(decile_score, two_year_recid, race)
X2 <- X2 %>% filter(race %in% (c("African-American","Caucasian")))

tibble_propub <- X2 %>% group_by(race, decile_score) %>%
  summarize(m=mean(two_year_recid),lo=lo95(two_year_recid),hi=hi95(two_year_recid)) 

gg_davies_goel_race <- ggplot(tibble_propub, aes(x=decile_score, y=m, color = race)) +
  geom_ribbon(data=tibble_propub[which(tibble_propub$race=="Caucasian"),], 
              aes(ymin=lo, ymax=hi), fill = "grey85", colour = NA) +
  geom_ribbon(data=tibble_propub[which(tibble_propub$race=="African-American"),], 
              aes(ymin=lo, ymax=hi), fill = "grey78", colour = NA) +
  geom_line() +
  xlab('COMPAS decile score') + ylab(' ') +
  ylim(0, 1) + xlim(1, 10) +
  scale_x_continuous(breaks=seq(1,10,1)) + 
  scale_colour_manual(name  ="Race",
                      labels=c("African-American", "Caucasian"),
                      values=c("African-American"="red", "Caucasian"="blue")) + 
  theme(legend.position = c(0.4, 0.9)) +
  theme(legend.title=element_blank()) +
  theme(legend.background=element_blank()) + 
  geom_hline(yintercept = mean(X2$two_year_recid), linetype = "dashed") +
  annotate("text", x=3, y=mean(X2$two_year_recid)+.1, 
           label=paste("Mean =", round(mean(X2$two_year_recid),2)), colour="black", angle=0) 

#############################
#############################

# ProPublica two-year general recidivism dataset (7214 total all race groups)

X2 <- df_propub_2_yr_csv %>% select(decile_score, two_year_recid, race)
X2 <- X2 %>% filter(race %in% (c("African-American","Caucasian")))

tibble_propub <- X2 %>% group_by(decile_score, race) %>%
  summarize(m=mean(two_year_recid),lo=lo95(two_year_recid),hi=hi95(two_year_recid)) 

Chouldechova <- ggplot(tibble_propub, aes(x=decile_score, y=m, fill = race)) +
  geom_bar(stat='identity', position = position_dodge()) +
  scale_fill_manual(name  ="Race",
                    labels=c("African-American", "Caucasian"),
                    values=c("African-American"="coral3", "Caucasian"="lightskyblue3")) +
  xlab(' ') + ylab(' ') +
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.4, 0.85)) +
  theme(legend.background=element_blank()) + 
  ylim(0,1) + 
  scale_x_continuous(breaks=seq(1,10,1))  +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept = mean(X2$two_year_recid), linetype = "dashed") +
  annotate("text", x=3, y=mean(X2$two_year_recid)+.15, 
           label=paste("Mean =", round(mean(X2$two_year_recid),2)), colour="black", angle=0) 

# Davies-Goel - Figure 1 [@2018arXiv180800023C] (...continued)
# Recidivism Rate by Sex by Score Decile
# Now replicate Figure


# Combining three graphs

davies_goel_plot_33 <- grid.arrange(gg_davies_goel_sex, gg_davies_goel_race, Chouldechova, ncol = 3,
                                   top = textGrob("Two-Year Recidivism Rate by COMPAS decile score by Sex and Race", 
                                                  gp=gpar(fontsize=12, font=2)))

##################################################################


############################
# END OF GRAPH (3-in-1)
############################



# Predictive Accuracy of COMPAS based on propublica original code.
# https://github.com/propublica/compas-analysis/blob/master/Compas%20Analysis.ipynb
#
df_cox_parsed <- read.csv('https://raw.githubusercontent.com/propublica/compas-analysis/master/cox-parsed.csv', as.is = TRUE)

library(survival)
library(ggfortify)

data <- filter(filter(df_cox_parsed, score_text != "N/A"), end > start) %>%
  mutate(race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other"))) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  mutate(score_factor = factor(score_text)) %>%
  within(score_factor <- relevel(score_factor, ref=2))

grp <- data[!duplicated(data$id),]
nrow(grp)


summary(grp$score_factor)


summary(grp$race_factor)

f <- Surv(start, end, event, type="counting") ~ score_factor
model <- coxph(f, data=data)
summary(model)

# Call:
#   coxph(formula = f, data = data)
# 
# n= 13344, number of events= 3469 
# 
#                       coef    exp(coef) se(coef)  z      Pr(>|z|)    
#   score_factorHigh   1.24969   3.48927  0.04146 30.14   <2e-16 ***
#   score_factorMedium 0.79627   2.21725  0.04077 19.53   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#                     exp(coef) exp(-coef) lower .95 upper .95
# score_factorHigh       3.489     0.2866     3.217     3.785
# score_factorMedium     2.217     0.4510     2.047     2.402
# 
# Concordance= 0.636  (se = 0.005 )
# Likelihood ratio test= 942.8  on 2 df,   p=<2e-16
# Wald test            = 954.8  on 2 df,   p=<2e-16
# Score (logrank) test = 1055  on 2 df,   p=<2e-16

decile_f <- Surv(start, end, event, type="counting") ~ decile_score
dmodel <- coxph(decile_f, data=data)
summary(dmodel)

# Call:
#   coxph(formula = decile_f, data = data)
# 
# n= 13344, number of events= 3469 
# 
#               coef    exp(coef) se(coef)     z    Pr(>|z|)    
# decile_score 0.194931  1.215228 0.005801 33.61   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
#               exp(coef) exp(-coef) lower .95 upper .95
# decile_score     1.215     0.8229     1.201     1.229
# 
# Concordance= 0.664  (se = 0.005 )
# Likelihood ratio test= 1112  on 1 df,   p=<2e-16
# Wald test            = 1129  on 1 df,   p=<2e-16
# Score (logrank) test = 1208  on 1 df,   p=<2e-16










###########################################################################################
############################################################################################
### NEW GRAPH FOR AGE ###
### UNDER CONSTRUCTION ####


X33 <- df_propub_2_yr_csv %>% select(age, decile_score, race)
X33 <- X33 %>% filter(race %in% (c("African-American","Caucasian")))


tibble_propub <- X33 %>% group_by(age, race) %>% summarize(m=mean(decile_score),lo=lo95(decile_score),hi=hi95(decile_score)) 


ScorebyAge <- ggplot(tibble_propub, aes(x=age, y=m, fill = race)) +
  geom_bar(stat='identity', position = position_dodge()) +
  scale_fill_manual(name  ="Race",
                    labels=c("African-American", "Caucasian"),
                    values=c("African-American"="coral3", "Caucasian"="lightskyblue3")) +
  xlab(' ') + ylab(' ') +
  theme(legend.position = "top") +
  theme(legend.title=element_blank()) +
  theme(legend.position = c(0.4, 0.85)) +
  theme(legend.background=element_blank()) + 
  ylim(0,1) + 
  scale_x_continuous(breaks=seq(1,10,1))  +
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.2,
                position=position_dodge(.9)) +
  geom_hline(yintercept = mean(X2$two_year_recid), linetype = "dashed") +
  annotate("text", x=3, y=mean(X2$two_year_recid)+.15, 
           label=paste("Mean =", round(mean(X2$two_year_recid),2)), colour="black", angle=0) 

ScorebyAge




