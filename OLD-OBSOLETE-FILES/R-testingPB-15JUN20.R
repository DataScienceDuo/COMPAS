library(readr)
library(IDPmisc)


compas_scores_raw <- read_csv("C:/Users/pablo/OneDrive/Escritorio/Proyecto Final WozU Git/COMPAS/compas-scores-raw.csv")
View(compas_scores_raw)


compas_scores2 <- na.omit(compas_scores_raw)

# To remove NA´s fields
compas_scores3 <- NaRV.omit(compas_scores_raw)
# No change.

# We remove rows for Persons with "weird" birthdates 
compas_scores4 <-compas_scores3[!(compas_scores3$Person_ID=="51157" | compas_scores3$Person_ID=="57823"),]
compas_scores5 <-compas_scores4[!(compas_scores4$Person_ID=="62384" | compas_scores4$Person_ID=="54272"),]

# examples of removing rows using a list/vector
# install.packages("Hmisc")
# library("Hmisc")
# datawithoutVF = data[which(rownames(data) %nin% remove), ]
# datawithoutVF = data[!row.names(data)%in%remove,]
# datawithoutVF = data[ !(row.names(data) %in% remove), ]

# Libraries needed for regression. (not in use YET, for next steps use)
install.packages("caret")
install.packages("e1071")
install.packages("predictmeans")
install.packages("gvlma")
install.packages("popbio")

library("car")
library("caret")
library("gvlma")
library("predictmeans")
library("e1071")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")

# We remove columns to keep the ones that we will use as IV
# We need to make a decision on what columns we will keep & if (and how) we will link this data with rows on other files




# Testing Logistic regression on COMPAS PROPUBLICA FILE
library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")

# read dataset processed by probublica.
datasets_1498_2680_propublicaCompassRecividism_data_fairml_csv_propublica_data_for_fairml_1_ <- read_csv("C:/Users/pablo/OneDrive/Escritorio/Proyecto Final WozU Git/COMPAS/datasets_1498_2680_propublicaCompassRecividism_data_fairml.csv_propublica_data_for_fairml (1).csv")
# Just to have a "more maneagable name"
logit_input <- datasets_1498_2680_propublicaCompassRecividism_data_fairml_csv_propublica_data_for_fairml_1_

# We will check how good predictors are some IV (like race, etc)
# Now we do a trial with a one-to-one (one IV, one DV), then all columns as IV except Two_yr_Recidivism

ylogit_propub <- glm(Two_yr_Recidivism ~ ., data=logit_input, family="binomial")

summary(ylogit_propub)

probabilities <- predict(ylogit_propub, type = "response")


###############################################
# This section creates a function called      #
# logisticPseudoR2s().  To use it             #
# type logisticPseudoR2s(myLogisticModel)     #
###############################################
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <-  length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}
############End of function ######################


logisticPseudoR2s(ylogit_propub)

# Backward step
step(ylogit_propub,direction = 'backward')

# Backward step doesn´t "keep" african. (HOWEVER KEEPS SEX AND AGE ?? hOW DO WE DRAW ADITIONAL CONCLUTIONS ON THIS?)
# We need to discuss if the remaing factors lead us to any conclusion
# ################
# ################ RESULT AFTER LAST STEP:
# ################
# Step:  AIC=7470.05
# Two_yr_Recidivism ~ Number_of_Priors + score_factor + Age_Above_FourtyFive + 
#   Age_Below_TwentyFive + Female + Misdemeanor
# 
# Df Deviance    AIC
# <none>                      7456.1 7470.1
# - Misdemeanor           1   7465.4 7477.4
# - Female                1   7482.9 7494.9
# - Age_Above_FourtyFive  1   7502.9 7514.9
# - Age_Below_TwentyFive  1   7513.5 7525.5
# - score_factor          1   7588.1 7600.1
# - Number_of_Priors      1   7765.7 7777.7
# 
# Call:  glm(formula = Two_yr_Recidivism ~ Number_of_Priors + score_factor + 
#              Age_Above_FourtyFive + Age_Below_TwentyFive + Female + Misdemeanor, 
#            family = "binomial", data = logit_input)
# 
# Coefficients:
#   (Intercept)      Number_of_Priors          score_factor  Age_Above_FourtyFive  Age_Below_TwentyFive                Female           Misdemeanor  
# -0.8046                0.1327                0.7187               -0.5197                0.5420               -0.3741               -0.1814  
# 
# Degrees of Freedom: 6171 Total (i.e. Null);  6165 Residual
# Null Deviance:	    8506 
# Residual Deviance: 7456 	AIC: 7470


#  REMOVE SOME COLUMNS : 
# 
#
#
head(compas_scores5)
# Next line we create a vector with all colunms to be removed.
dropfromraw <- c("Person_ID", "AssessmentID", "Case_ID", "LastName", "FirstName", "MiddleName", "Screening_Date", "ScaleSet", "Screening_Date", "RecSupervisionLevelText", "DisplayText", "RawScore", "AssessmentReason","IsCompleted", "IsDeleted" )
compas_scores_redcol = compas_scores5[,!(names(compas_scores5) %in% dropfromraw)]
head(compas_scores_redcol)
# Here we try with both lm & glm. Should we use family="binomial"??
#
ylogit_compas_scores_redcol <- glm(DecileScore ~ ., data=compas_scores_redcol)
# This line hang the R environment ???