# REV 17JUN20
# PRELIMINARY WRANLING DONE AT THE BEGINNING
install.packages("caret")
install.packages("e1071")
install.packages("predictmeans")
install.packages("gvlma")
install.packages("popbio")
install.packages("anytime")


# Libraries needed for regression. (not in use YET, for next steps use)
library("readr")
library("IDPmisc")
library("anytime")
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

#############################################################################################################################################################
# We load all needed datafiles (we have 4 datafiles)
# DATA LOADING FROM FILES #
#
compas_scores_raw <- read_csv("C:/Users/pablo/OneDrive/Escritorio/Proyecto Final WozU Git/COMPAS/compas-scores-raw.csv")
cox_violent_parsed <- read_csv("C:/Users/pablo/OneDrive/Escritorio/Proyecto Final WozU Git/COMPAS/COMPAS DATA FILES INPUT/cox-violent-parsed.csv")
cox_violent_parsed_filt <- read_csv("C:/Users/pablo/OneDrive/Escritorio/Proyecto Final WozU Git/COMPAS/COMPAS DATA FILES INPUT/cox-violent-parsed_filt.csv")
#
#
#############################################################################################################################################################


# To remove NA´s fields
compas_scores2 <- na.omit(compas_scores_raw)
compas_scores3 <- NaRV.omit(compas_scores_raw)
# No change, after running the line above. :)

# We remove rows for Persons with "weird" birthdates 
# after group virtual zoom we decided to remove these four rows:
compas_scores4 <-compas_scores3[!(compas_scores3$Person_ID=="51157" | compas_scores3$Person_ID=="57823"),]
compas_scores5 <-compas_scores4[!(compas_scores4$Person_ID=="62384" | compas_scores4$Person_ID=="54272"),]


######################################################################
# examples of removing rows using a list/vector
# install.packages("Hmisc")
# library("Hmisc")
# datawithoutVF = data[which(rownames(data) %nin% remove), ]
# datawithoutVF = data[!row.names(data)%in%remove,]
# datawithoutVF = data[ !(row.names(data) %in% remove), ]
######################################################################

# We merge first and last name into a new column named ID_name
compas_scores5$ID_name <- paste(compas_scores5$FirstName,compas_scores5$LastName)
write.csv(compas_scores5,"compas_scores_raw_IDname.csv", row.names = TRUE)
#
#
# we change column "name" to "ID_name"
names(cox_violent_parsed_filt)[names(cox_violent_parsed_filt)=="name"] <- "ID_name"
write.csv(cox_violent_parsed_filt, "cox_violent_parsed_filt_IDname.csv", row.names = TRUE)

# We remove columns to keep the ones that we will use as IV
# We need to make a decision on what columns we will keep & if (and how) we will link this data with rows on other files
# (THIS WILL BE DONE BELOW)

########################################################################################################################
###################################### PART 2 DATA TESTING BACK AND FORTH PROCESS ######################################
########################################################################################################################

# Testing Logistic regression on COMPAS PROPUBLICA FILE (using binary output recividism as DV Variable)


# read dataset processed by probublica.
datasets_1498_2680_propublicaCompassRecividism_data_fairml_csv_propublica_data_for_fairml_1_ <- read_csv("C:/Users/pablo/OneDrive/Escritorio/Proyecto Final WozU Git/COMPAS/datasets_1498_2680_propublicaCompassRecividism_data_fairml.csv_propublica_data_for_fairml (1).csv")
head(datasets_1498_2680_propublicaCompassRecividism_data_fairml_csv_propublica_data_for_fairml_1_)
# Just to have a "more maneagable name"
logit_input <- datasets_1498_2680_propublicaCompassRecividism_data_fairml_csv_propublica_data_for_fairml_1_

# We will check how good predictors are some IV (like race, etc)
# Now we do a trial with a one-to-one (one IV, one DV), then with all the factors/IV
# Here our DV is Two_yr_Recidivism
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

# Now we do a backward regression, let´s see where the destiny takes us... ;)
step(ylogit_propub,direction = 'backward')


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


# NOTE/COMMENTS: 
# Backward step doesn´t "keep" african race as DV. (HOWEVER IT KEEPS SEX AND AGE ?? hOW DO WE DRAW ADITIONAL CONCLUTIONS ON THIS?)
# We need to discuss if the remaing factors lead us to any conclusion




###################################################################################################################################
# Here we will do tome work on  compas-scores-raw.csv file
###################################################################################################################################
# we REMOVE SOME COLUMNS : 
# 
#
#
head(compas_scores5)
# Next line we create a vector with all colunms to be removed. (feel free to change if needed)
dropfromraw <- c("Person_ID", "AssessmentID", "Case_ID", "LastName", "FirstName", "MiddleName", "Screening_Date", "ScaleSet", "Screening_Date", "RecSupervisionLevelText", "DisplayText", "RawScore", "AssessmentReason","IsCompleted", "IsDeleted" )
compas_scores_redcol = compas_scores5[,!(names(compas_scores5) %in% dropfromraw)]
head(compas_scores_redcol)
#
#
compas_scores_redcoldate <- compas_scores_redcol
# We recode DateOfBirth as a date field:
compas_scores_redcoldate$DateOfBirth <- as.Date(compas_scores_redcoldate$DateOfBirth, format = "%m/%d/%y")

# We convert to factors using as.factor() function for all the remaining columns that are not continuous IV
compas_scores_redcoldate$Agency_Text <- as.factor(compas_scores_redcoldate$Agency_Text)
compas_scores_redcoldate$Sex_Code_Text <- as.factor(compas_scores_redcoldate$Sex_Code_Text)
compas_scores_redcoldate$Ethnic_Code_Text <- as.factor(compas_scores_redcoldate$Ethnic_Code_Text)
compas_scores_redcoldate$Language <- as.factor(compas_scores_redcoldate$Language)
compas_scores_redcoldate$LegalStatus <- as.factor(compas_scores_redcoldate$LegalStatus)
compas_scores_redcoldate$CustodyStatus <- as.factor(compas_scores_redcoldate$CustodyStatus)
compas_scores_redcoldate$MaritalStatus <- as.factor(compas_scores_redcoldate$MaritalStatus)
compas_scores_redcoldate$ScoreText <- as.factor(compas_scores_redcoldate$ScoreText)

# We perform a "visual check" on our dataframe
head(compas_scores_redcoldate)

#This function shows how the dummy coding is performed by lm() in R - This line is just for testing verification purposes
# If needed we can use another column if we like to see how such columns are dummy coded by lm() funcion. Just change column name after $
contrasts(compas_scores_redcoldate$LegalStatus)


ylogit_compas_scores_redcoldate <- lm(DecileScore ~ ., data=compas_scores_redcoldate)
# summary also takes it´s time :)
summary(ylogit_compas_scores_redcoldate)









# we are HERE ON JUN 17TH !!!!
# We still ned to figure out how we can "connect" al four source data files in order to improve the quality of our conclutions!!!!


############################################################################################################################
############################################################################################################################
#
# Not Needed at the end of the day, after converting date column to date, RStudio stopped having issues with freeze issues.
# last test below run on June 16th
# We reduce the number of rows to 10K from 60k to be able to run on the computer, unless some times computer get frozen.
# compas_scores_sampled <- compas_scores_redcol[sample(nrow(compas_scores_redcol), 10000), ]
# head(compas_scores_sampled)
# ylogit_compas_scores_sampled <- lm(DecileScore ~ ., data=compas_scores_sampled)
# summary(ylogit_compas_scores_sampled)
# levels(compas_scores_redcoldate$Sex_Code_Text) <- c(1,0)
# compas_scores_redcoldate$Sex_Code_Text <- as.numeric(compas_scores_redcoldate$Sex_Code_Text)
#
# Warning! This can freeze computer is run on the full dataset / FIXED! no issues once you uses as.date() to recode DateOfBirth columns
# ylogit_compas_scores_redcoldate <- lm(DecileScore ~ ., data=compas_scores_redcoldate)
# summary also takes it´s time :)
# summary(ylogit_compas_scores_redcoldate)
# ???? our function only seems to work for logit regression... interesting.
# logisticPseudoR2s(ylogit_compas_scores_redcoldate)
#
#
#############################################################################################################################
#############################################################################################################################

