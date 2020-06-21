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


# To remove NA´s fields & We remove rows for Persons with "weird" birthdates 
compas_scores2 <- na.omit(compas_scores_raw)
compas_scores3 <- NaRV.omit(compas_scores_raw)
# No change, after running the line above. :)
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

# We merge first and last name into a new column named ID_name & we write a new csv file on current directory
compas_scores5$ID_name <- paste(compas_scores5$FirstName,compas_scores5$LastName)
write.csv(compas_scores5,"compas_scores_raw_IDname.csv", row.names = TRUE)
# we change column "name" to "ID_name" & we write a new csv file on current directory
names(cox_violent_parsed_filt)[names(cox_violent_parsed_filt)=="name"] <- "ID_name"
write.csv(cox_violent_parsed_filt, "cox_violent_parsed_filt_IDname.csv", row.names = TRUE)


# We remove columns to keep the ones that we will use as IV
# We need to make a decision on what columns we will keep & if (and how) we will link this data with rows on other files
# (THIS WILL BE DONE BELOW)

########################################################################################################################
###################################### PART 2 DATA TESTING BACK AND FORTH PROCESS ######################################
########################################################################################################################


# Testing LOGISTIC REGRESSION on COMPAS PROPUBLICA FILE (using binary output recividism as DV Variable)


# read dataset processed by probublica.
datasets_1498_2680_propublicaCompassRecividism_data_fairml_csv_propublica_data_for_fairml_1_ <- read_csv("C:/Users/pablo/OneDrive/Escritorio/Proyecto Final WozU Git/COMPAS/datasets_1498_2680_propublicaCompassRecividism_data_fairml.csv_propublica_data_for_fairml (1).csv")
head(datasets_1498_2680_propublicaCompassRecividism_data_fairml_csv_propublica_data_for_fairml_1_)
# Just to have a "more maneagable name"
logit_input <- datasets_1498_2680_propublicaCompassRecividism_data_fairml_csv_propublica_data_for_fairml_1_

# We will check how good predictors are some IV (like race, etc)
# Now we do a trial with a one-to-one (one IV, one DV), then with all the factors/IV
# Here our DV is Two_yr_Recidivism
ylogit_propub <- glm(Two_yr_Recidivism ~ ., data=logit_input, family="binomial")

probabilities <- predict(ylogit_propub, type = "response")


logit_input$Pred_rec <- ifelse(probabilities > .5,"pos","neg")
logit_input$Pred_recR <- NA
logit_input$Pred_recR[logit_input$Pred_rec=='pos'] <- 1
logit_input$Pred_recR[logit_input$Pred_rec=='neg'] <- 0

head(logit_input)

logit_input3 <- logit_input
logit_input3$Pred_recR <- as.factor(logit_input$Pred_recR)
logit_input3$Two_yr_Recidivism <- as.factor(logit_input$Two_yr_Recidivism)
head(logit_input3)

# We calculate the confusion matrix 
confusion_mat_recidivism <- caret::confusionMatrix(logit_input3$Two_yr_Recidivism, logit_input3$Pred_recR)
confusion_mat_recidivism

# Confusion Matrix and Statistics
# 
# Reference
# Prediction    0    1
#          0 2583  780
#          1 1202 1607
# 
# Accuracy : 0.6789          
# 95% CI : (0.6671, 0.6905)
# No Information Rate : 0.6133          
# P-Value [Acc > NIR] : < 2.2e-16       
# 
# Kappa : 0.3444          
# 
# Mcnemar's Test P-Value : < 2.2e-16       
#                                           
#             Sensitivity : 0.6824          
#             Specificity : 0.6732          
#          Pos Pred Value : 0.7681          
#          Neg Pred Value : 0.5721          
#              Prevalence : 0.6133          
#          Detection Rate : 0.4185          
#    Detection Prevalence : 0.5449          
#       Balanced Accuracy : 0.6778          
#                                           
#        'Positive' Class : 0   
#

# LOGIT LINEARITY
# We keep only fields that are numeric
logit_input1 <- logit_input %>% dplyr::select_if(is.numeric)
predictors_num <- colnames(logit_input1)
logit_input1 <- logit_input1 %>% mutate(logit=log(probabilities/(1-probabilities))) %>% gather(key= "predictors_num", value="predictor.value", -logit)
# Now we graph
ggplot(logit_input1, aes(logit, predictor.value))+ geom_point(size=.5, alpha=.5)+ geom_smooth(method= "loess")+ theme_bw()+ facet_wrap(~predictors_num, scales="free_y")
# looks linear in most of the range (however above 2.5 becomes non linear)

plot(ylogit_propub$residuals)
# We should check the plot we have positive/negative alternated values. Does this means something else?
# We run durbin whatson test for independence of errors
dwtest(ylogit_propub, alternative="two.sided")
# Durbin-Watson test
# 
# data:  ylogit_propub
# DW = 2.0276, p-value = 0.2781
# alternative hypothesis: true autocorrelation is not 0

# We check for outliers (now seems ok)
infl <- influence.measures(ylogit_propub)
summary(infl)



summary(ylogit_propub)
#
# Call:
#   glm(formula = Two_yr_Recidivism ~ ., family = "binomial", data = logit_input)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.5875  -0.9257  -0.6413   1.0355   2.0572  
# 
# Coefficients:
#                         Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)          -0.793407   0.067365 -11.778  < 2e-16 ***
#   Number_of_Priors      0.131516   0.008431  15.599  < 2e-16 ***
#   score_factor          0.706420   0.063352  11.151  < 2e-16 ***
#   Age_Above_FourtyFive -0.516489   0.077557  -6.660 2.75e-11 ***
#   Age_Below_TwentyFive  0.540449   0.071805   7.527 5.21e-14 ***
#   African_American      0.026285   0.063804   0.412  0.68037    
#   Asian                -0.519606   0.432354  -1.202  0.22944    
#   Hispanic             -0.120019   0.110257  -1.089  0.27635    
#   Native_American      -0.456590   0.655601  -0.696  0.48615    
#   Other                -0.058954   0.129012  -0.457  0.64769    
#   Female               -0.379289   0.072906  -5.202 1.97e-07 ***
#   Misdemeanor          -0.178525   0.059570  -2.997  0.00273 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 8506.4  on 6171  degrees of freedom
# Residual deviance: 7451.9  on 6160  degrees of freedom
# AIC: 7475.9
# 
# Number of Fisher Scoring iterations: 4

# We should check, about this line and output plot below: (is focussed on a single factor, here we have multiple, should need to think about another plot)
logi.hist.plot(logit_input$Two_yr_Recidivism,logit_input$Number_of_Priors, boxp=FALSE, type="hist", col="gray")





################################################
# This section creates a function called       #
# logisticPseudoR2s().  To use it              #
# type logisticPseudoR2s(myLogisticModel)      #
################################################
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

# Now we do a BACKWARD REGRESION, let´s see where the destiny takes us... ;)
step(ylogit_propub,direction = 'backward')



# ###############################################################################################################################
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
###################################################################################################################################

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
dropfromraw <- c("Person_ID", "AssessmentID", "Case_ID", "LastName", "FirstName", "MiddleName", "Screening_Date", "ScaleSet", "Screening_Date", "RecSupervisionLevelText", "DisplayText", "RawScore", "AssessmentReason","IsCompleted", "IsDeleted","ID_name")
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









# we are HERE ON JUN 19TH !!!!
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

