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
# Now we do a trial with a one-to-one (one IV, one DV)
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


