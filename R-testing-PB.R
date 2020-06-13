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

