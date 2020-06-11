# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from pylab import *
import seaborn as sns
# %matplotlib inline
import  statsmodels.api as sm
import statsmodels.stats.api as sms
from scipy.stats import boxcox

compas_raw_in0 = pd.read_csv(r'C:\Users\pablo\OneDrive\Escritorio\Proyecto Final WozU Git\COMPAS\compas-scores-raw.csv')

compas_raw_in0

print(compas_raw_in0)

# Here was testing options to remove "bad" ID´s
# compas_raw_in1 = compas_raw_in0.query("Person_ID != '51157'")
# compas_raw_in2 = compas_raw_in0[compas_raw_in0['Person_ID'] != 51157]
# compas_raw_in3 = compas_raw_in0[compas_raw_in0['Person_ID'].ne(51157)]
# sales.drop(sales[sales.CustomerId.isin(badcu)].index.tolist())

# We remove the inmates with the Person_ID we discard
badID= [51157, 57823, 62384, 54272]
compas_raw_in4 = compas_raw_in0[~compas_raw_in0.Person_ID.isin(badID)]
