import pandas as pd
import numpy as np

# ---------------------------
# Using Pandas for imputation
# (tesing)
# ---------------------------

# Setting up data frame with missing data (df)
tmp = pd.DataFrame(np.random.randn(100,10))
df = tmp[tmp>-1.5]

# Short analysis of missing data
df.describe()
df.info()

# SIMPLE IMPUTATIONS
# Mean imputation
df1 = df.fillna(df.mean())

# Hot Deck (replacing missing with random values from data set)
x = None
while x is None:
    x = df.sample(n=1)[pd.Series(np.random.randint(0, 9, size=1))].iloc[0] # Selecting random value from observed
df2 = df.fillna(x.iloc[0]) 

# IMPUTED DATA ANALYSIS
df1.describe()
df2.describe()
