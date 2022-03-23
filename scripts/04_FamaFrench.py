# Libraries  --------------------------------------------------------------

import numpy as np
import pandas as pd
import datetime as dt

from pandas_datareader.famafrench import get_available_datasets

from sklearn.linear_model import LinearRegression


# Asset Prices -------------------------------------------------------------

# list of relevant tickers
ticker = ['NVDA', 'AAPL']

# importing daily asset data
df = get_stock_data(ticker = ticker, start = '2017-03-24', end = '2022-03-23')

# calculating daily returns
for t in ticker:
  df['Daily Rtn', t] = np.log(df['Adj Close', t] / df['Adj Close', t].shift())
  
# dropping null values
df.dropna(inplace = True)

# inspecting data
df


# Fama French --------------------------------------------------------------

# inspecting fama french datasets
get_available_datasets()

# importing fama french factors
ds = web.DataReader('F-F_Research_Data_Factors_daily', 'famafrench')[0]

test = web.DataReader('F-F_Research_Data_Factors_weekly', 'famafrench')[0]

test

# inspecting data
ds

# joining prices and fama french data
df = pd.merge(df['Daily Rtn'], ds, left_index = True, right_index = True)

# inspecting data
df

# Linear Regression -------------------------------------------------------

# creating feature array with 3 factors
X = df[['Mkt-RF', 'SMB', 'HML']].to_numpy() / 100

X

# creating target array with daily returns
y = (df['NVDA'] - df['RF']).to_numpy()

y

# delcaring model object
model = LinearRegression()

# fitting model to features and target
model.fit(X, y)

# printing alpha
print('alpha:', model.intercept_)

# printing coefficients
print('betas:', model.coef_)
               
