import numpy as np
import pandas as pd
import datetime as dt

from sklearn.linear_model import LinearRegression

def get_capm(df):

  X = df['Daily Rtn', 'SPY'].to_numpy().reshape((-1, 1))
  
  model = []
  
  for t in ticker[:-1]
  
    y = df['Daily Rtn', t].to_numpy()
    
    lm = LinearRegression()
    
    lm.fit(X, y)
    
    model.append(lm)
  
  return model
