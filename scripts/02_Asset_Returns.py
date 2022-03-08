import numpy as np
import pandas as pd
import datetime as dt

def get_returns(df, ticker):
  
  for t in ticker:
    df['Daily Rtn', t] = np.log(df['Adj Close', t] / df['Adj Close', t].shift())
    
  df.dropna(inplace = True)
    
  return df
