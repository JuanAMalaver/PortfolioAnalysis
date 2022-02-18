import pandas as pd
import pandas_datareader.data as web
import datetime as dt

def get_stock_data(ticker):
  
  df = web.DataReader(ticker, 'yahoo', start = '2021-02-18', end = '2022-02-18')
  
  return df
