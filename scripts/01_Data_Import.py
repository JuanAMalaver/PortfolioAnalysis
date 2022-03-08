import pandas as pd
import pandas_datareader.data as web
import datetime as dt

def get_stock_data(ticker, start, end):
  
  df = web.DataReader(ticker, 'yahoo', start = start, end = end)
  
  return df
