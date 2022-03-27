# Libraries  --------------------------------------------------------------

import numpy as np
import pandas as pd
import datetime as dt
import pandas_datareader.data as web

from pandas_datareader.famafrench import get_available_datasets
from sklearn.linear_model import LinearRegression


# helper functions ------------------------------------------------------------
def ask_user_for_tickers():
    print("Type in tickers to analyze. Enter blank when done.")

    list = []
    end_flag = False

    while(not end_flag):
        user_input = input("Input ticker: ")
        if(not user_input):
            end_flag = not end_flag
        else:
            list.append(user_input)
    return list


def get_stock_data(ticker, start, end):
    df = web.DataReader(ticker, 'yahoo', start = start, end = end)
    return df


# Asset Prices -------------------------------------------------------------
def get_asset_prices(start_date, end_date, tickers):
    # importing daily asset data
    df = get_stock_data(ticker = tickers, start = start_date, end = end_date)

    # calculating daily returns
    for t in tickers:
        df['Daily Rtn', t] = np.log(df['Adj Close', t] / df['Adj Close', t].shift())
    
    # dropping null values
    df.dropna(inplace = True)

    # inspecting data
    return df


# Fama French --------------------------------------------------------------
def fama_french(asset_price_df):
    # importing fama french factors
    ds = web.DataReader('F-F_Research_Data_Factors_daily', 'famafrench')[0]
    # joining prices and fama french data
    ff_df = pd.merge(asset_price_df['Daily Rtn'], ds, left_index = True, right_index = True)

    return ff_df


# Linear Regression -------------------------------------------------------
def lin_reg(df, ticker):
    # creating feature array with 3 factors
    x = df[['Mkt-RF', 'SMB', 'HML']].to_numpy() / 100

    # creating target array with daily returns
    y = (df[ticker] - df['RF']).to_numpy()

    # delcaring model object
    model = LinearRegression()

    # fitting model to features and target
    model.fit(x, y)

    # printing alpha
    alpha = model.intercept_
    betas = model.coef_

    output = {}
    output['alpha'] = alpha
    output['betas'] = betas
    return output

# do Fama French flow
def do_fama_french():
    tickers = ask_user_for_tickers()
    start_date = "2017-01-01"
    end_date = "2022-02-28"
    
    asset_price_data_frame = get_asset_prices(start_date, end_date, tickers)
    fama_french_data_frame = fama_french(asset_price_data_frame)

    outputs = {}

    for t in tickers:
        outputs[t] = lin_reg(fama_french_data_frame, t)
        # (alphas, betas) = lin_reg(fama_french_data_frame, t)

    print(outputs)

do_fama_french()
