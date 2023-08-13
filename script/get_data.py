import csv
import json
import os

import pandas as pd
import yfinance as yf


# defining the function that make csv file of the historical data of the wanted coin
def make_csv(symbol, folder, candles):
    """
    This function makes csv file of desired coin with defined properties
    :param folder: folder to save the file
    :param symbol: currency pair that you want to make csv file
    :param candles: historical data of the desired coin
    :return: historical data in csv file
    """

    try:
        os.mkdir(folder)
    except FileExistsError:
        # directory already exists
        pass
    candles.to_csv(f"{folder}/{symbol}_hist.csv")


def get_info_on_stock(folder, ticker):
    """
    This function makes csv file of desired ticker from Yahoo Finance
    :param folder: folder to save the file
    :param ticker: coin name that you want to make csv file
    :return: historical data in csv file
    """

    try:
        os.mkdir(folder)
    except FileExistsError:
        # directory already exists
        pass
    stock = yf.Ticker(ticker)

    ticker_info = stock.info
    with open(f"{folder}/{ticker}_info.txt", 'w') as file:
        file.write(json.dumps(ticker_info))

    hist = stock.history(period='max')
    hist.to_csv(f"{folder}/{ticker}_hist.csv")

    major_stake_holders = stock.major_holders
    major_stake_holders.to_csv(f"{folder}/{ticker}_major_stake_holders.csv")
    institutional_holders = stock.institutional_holders
    institutional_holders.to_csv(f"{folder}/{ticker}_institutional_holders.csv")


btc_data = yf.download("BTC-USD", start="2010-01-01", end="2023-06-09")
make_csv("BTCUSD", "binance", btc_data)
get_info_on_stock("BTC", "BTC-USD")
get_info_on_stock("GOLD", "GC=F")
get_info_on_stock("WTI", "CL=F")
get_info_on_stock("BRENT", "BZ=F")
get_info_on_stock("NASDAQ", "^IXIC")
get_info_on_stock("SP500", "^GSPC")
