import sys
sys.path.append('../..')
import datetime as dt
import pandas as pd
import json
import requests
def get_all_coin():
    return ['BTCUSDT']

def get_binance_bars(symbol, interval, startTime, endTime):
    url = "https://api.binance.com/api/v3/klines"
    startTime = str(int(startTime.timestamp() * 1000))
    endTime = str(int(endTime.timestamp() * 1000))
    limit = '1500'
    req_params = {"symbol": symbol, 'interval': interval, 'startTime': startTime, 'endTime': endTime, 'limit': limit}
    df = pd.DataFrame(json.loads(requests.get(url, params=req_params).text))

    if (len(df.index) == 0):
        return None

    df = df.iloc[:, 0:6]
    df.columns = ['datetime', 'open', 'high', 'low', 'close', 'volume']
    df.open = df.open.astype("float")
    df.high = df.high.astype("float")
    df.low = df.low.astype("float")
    df.close = df.close.astype("float")
    df.volume = df.volume.astype("float")
    df['adj_close'] = df['close']
    df.index = [dt.datetime.fromtimestamp(x / 1000.0)+dt.timedelta(hours=8) for x in df.datetime]
    return df
def get_all_coin_history_kline_data():
    all_coin=get_all_coin()
    s_datetime = dt.datetime(2022, 3, 1, 0, 0)
    N=((dt.datetime.now() - s_datetime).days)
    data=pd.DataFrame()
    for coin in all_coin:
        for i in range(N):
            startTime=s_datetime+dt.timedelta(days=i)
            endTime=s_datetime+dt.timedelta(days=i+1)
            df = get_binance_bars(symbol=coin,interval='1m',startTime=startTime,endTime=endTime)
            data=data.append(df)
    data.to_csv('BTC.csv')
get_all_coin_history_kline_data()