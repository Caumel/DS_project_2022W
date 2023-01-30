import pandas as pd
import scipy.io
import os
import mne
import numpy as np
from tqdm import tqdm
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import tensorflow as tf
import keras
from datetime import datetime
import tqdm

from sklearn.preprocessing import LabelEncoder, MinMaxScaler
from sklearn.metrics import accuracy_score, precision_score, recall_score
from sklearn.model_selection import train_test_split
from tensorflow.keras import layers, losses
from tensorflow.keras.datasets import fashion_mnist
from tensorflow.keras.models import Model, Sequential
from tensorflow.keras.layers import LSTM, Dense, Dropout, Masking, Embedding
from keras.callbacks import EarlyStopping, ModelCheckpoint

tf.random.set_seed(7)

# Create file of areas

file_areas = "../Join/telemetriedaten/TelemetrieFiwigatter/TelemetrieFiwigatter_Tel.Fiwi.csv"

df = pd.read_csv(file_areas)
df["start"] = pd.to_datetime(df["start"], infer_datetime_format=True)   
df["end"] = pd.to_datetime(df["end"], infer_datetime_format=True)   

# , "ID" "location","start","end","duration"
# 0,"DE.2011.23","Fiwi",2017-02-01 15:55:08,2017-02-01 15:58:13,3.08333333333333

df_2016 = df[(df["start"] > datetime.strptime("2016-01-01", '%Y-%m-%d')) & (df["start"] < datetime.strptime("2016-12-31", '%Y-%m-%d'))]
df_2017 = df[(df["start"] > datetime.strptime("2017-01-01", '%Y-%m-%d')) & (df["start"] < datetime.strptime("2017-12-31", '%Y-%m-%d'))]
df_2018 = df[(df["start"] > datetime.strptime("2018-01-01", '%Y-%m-%d')) & (df["start"] < datetime.strptime("2018-12-31", '%Y-%m-%d'))]
df_2019 = df[(df["start"] > datetime.strptime("2019-01-01", '%Y-%m-%d')) & (df["start"] < datetime.strptime("2019-12-31", '%Y-%m-%d'))]


df_final = pd.DataFrame(columns=["ID","Date","x-value","y-value","location"])

# for index_data,df in [df_2016,df_2017,df_2018,df_2019]:
for index, row in tqdm.tqdm(df.iloc[78100:,:].iterrows(), total=df.shape[0]):
    id = row["ID"]
    init = row["start"]
    end = row["end"]
    location = row["location"]
    list_dayofcount = [value.strftime("%Y-%m-%d") for value in pd.date_range(start=init,end=end).to_list()]
    for day in list_dayofcount:
        try:
            df_day = pd.read_csv(os.path.join("../Join/telemetriedaten/position_per_wild_fiwi/" + id + "/",day + "_fiwi.csv"))
            df_day["Date"] = pd.to_datetime(df_day["Date"], infer_datetime_format=True)   
            df_to_save = df_day[(df_day["Date"] > init) & (df_day["Date"] < end)][["ID","Date","x-value", "y-value"]]
            df_to_save['location'] = location
            df_final = pd.concat([df_final,df_to_save.iloc[:,:]])
        except Exception as e:
            pass
df_final.to_csv(f"../Join/telemetriedaten/areas.csv")
    # df_final.to_csv(f"../Join/telemetriedaten/areas_{index_data+16}.csv")