# %% [markdown]
# RNN podemos usar, pero is complex to train with longer sequences and can suffer from the problem of vanishing gradients
# 
# Hay otra variaciones como LSTM and GRU que no tienen este problema.

# %%
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

from sklearn.preprocessing import LabelEncoder, MinMaxScaler
from sklearn.metrics import accuracy_score, precision_score, recall_score
from sklearn.model_selection import train_test_split
from tensorflow.keras import layers, losses
from tensorflow.keras.datasets import fashion_mnist
from tensorflow.keras.models import Model, Sequential
from tensorflow.keras.layers import LSTM, Dense, Dropout, Masking, Embedding
from keras.callbacks import EarlyStopping, ModelCheckpoint, CSVLogger

tf.random.set_seed(7)

# %% [markdown]
# ### Prepare files

# %%


# %% [markdown]
# ## Data preparation

# %%
# Wild board DE.2011.14
path_file = "../Join/telemetriedaten/position_per_wild_fiwi/DE.2011.14/DE.2011.14.csv"
df = pd.read_csv(path_file)
df = df.set_index('Date')
df = df.drop(columns=["Unnamed: 0","tagid","MACadresse","ID"])
ohe = pd.get_dummies(df.WindDir)
df = df.drop('WindDir',axis = 1)
# df = df.join(ohe)

# %%
# LSTMs are sensitive to the scale of the input data

# normalize the dataset
scaler = MinMaxScaler(feature_range=(0,1))
df[['TempMain', 'HumMain', 'Wind', 'Rain', 'Solar','TempBB', 'TempForest',
    'HumForest', 'BaroPressure', '°C_FG', 'hum_FG', 'dew_p_FG', '°C_VG',
    'hum_VG', 'dew_p_VG', 'SuhlenVG', 'SuhlenFG']] = scaler.fit_transform(df[['TempMain', 'HumMain', 'Wind', 'Rain', 'Solar',
                                                                              'TempBB', 'TempForest', 'HumForest', 'BaroPressure', '°C_FG', 'hum_FG',
                                                                              'dew_p_FG', '°C_VG', 'hum_VG', 'dew_p_VG', 'SuhlenVG', 'SuhlenFG']])
values = df.values
values = values.astype('float32')

# %%
df_temporal = df.copy()#.iloc[0:5,:]
df_temporal[["x-value","y-value"]] = df_temporal[["x-value","y-value"]].shift(-1)
df_temporal = df_temporal.dropna(subset=["x-value","y-value"])

# %%
# Split dataset 

values = df_temporal.values
length = values.shape[0]

train, validation, test = np.split(df_temporal, [int(.8 * len(df_temporal)), int(.9 * len(df_temporal))])


X_train, y_train = train.drop(columns=["x-value","y-value"]).values,  train[["x-value","y-value"]].values
X_validation, y_validation = validation.drop(columns=["x-value","y-value"]).values,  validation[["x-value","y-value"]].values
X_test, y_test = test.drop(columns=["x-value","y-value"]).values,  test[["x-value","y-value"]].values

# %%
X_train = X_train.reshape((X_train.shape[0], 1, X_train.shape[1]))
X_validation = X_validation.reshape((X_validation.shape[0], 1, X_validation.shape[1]))
X_test = X_test.reshape((X_test.shape[0], 1, X_test.shape[1]))

# %%
print(X_train.shape, y_train.shape, X_validation.shape, y_validation.shape, X_test.shape, y_test.shape)

# %% [markdown]
# ## Model and miscelaneous

# %%
def create_LSTM_model(data, data_size):
    model = Sequential()
    # Masking layer for pre-trained embeddings
    model.add(LSTM(units=data_size, input_shape=(data.shape[1], data.shape[2])))
    # Fully connected layer
    model.add(Dense(64, activation='relu'))
    # # Dropout for regularization
    model.add(Dropout(0.5))
    # Output layer
    model.add(Dense(2, activation='softmax'))
    return model

model = create_LSTM_model(X_train,2048)
model.summary()

model.compile(optimizer='adam', loss='binary_crossentropy', metrics=['accuracy'])

# Compile the model
# model.compile(optimizer='adam', loss='categorical_crossentropy', metrics=['accuracy'])

# %% [markdown]
# ## Train model

# %%
# Create callbacks
callbacks = [
                EarlyStopping(monitor='val_loss', patience=5),
                ModelCheckpoint('./models/model.h5', save_best_only=True, save_weights_only=False),
                CSVLogger('./training.log')
            ]

# %%
print("Training")
history = model.fit(X_train,  y_train, 
                    batch_size=2048, epochs=10,
                    callbacks=callbacks,
                    validation_data=(X_validation, y_validation))

csv_logger = CSVLogger('training.log')
model.save('./models/model.h5')

# %% [markdown]
# ## Results

# %% [markdown]
# 


