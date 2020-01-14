# -*- coding: utf-8 -*-
"""
Created on Thu Dec  5 04:07:19 2019

@author: Nada
"""
pip install tensorflow

import tensorflow
import numpy as np
from keras.preprocessing.image import ImageDataGenerator
from keras.models import Sequential, Model
from keras.layers import Dropout, Flatten, Dense
from keras import applications
from keras.layers import Activation, Dropout, Flatten, Dense, GlobalAveragePooling2D
from keras.preprocessing.image import ImageDataGenerator

# dimensions of our images.
img_width, img_height = 64, 64

train_data_dir = 'train'
validation_data_dir = 'test'
nb_train_samples = 8000
nb_validation_samples = 2000
epochs = 1
batch_size = 32

base_model = applications.VGG16(include_top=False, weights='imagenet', input_shape=(64, 64, 3))
model = Flatten(input_shape=base_model.output_shape[1:])(base_model.output)
model = Dense(128, activation='relu')(model)
model = Dense(1, activation='sigmoid')(model)
model = Model(inputs=base_model.input, outputs=model)

train_datagen = ImageDataGenerator(rescale=1. / 255,
                                   shear_range=0.2,
                                   zoom_range=0.2,
                                   horizontal_flip=False)

test_datagen = ImageDataGenerator(rescale=1. / 255)

training_set = train_datagen.flow_from_directory('dataset/training_set',
                                                 target_size=(64, 64),
                                                 batch_size=32,
                                                 class_mode='binary')

test_set = test_datagen.flow_from_directory('dataset/test_set',
                                            target_size=(64, 64),
                                            batch_size=32,
                                            class_mode='binary')

from keras import callbacks
from keras import metrics

metrics = ['accuracy', 'val_acc']
es = callbacks.EarlyStopping(monitor='val_loss', patience=6)
mc = callbacks.ModelCheckpoint('best_model-1.h5', monitor='val_loss')
callback_list = [es, mc]
model.compile(loss='binary_crossentropy',
              optimizer='adam',
              metrics=['accuracy'])

model.fit_generator(training_set,
                    steps_per_epoch=8000,
                    epochs=2,
                    validation_data=test_set,
                    validation_steps=2000,
                    verbose=1,
                    callbacks=callback_list)
