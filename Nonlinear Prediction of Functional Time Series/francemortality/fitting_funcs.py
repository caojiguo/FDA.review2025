import numpy as np
from tensorflow import keras
from tensorflow.keras import optimizers
from tensorflow.keras.utils import plot_model
from keras.layers import LSTM, Dense, RepeatVector, TimeDistributed, Bidirectional
import tensorflow as tf
from keras import backend as K
from tensorflow.python.keras.engine import training
from tensorflow.python.keras.layers.core import Dropout
import random 
import math 


class fun_encoder(tf.keras.Model):
    def __init__(self, latentdim):
        super(fun_encoder, self).__init__()
        self.latentdim = latentdim
        #self.lstm_b = LSTM(64, return_sequences=True)
        self.lstm_c = LSTM(64, return_sequences=True)
        self.lstm_a = LSTM(latentdim, return_sequences=False)
    def call(self, input):
        
        #current_z = self.lstm_c(input[0])
        #current_z = self.lstm_b(current_z)
        current_z = self.lstm_a(input[0])

        #next_z    = self.lstm_c(input[1])
        #next_z    = self.lstm_b(next_z)
        next_z    = self.lstm_a(input[1]) 
        return current_z, next_z 

class fun_decoder(tf.keras.Model):
    def __init__(self, outputshape):
        super(fun_decoder, self).__init__()
        self.timepts   = outputshape[0]
        self.outputdim = outputshape[1]
        self.repeat     = RepeatVector(self.timepts)
        #self.lstm_2     = LSTM(64, return_sequences=True)
        self.lstm_1     = LSTM(64, return_sequences=True,input_shape = (self.timepts, self.outputdim))
        self.dense_output = Dense(self.outputdim) 
    def call(self, input):
        x = RepeatVector(self.timepts)(input)
        x = self.lstm_1(x)
        #x = self.lstm_2(x)
        x = TimeDistributed(self.dense_output)(x)

        return x 

class ae_mod(tf.keras.Model):
    def __init__(self, latentdim, outputshape, *args, **kwargs):
        super(ae_mod, self).__init__(*args, **kwargs)
        self.fun_encoder = fun_encoder(latentdim)
        self.fun_decoder = fun_decoder(outputshape)
        self.autoregressor = autoregressor(latentdim)

    def call(self, inputs):
        current_z, next_z = self.fun_encoder(inputs)
        autoreg_z = self.autoregressor(current_z)

        current_recon = self.fun_decoder(current_z)
        next_recon    = self.fun_decoder(autoreg_z)

        
        return  current_recon, next_recon, current_z, next_z, autoreg_z 

class autoregressor(tf.keras.Model):
    def __init__(self, latentdim):
        super(autoregressor, self).__init__()
        self.dense1 = Dense(512,activation='sigmoid')
        self.dropout = Dropout(0.3)
        self.dense2 = Dense(256,activation='sigmoid')
        self.dense3 = Dense(128,activation='sigmoid')
        self.dense4 = Dense(64,activation='relu')
        self.outputdense = Dense(latentdim)

    def call(self, inputs):
        #x = self.dense1(inputs)
        #x = self.dropout(x)
        #x = self.dense2(x)
        #x = self.dropout(x)
        #x = self.dense3(x) 
        #x = self.dropout(x)
        #x = self.dense4(x) #star 1, added after
        x = self.outputdense(inputs)

        return x 

class autoregressor_weekday(tf.keras.Model):
    def __init__(self, latentdim):
        super(autoregressor_weekday, self).__init__()
        self.dense1 = Dense(512)
        self.dropout = Dropout(0.3)
        self.dense2 = Dense(256)
        self.dense3 = Dense(128)
        self.dense4 = Dense(64)
        self.outputdense = Dense(latentdim)

    def call(self, inputs):
        #x = self.dense1(inputs)
        #x = self.dense2(x)
        #x = self.dropout(x)
        #x = self.dense3(x) 
        #x = self.dropout(x)
        #x = self.dense4(x) #star 1, added after
        x = self.outputdense(inputs)

        return x 


class autoregressor_weekend(tf.keras.Model):
    def __init__(self, latentdim):
        super(autoregressor_weekend, self).__init__()
        self.dense11 = Dense(512)
        self.dropout = Dropout(0.3)
        self.dense22 = Dense(256)
        self.dense33 = Dense(128)
        self.dense44 = Dense(64)
        self.outputdense_end = Dense(latentdim)

    def call(self, inputs):
        #x = self.dense11(inputs)
        #x = self.dropout(x)
        #x = self.dense22(x)
        #x = self.dropout(x)
        #x = self.dense33(x)
        #x = self.dropout(x)
        #x = self.dense44(x) #star 1, added after
        x = self.outputdense_end(inputs)

        return x


class autoregressor_gamma(tf.keras.Model):
    def __init__(self, latentdim):
        super(autoregressor_gamma, self).__init__()
        self.outputdense = Dense(latentdim)

    def call(self, inputs):
        x = self.outputdense (inputs)

        return x

class ae_mod_diff(tf.keras.Model):
    def __init__(self, latentdim, outputshape, *args, **kwargs):
        super(ae_mod_diff, self).__init__(*args, **kwargs)
        self.fun_encoder = fun_encoder(latentdim)
        self.fun_decoder = fun_decoder(outputshape)
        self.autoregressor_weekday = autoregressor_weekday(latentdim)
        self.autoregressor_weekend = autoregressor_weekend(latentdim)
        self.autoregressor_gamma   = autoregressor_gamma(latentdim)

    def call(self, inputs):
        current_z, next_z = self.fun_encoder(inputs[0:2])
        autoreg_z = tf.multiply(tf.expand_dims(inputs[2],1) ,self.autoregressor_weekday(current_z)) + tf.multiply((1-tf.expand_dims(inputs[2],1)), self.autoregressor_weekend(current_z))
        #autoreg_z = self.autoregressor_gamma(keras.layers.Concatenate()([current_z,tf.expand_dims(inputs[2],1)]))


        current_recon = self.fun_decoder(current_z)
        next_recon    = self.fun_decoder(autoreg_z)
        
        return  current_recon, next_recon, current_z, next_z, autoreg_z 



class ae_mod_full(tf.keras.Model):
    def __init__(self, latentdim, outputshape, *args, **kwargs):
        super(ae_mod_full, self).__init__(*args, **kwargs)
        self.fun_encoder = fun_encoder(latentdim)
        self.fun_decoder = fun_decoder(outputshape)
        self.autoregressor = autoregressor(latentdim)
        
    def call(self, inputs):
        current_z, next_z = self.fun_encoder(inputs[0:2])
        #zforauto = keras.layers.Concatenate()([current_z,tf.one_hot(inputs[2],2),tf.one_hot(inputs[3],4)])
        zforauto = keras.layers.Concatenate()([current_z,keras.layers.Embedding(2,8)(inputs[2]),keras.layers.Embedding(4,8)(inputs[3])])
        autoreg_z = self.autoregressor(zforauto)

        current_recon = self.fun_decoder(current_z)
        next_recon    = self.fun_decoder(autoreg_z)

        
        return  current_recon, next_recon, current_z, next_z, autoreg_z 




def mse_loss(obs, pred):
    return tf.reduce_mean(K.mean(K.square(obs - pred), axis=-1))

def loss(model, x, y, weightingvec ):
    y_ = model(x)
    recon_loss = mse_loss(y[0],y_[0])
    next_recon_loss = mse_loss(y[1],y_[1])
    autoreg_penalty = mse_loss(y_[3], y_[4])
    return weightingvec[0] * recon_loss + weightingvec[1] * next_recon_loss  + weightingvec[2] * autoreg_penalty

def grad(model, inputs, targets, weightingvec):
    with tf.GradientTape() as tape:
        loss_value = loss(model, inputs, targets,weightingvec=weightingvec)
    return loss_value, tape.gradient(loss_value, model.trainable_variables)


def fitting_wrap(model, train_x, train_y, weightingvec, test_x=[], test_y=[],numofepochs = 30,batchsize=32,optimizer=tf.keras.optimizers.Adam()):
    training_loss = np.zeros((numofepochs,))
    trainingsize = train_x[0].shape[0]
    for epoch in range(numofepochs):
        batchindex = np.array_split(np.array(random.sample(range(0, trainingsize), trainingsize)),math.floor(trainingsize/batchsize))
        if epoch ==0:
            print(weightingvec)
        for step in range(len(batchindex)):
            batchset = [train_x[0][batchindex[step],:,:],train_x[1][batchindex[step],:,:] ,train_x[2][batchindex[step]]]    
            loss_val, grads = grad(model, batchset, batchset, weightingvec)
            optimizer.apply_gradients(zip(grads, model.trainable_variables))
        
        print('\nEnd of epoch %d' % (epoch,))
        print('Training loss is %f' % loss_val)
    training_loss[epoch] = loss_val

    return model, training_loss


def fitting_wrap_noday(model, train_x, train_y, weightingvec, test_x=[], test_y=[],numofepochs = 30,batchsize=32,optimizer=tf.keras.optimizers.Adam()):
    training_loss = np.zeros((numofepochs,))
    trainingsize = train_x[0].shape[0]
    for epoch in range(numofepochs):
        batchindex = np.array_split(np.array(random.sample(range(0, trainingsize), trainingsize)),math.floor(trainingsize/batchsize))
        if epoch ==0:
            print(weightingvec)
        for step in range(len(batchindex)):
            batchset = [train_x[0][batchindex[step],:,:],train_x[1][batchindex[step],:,:] ]    
            loss_val, grads = grad(model, batchset, batchset, weightingvec)
            optimizer.apply_gradients(zip(grads, model.trainable_variables))
        
        print('\nEnd of epoch %d' % (epoch,))
        print('Training loss is %f' % loss_val)
    training_loss[epoch] = loss_val

    return model, training_loss