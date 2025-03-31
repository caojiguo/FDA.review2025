import numpy as np
import pandas as pd
from pandas.io.parsers import read_csv 
from tensorflow import keras
from tensorflow.keras import optimizers
from tensorflow.keras.utils import plot_model
from keras.layers import LSTM, Dense, RepeatVector, TimeDistributed, Bidirectional
import tensorflow as tf
from keras import backend as K
from tensorflow.python.keras.engine import training
from fitting_funcs import *
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.decomposition import PCA
from statsmodels.tsa.arima.model import ARIMA
import math
import random 

mydf = pd.read_csv('nordelectricity.csv')
mydf = mydf.pivot(index = 'Dates',columns ='Hour',values='priceahead')

weekday  = []
weekdayname =[]
for jj in range(len(pd.to_datetime(mydf.index))):
    weekday.append(pd.to_datetime(mydf.index)[jj].weekday() + 1)
    weekdayname.append(pd.to_datetime(mydf.index)[jj].day_name())


weekday = np.array(weekday)
weekday[weekday<=5] = 1
weekday[weekday>5] = 0


'''
for ii in range(5,12):
    plt.plot(mydf.to_numpy().astype('float32')[ii,:],label=weekdayname[ii])
plt.legend()
plt.show()
'''


numofhours = 24
inputdim = 1 

batchsize = 32 
latentdim = 64


#normalize instead of standardize
totaldf = mydf.to_numpy().astype('float32')
#totaldf = np.expand_dims(totaldf,2)
weekday = np.delete(weekday, np.argwhere(np.isnan(totaldf))[:,0], 0 )

totaldf = np.delete(totaldf, np.argwhere(np.isnan(totaldf))[:,0], 0 )
totalmean = np.mean(totaldf)
totalsd   = np.std(totaldf)
#plt.plot(totaldf[0,:,:]);plt.show()
totaldf = (totaldf - totalmean)/totalsd
#plt.plot(totaldf[0,:]);plt.show()
totaldf = np.expand_dims(totaldf,2)

numofobs = totaldf.shape[0]



#data visualization 
'''
plt.style.use('fivethirtyeight')
plt.rcParams['axes.facecolor']='white'
plt.rcParams['savefig.facecolor']='white'
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
fig, ax = plt.subplots(figsize=(12,7))
ax.plot(totaldf[0,:],color='grey',alpha = 0.6,linewidth=2)
for jj in range(1,100):
    ax.plot(totaldf[jj,:],color='grey',alpha = 0.6,linewidth=2)
ax.plot(totaldf[0,:],label='Weekday',linewidth=3.5)
ax.plot(totaldf[5,:],label='Weekend',linewidth=3.5)
ax.set_xticks(np.arange(0,25,4))
ax.set_yticks(range(20,120,20))
ax.set_xlim([0,24])
ax.set_xlabel('Hour')
ax.set_ylabel('Price')
ax.set_frame_on(False)
fig.patch.set_alpha(1)
plt.legend(frameon=False)
fig.savefig('norddatadisplay.pdf',bbox_inches='tight')


'''



testlength = 500

trainset = [totaldf[0:(numofobs-testlength-1),:], totaldf[1:(numofobs - testlength),:],np.array(weekday[1:(numofobs - testlength)]).astype('float32')]
traindate = np.array(weekday[1:(numofobs - testlength)])
testset  = totaldf[-testlength:,:,:]
testdate = weekday[-testlength:]


#testmean = totalmean[-testlength:]
#testsd = totalsd[-testlength:]



mymod = ae_mod_diff(latentdim, [numofhours, inputdim])
optimizer = tf.keras.optimizers.Adam()

#mymod, trainingloss = fitting_wrap(model = mymod, train_x = trainset, train_y = trainset,
#                                       weightingvec= [1,1,1e-5],optimizer=optimizer,numofepochs=20)


mymod.load_weights('./fitting/withday')

lastobs = np.expand_dims(trainset[1][trainset[1].shape[0]-1,:],0)
predlist = []
predlist.append(lastobs)

for jj in range(testlength):
    predlist.append(mymod([predlist[jj],predlist[jj],np.array([testdate[jj]]).astype('float32')])[1])
del predlist[0]

len(predlist)
#np.savetxt('withday_pred_500.txt',np.squeeze(np.squeeze(np.array(predlist),3),1) )
#convert list to matrix 
predmat = np.zeros(testset.shape)
for im in range(testlength):
    predmat[im,:,:] = predlist[im][0,:,:]

ftsa_pred_8 = pd.read_csv('ftsapred_8.csv',delimiter=',').to_numpy()
plsr_pred   = pd.read_csv('plsr.csv',delimiter=',').to_numpy().T
dfpca_pred  = pd.read_csv('dfpca.csv',delimiter=',').to_numpy()
pred_noday =  np.loadtxt('noday_pred_500.txt')

supposed = mymod([testset,testset,testdate.astype('float32')])


stepdf = np.vstack([np.expand_dims(trainset[1][trainset[1].shape[0]-1,:],0),testset[0:(testlength-1),:,:]])
stepdate = np.hstack([traindate[-1],testdate[0:(testlength-1)]]).astype('float32')
onesteppred = mymod([stepdf,stepdf,stepdate])[1]


properror     = np.squeeze(np.squeeze(np.array(predlist),3),1) - np.squeeze(testset,2)
properror_onestep = np.squeeze(onesteppred,2) - np.squeeze(testset,2) 
properror_noday   = pred_noday - np.squeeze(testset,2) 
funseries_8   = ftsa_pred_8 -np.squeeze(testset,2) 
plsr_error    = plsr_pred -np.squeeze(testset,2) 
dfpca_error   = dfpca_pred - np.squeeze(testset,2) 


cm = plt.cm.tab10(range(0,7))
plt.style.use('fivethirtyeight')
plt.rcParams['axes.facecolor']='white'
plt.rcParams['savefig.facecolor']='white'
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
fig, ax = plt.subplots(figsize = (12,8))
ax.plot(range(1,21),np.mean(properror**2,1)[0:20],label='NOP_weekday',linewidth=3 , color = cm[0])
ax.plot(range(1,21),np.mean(properror_noday**2,1)[0:20],label='NOP',linewidth=3,color = cm[4])
ax.plot(range(1,21),np.mean(funseries_8**2,1)[0:20],label='FTSA',linewidth=3, color = cm[2])
ax.plot(range(1,21),np.mean(plsr_error**2,1)[0:20], label='FPLSR',linewidth=3,color=cm[1])
ax.plot(range(1,21),np.mean(dfpca_error**2,1)[0:20], label='DFPCA',linewidth=3,color=cm[3])
ax.set_xticks([1,5,10,15,20])
ax.set_yticks(np.arange(0,2.5,0.5))
ax.set_xlabel('Day +')
ax.set_ylabel('MSPE')
ax.set_frame_on(False)
ax.legend(loc=(0.5,0.75),frameon=False)
fig.savefig('nordmspecompare.pdf',bbox_inches='tight')




#Comparison for the first unseen curve 
plt.style.use('fivethirtyeight')
plt.rcParams['axes.facecolor']='white'
plt.rcParams['savefig.facecolor']='white'
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
labels=['TRUE','NOP_weekday','NOP','FTSA','FPLSR']
fig, ax = plt.subplots(1,3,figsize = (15,6),sharey=True)
ax = ax.ravel()
ii = 0
for testindex in range(0,3): 
    ax[ii].plot(testset[testindex,:,:]*totalsd+totalmean,label='true',linewidth=2,color='k')
    ax[ii].plot(predlist[testindex].numpy().reshape(-1)*totalsd+totalmean,label='NOP_weekday',linewidth=2,color=cm[0])
    ax[ii].plot(pred_noday[testindex,:]*totalsd+totalmean,label='NOP',linewidth=2,color=cm[4])
    # ax[testindex].plot(ftsa_pred_1[testindex,:],label='funseries_1')
    ax[ii].plot(ftsa_pred_8[testindex,:]*totalsd+totalmean,label='FTSA',linewidth=2,color=cm[2])
    ax[ii].plot(plsr_pred[testindex,:]*totalsd+totalmean,label='FPLSR',linewidth=2,color=cm[1])
    ax[ii].plot(dfpca_pred[testindex,:]*totalsd+totalmean,label='DFPCA',linewidth=2,color=cm[3])
    ax[ii].set_xlabel('Hour',fontsize = 14); ax[ii].set_xticks(np.arange(0,25,4))
    #ax[testindex].legend()
    ax[ii].text(12,67,f'Day +{testindex+1}', fontsize = 12)
    ax[ii].set_frame_on(False)
    ii += 1
ax[0].set_ylabel('Price',fontsize = 14)
labels=['TRUE','NOP_weekday','NOP','FTSA','FPLSR','DFPCA']
fig.legend(labels, loc='upper center', ncol=len(labels),bbox_to_anchor=(0.5,1.001),frameon=False)
fig.savefig('pred_compare_withday_1.pdf',bbox_inches='tight')


#Comparison for the first unseen curve 
plt.style.use('fivethirtyeight')
plt.rcParams['axes.facecolor']='white'
plt.rcParams['savefig.facecolor']='white'
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"

fig, ax = plt.subplots(1,3,figsize = (15,6),sharey=True)
ax = ax.ravel()
ii = 0
for testindex in range(3,6): 
    ax[ii].plot(testset[testindex,:,:]*totalsd+totalmean,label='true',linewidth=2,color='k')
    ax[ii].plot(predlist[testindex].numpy().reshape(-1)*totalsd+totalmean,label='NOP_weekday',linewidth=2,color=cm[0])
    ax[ii].plot(pred_noday[testindex,:]*totalsd+totalmean,label='NOP',linewidth=2,color=cm[4])
    # ax[testindex].plot(ftsa_pred_1[testindex,:],label='funseries_1')
    ax[ii].plot(ftsa_pred_8[testindex,:]*totalsd+totalmean,label='FTSA',linewidth=2,color=cm[2])
    ax[ii].plot(plsr_pred[testindex,:]*totalsd+totalmean,label='FPLSR',linewidth=2,color=cm[1])
    ax[ii].plot(dfpca_pred[testindex,:]*totalsd+totalmean,label='DFPCA',linewidth=2,color=cm[3])
    ax[ii].set_xlabel('Hour',fontsize = 14); ax[ii].set_xticks(np.arange(0,25,4))
    #ax[testindex].legend()
    ax[ii].text(12,67,f'Day +{testindex+1}', fontsize = 12)
    ax[ii].set_frame_on(False)
    ii += 1
ax[0].set_ylabel('Price',fontsize = 14)
fig.savefig('pred_compare_withday_2.pdf',bbox_inches='tight')
