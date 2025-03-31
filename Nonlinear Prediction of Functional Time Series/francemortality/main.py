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

mydf = pd.read_csv('francemortality.csv')


ageinterval = 100
inputdim = 1 
batchsize = 32 
latentdim = 4


totaldf = mydf.to_numpy().astype('float32')
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
cm = plt.cm.tab10(range(0,7))
fig, ax = plt.subplots(figsize = (11,6))
ax.plot(totaldf[0,:],linewidth=2,alpha = 0.6)
for jj in range(1,totaldf.shape[0]):
    ax.plot(totaldf[jj,:],linewidth=1.5,alpha = 0.6,color='grey')
ax.plot(totaldf[0,:],linewidth=2,color= cm[5],label='1916')
ax.plot(totaldf[totaldf.shape[0]-80,:],linewidth=2,color=cm[4],label='1936')
ax.plot(totaldf[totaldf.shape[0]-60,:],linewidth=2,color=cm[3],label='1956')    
ax.plot(totaldf[totaldf.shape[0]-40,:],linewidth=2,color=cm[2],label='1976')
ax.plot(totaldf[totaldf.shape[0]-20,:],linewidth=2,color=cm[1],label='1996')
ax.plot(totaldf[totaldf.shape[0]-1,:], linewidth=2,color=cm[0],label='2016')
ax.set_xticks(range(0,101,20))
ax.set_xlim(0,100)
ax.set_xlabel('Age',fontsize=20)
ax.set_ylabel('Mortality Rate',fontsize=20)
ax.tick_params(axis='both',which='major',labelsize=18)
ax.legend(frameon=False, prop={'size': 20})
fig.savefig('mortality.pdf',bbox_inches='tight')


totaldf = np.log(mydf.to_numpy().astype('float32'))
#data visualization 
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
cm = plt.cm.tab10(range(0,7))
fig, ax = plt.subplots(figsize = (11,6))
ax.plot(totaldf[0,:],linewidth=2,alpha = 0.6)
for jj in range(1,totaldf.shape[0]):
    ax.plot(totaldf[jj,:],linewidth=1.5,alpha = 0.6,color='grey')
ax.plot(totaldf[0,:],linewidth=2,color= cm[5],label='1916')
ax.plot(totaldf[totaldf.shape[0]-80,:],linewidth=2,color=cm[4],label='1936')
ax.plot(totaldf[totaldf.shape[0]-60,:],linewidth=2,color=cm[3],label='1956')    
ax.plot(totaldf[totaldf.shape[0]-40,:],linewidth=2,color=cm[2],label='1976')
ax.plot(totaldf[totaldf.shape[0]-20,:],linewidth=2,color=cm[1],label='1996')
ax.plot(totaldf[totaldf.shape[0]-1,:], linewidth=2,color=cm[0],label='2016')
ax.set_xticks(range(0,101,20))
ax.set_xlim(0,100)
ax.set_xlabel('Age',fontsize=20)
ax.set_ylabel('Log Mortality Rate',fontsize=20)
ax.tick_params(axis='both',which='major',labelsize=18)
ax.legend(frameon=False, prop={'size': 20})
plt.show()
fig.savefig('logmortality.pdf',bbox_inches='tight')
#plt.show()
# 1816-2016





totaldf = mydf.to_numpy().astype('float32')
totaldf = np.expand_dims(totaldf,2)
numofobs = totaldf.shape[0]

testlength = 20

trainset = [totaldf[0:(numofobs-testlength-1),:], totaldf[1:(numofobs - testlength),:]]
testset  = totaldf[-testlength:,:,:]


mymod = ae_mod(latentdim, [ageinterval, inputdim])
optimizer = tf.keras.optimizers.Adam()


#mymod, trainingloss = fitting_wrap_noday(model = mymod, train_x = trainset, train_y = trainset,
#                                       weightingvec= [1,1,1e-5],optimizer=optimizer,numofepochs=100)


mymod.load_weights('./fitting/mort')

lastobs = np.expand_dims(trainset[1][trainset[1].shape[0]-1,:],0)
predlist = []
predlist.append(lastobs)
for jj in range(testlength):
    predlist.append(mymod([predlist[jj],predlist[jj]])[1])
del predlist[0]

ftsa_pred_8 = pd.read_csv('linear_ftsapred.csv',delimiter=',').to_numpy()
plsr_pred   = pd.read_csv('linear_plsr.csv',delimiter=',').to_numpy()
dfpca_pred  = pd.read_csv('linear_dfpca.csv',delimiter=',').to_numpy()



plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
cm = plt.cm.tab10(range(0,7))
fig, ax = plt.subplots(figsize = (11,6))
ax.plot(predlist[0].numpy().reshape(-1),alpha=0.6,linewidth=1.5,color='grey')
for jj in range(1,20):
    ax.plot(predlist[jj].numpy().reshape(-1),alpha=0.6,linewidth=1.5,color='grey')
ax.plot(predlist[0].numpy().reshape(-1),linewidth=2,color=cm[2],label='1996')
ax.plot(predlist[1].numpy().reshape(-1),linewidth=2,color=cm[1],label='2006')     
ax.plot(predlist[19].numpy().reshape(-1),linewidth=2,color=cm[0],label='2016')
ax.set_xticks(range(0,101,20))
ax.set_xlim(0,100)
ax.set_xlabel('Age',fontsize=20)
ax.set_ylabel('Mortality Rate',fontsize=20)
ax.tick_params(axis='both',which='major',labelsize=18)
ax.legend(frameon=False, prop={'size': 20},loc='upper left')
fig.savefig('raw_preddisplay.pdf',bbox_inches='tight')


plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
cm = plt.cm.tab10(range(0,7))
fig, ax = plt.subplots(figsize = (11,6))
ax.plot(np.log(predlist[0].numpy().reshape(-1)),alpha=0.6,linewidth=1.5,color='grey')
for jj in range(1,20):
    ax.plot(np.log(predlist[jj].numpy().reshape(-1)),alpha=0.6,linewidth=1.5,color='grey')
ax.plot(np.log(predlist[0].numpy().reshape(-1)),linewidth=2,color=cm[2],label='1996')
ax.plot(np.log(predlist[1].numpy().reshape(-1)),linewidth=2,color=cm[1],label='2006')     
ax.plot(np.log(predlist[19].numpy().reshape(-1)),linewidth=2,color=cm[0],label='2016')
ax.set_xticks(range(0,101,20))
ax.set_xlim(0,100)
ax.set_xlabel('Age',fontsize=20)
ax.set_ylabel('Log Mortality Rate',fontsize=20)
ax.tick_params(axis='both',which='major',labelsize=18)
ax.legend(frameon=False, prop={'size': 20},loc='lower right')
fig.savefig('preddisplay.pdf',bbox_inches='tight')




plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
plt.rc('font', size=20)
properror     = np.squeeze(np.squeeze(np.array(predlist),3),1) - np.squeeze(testset,2)
funseries_8   = ftsa_pred_8 -np.squeeze(testset,2) 
plsr_error    = plsr_pred -np.squeeze(testset,2) 
dfpca_error   = dfpca_pred - np.squeeze(testset,2) 
fig, ax = plt.subplots(figsize = (20,10))
ax.plot(range(1,21),np.mean(properror**2,1),label='NOP',linewidth=3)
ax.plot(range(1,21),np.mean(funseries_8**2,1),label='FTSA',linewidth=3)
ax.plot(range(1,21),np.mean(plsr_error**2,1), label='FPLSR',linewidth=3)
ax.plot(range(1,21),np.mean(dfpca_error**2,1), label='DFPCA',linewidth=3)
ax.set_xticks([1,5,10,15,20])
ax.ticklabel_format(style='sci', axis='y', scilimits=(0,0))
ax.yaxis.get_offset_text().set_fontsize(20)
ax.set_xlabel('Year +',fontsize = 25,)
ax.set_ylabel('MSPE',fontsize = 25,)
ax.legend(frameon=False,prop={'size': 25})
ax.tick_params(axis='both',which='major',labelsize=20)
plt.show()
fig.savefig('mortalitymspe.pdf',bbox_inches='tight')

