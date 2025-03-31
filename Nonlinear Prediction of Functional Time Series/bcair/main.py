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


mydf = np.load('airquality.npy')
mydf = mydf[:,:,[0,1,3]]
mean_dim = np.mean(mydf,axis = (0,1),keepdims=True) 
sd_dim   = np.std(mydf,axis = (0,1),keepdims=True) 
totaldf  = (mydf - mean_dim)/sd_dim


'''
gor = np.zeros((totaldf.shape[0],72))
for jj in range(totaldf.shape[0]):
    gor[jj,:] = np.concatenate((totaldf[jj,:,0], totaldf[jj,:,1], totaldf[jj,:,2]))
np.savetxt('AQdata.txt',gor)
'''

plt.style.use('default')
cm = plt.cm.tab10(range(0,7))
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
fig ,ax = plt.subplots(1,3,figsize = (15,7))
ax[0].plot(mydf[0,:,0],color='grey',alpha = 0.6,linewidth=2)
for jj in range(1,100):
    ax[0].plot(mydf[jj,:,0],color='grey',alpha = 0.6,linewidth=2)
ax[0].plot(mydf[51,:,0],linewidth=3,label='Day1',color=cm[0])
ax[0].plot(mydf[52,:,0],linewidth=3,label='Day2',color=cm[1])
ax[0].plot(mydf[53,:,0],linewidth=3,label='Day3',color=cm[2])
ax[0].plot(mydf[54,:,0],linewidth=3,label='Day4',color=cm[3])
ax[0].plot(mydf[55,:,0],linewidth=3,label='Day5',color=cm[4])
#ax[0].plot(mydf[56,:,0],linewidth=3,label='Day6',color=cm[5])
ax[0].set_xticks(np.arange(0,25,4))
ax[0].set_xlim([0,24])
ax[0].tick_params(axis='both',which='major',labelsize=12)
ax[0].set_xlabel('Hour',fontsize=14)
ax[0].set_title('NO2',fontsize =14,weight='bold')
ax[0].set_ylabel('PPB',fontsize=14)

ax[1].plot(mydf[0,:,1],color='grey',alpha = 0.6,linewidth=2)
for jj in range(1,100):
    ax[1].plot(mydf[jj,:,1],color='grey',alpha = 0.6,linewidth=2)
ax[1].plot(mydf[51,:,1],linewidth=3,label='Day1',color=cm[0])
ax[1].plot(mydf[52,:,1],linewidth=3,label='Day2',color=cm[1])
ax[1].plot(mydf[53,:,1],linewidth=3,label='Day3',color=cm[2])
ax[1].plot(mydf[54,:,1],linewidth=3,label='Day4',color=cm[3])
ax[1].plot(mydf[55,:,1],linewidth=3,label='Day5',color=cm[4])
#ax[1].plot(mydf[56,:,1],linewidth=3,color=cm[5])
ax[1].set_xticks(np.arange(0,25,4))
ax[1].set_xlim([0,24])
ax[1].tick_params(axis='both',which='major',labelsize=12)
ax[1].set_xlabel('Hour',fontsize=14)
ax[1].set_title('O3',fontsize =14,weight='bold')

ax[2].plot(mydf[0,:,2],color='grey',alpha = 0.6,linewidth=2)
for jj in range(1,100):
    ax[2].plot(mydf[jj,:,2],color='grey',alpha = 0.6,linewidth=2)
ax[2].plot(mydf[51,:,2],linewidth=3,label='Day1',color=cm[0])
ax[2].plot(mydf[52,:,2],linewidth=3,label='Day2',color=cm[1])
ax[2].plot(mydf[53,:,2],linewidth=3,label='Day3',color=cm[2])
ax[2].plot(mydf[54,:,2],linewidth=3,label='Day4',color=cm[3])
ax[2].plot(mydf[55,:,2],linewidth=3,label='Day5',color=cm[4])
#ax[2].plot(mydf[56,:,2],linewidth=3,color=cm[5])
ax[2].set_xticks(np.arange(0,25,4))
ax[2].set_xlim([0,24])
ax[2].set_xlabel('Hour',fontsize=14)
ax[2].set_title('SO3',fontsize =14,weight='bold')
handles, labels = plt.gca().get_legend_handles_labels()
by_label = dict(zip(labels, handles))
ax[2].legend(by_label.values(), by_label.keys(),frameon=False)
ax[2].tick_params(axis='both',which='major',labelsize=12)
fig.savefig('multidata_display.pdf',bbox_inches='tight')




numofhours = 24
inputdim = 3
batchsize = 2192 
latentdim = 16

numofobs = totaldf.shape[0]

testlength =  50

trainset = [totaldf[0:(numofobs-testlength-1),:], totaldf[1:(numofobs - testlength),:]]
testset  = totaldf[-testlength:,:,:]

mymod = ae_mod(latentdim, [numofhours, inputdim])
optimizer = tf.keras.optimizers.Adam()

mymod, trainingloss = fitting_wrap_noday(model = mymod, train_x = trainset, train_y = trainset,
                                       weightingvec= [1,1,1e-5],optimizer=optimizer,numofepochs=50)

#mymod.load_weights('./fitting/fit')

predtest = mymod([testset,testset])[0].numpy()
plt.plot(predtest[0,:,0],label='proposed')
plt.plot(testset[0,:,0],label='true')
plt.legend()
plt.show()



plt.style.use('default')
cm = plt.cm.tab10(range(0,7))
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
pred_multi_plsr_temp      = pd.read_csv('fplsr_multi.csv',delimiter=',').to_numpy()
pred_multi_funseries_temp = pd.read_csv('ftsapred_multi.csv',delimiter=',').to_numpy()
pred_multi_dfpca_temp = pd.read_csv('dfpca_multi.csv',delimiter=',').to_numpy()
pred_multi_plsr      = np.zeros((50,24,3))
pred_multi_funseries = np.zeros((50,24,3))
pred_multi_dfpca     = np.zeros((50,24,3))
for jj in range(50):
    pred_multi_plsr[jj,:,:] =       pred_multi_plsr_temp[jj,:].reshape((3,24)).T
    pred_multi_funseries[jj,:,:] =  pred_multi_funseries_temp[jj,:].reshape((3,24)).T
    pred_multi_dfpca[jj,:,:] =      pred_multi_dfpca_temp[jj,:].reshape((3,24)).T


cm = plt.cm.tab10(range(0,7))
fig, axes = plt.subplots(3,3,figsize = (11,6),sharex=True)
for testindex in range(3):
    for dim in range(3):
        axes[testindex,dim].plot(testset[testindex,:,dim]*sd_dim[0,0,dim]+mean_dim[0,0,dim],label='TRUE',color='black',linewidth=2,alpha = 0.7)
        axes[testindex,dim].plot(predtest[testindex,:,dim]*sd_dim[0,0,dim]+mean_dim[0,0,dim],label='NOP',color=cm[0],linewidth=3,alpha = 0.7)
        axes[testindex,dim].plot(pred_multi_plsr[testindex,:,dim]*sd_dim[0,0,dim]+mean_dim[0,0,dim],label='FPLSR',color=cm[1],linewidth=3,alpha = 0.7)
        axes[testindex,dim].plot(pred_multi_funseries[testindex,:,dim]*sd_dim[0,0,dim]+mean_dim[0,0,dim],label='FTSA',color=cm[2],linewidth=3,alpha = 0.7)
        axes[testindex,dim].plot(pred_multi_dfpca[testindex,:,dim]*sd_dim[0,0,dim]+mean_dim[0,0,dim],label='DFPCA',color=cm[3],linewidth=3,alpha = 0.7)
        
        axes[testindex,dim].set_xlabel('Hour',fontsize = 14); axes[testindex,dim].set_xticks(np.arange(0,25,4))
        if dim==0: axes[testindex,dim].set_ylabel('PPB',fontsize = 14)
fig.subplots_adjust(wspace=0.2, hspace=0)
axes[0,0].set_title('NO2',fontsize = 14,fontweight='bold')
axes[0,1].set_title('O3',fontsize = 14,fontweight='bold')
axes[0,2].set_title('SO3',fontsize = 14,fontweight='bold')
axes[0,2].text(25,0.5,'Day +1')
axes[1,2].text(25,0.5,'Day +2')
axes[2,2].text(25,0.5,'Day +3')
handles, labels = plt.gca().get_legend_handles_labels()
by_label = dict(zip(labels, handles))
axes[0,1].legend(by_label.values(), by_label.keys(), loc='upper center', ncol=len(labels),bbox_to_anchor=(0.5,1.5),frameon=False)
fig.savefig('AQpredcompare.pdf',bbox_inches='tight')




multi_prop_error      = predtest           - testset
multi_plsr_error      = pred_multi_plsr    - testset
multi_funseries_error = pred_multi_funseries - testset
multi_dfpca_error = pred_multi_dfpca - testset


from itertools import chain
import matplotlib.pyplot as plt
import seaborn as sns

lengthnum = 50
cm = plt.cm.tab10(range(0,7))
boxplotdf = pd.DataFrame({'Chemical':list(chain.from_iterable(i if isinstance(i, list) else [i] for i in [['NO2'] *lengthnum,['O3']*lengthnum,['SO3']*lengthnum]))*4 ,
                          'Method':list(chain.from_iterable(i if isinstance(i, list) else [i] for i in [['NOP']*(lengthnum*3),['FTSA'] * (lengthnum*3) , ['FPLSR']*(lengthnum*3) , ['DFPCA']*(lengthnum*3)])),
                          'MSPE': np.hstack([   np.mean(multi_prop_error[:,:,0] **2,1)[0:lengthnum],
                                                np.mean(multi_prop_error[:,:,1] **2,1)[0:lengthnum],
                                                np.mean(multi_prop_error[:,:,2] **2,1)[0:lengthnum],  
                                                np.mean(multi_funseries_error[:,:,0] **2,1)[0:lengthnum],
                                                np.mean(multi_funseries_error[:,:,1] **2,1)[0:lengthnum],
                                                np.mean(multi_funseries_error[:,:,2] **2,1)[0:lengthnum],
                                                np.mean(multi_plsr_error[:,:,0] **2,1)[0:lengthnum],
                                                np.mean(multi_plsr_error[:,:,1] **2,1)[0:lengthnum],
                                                np.mean(multi_plsr_error[:,:,2] **2,1)[0:lengthnum],
                                                np.mean(multi_dfpca_error[:,:,0] **2,1)[0:lengthnum],
                                                np.mean(multi_dfpca_error[:,:,1] **2,1)[0:lengthnum],
                                                np.mean(multi_dfpca_error[:,:,2] **2,1)[0:lengthnum]                           
                                                         ])})

plt.style.use('default')
cm = plt.cm.tab10(range(0,7))
plt.rcParams["font.weight"] = "bold"
plt.rcParams["axes.labelweight"] = "bold"
fig, ax = plt.subplots(figsize=(10,7))                            
my_pal = {"DFPCA":cm[3],"FTSA": cm[2], 'FPLSR':cm[1],'NOP':cm[0]}
sns.boxplot(x='Chemical',y='MSPE',data=boxplotdf,hue='Method',palette=my_pal,linewidth=1.5)
ax.legend(frameon=False)
fig.savefig('multichemicalcompare.pdf',bbox_inches='tight')

