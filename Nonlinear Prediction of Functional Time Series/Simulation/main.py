import numpy as np 
from fitting_funcs import *
import matplotlib.pyplot as plt
import pandas as pd
import timeit


serieslength = [100,500,1000,2000]

proprrunningtime = np.zeros((100,4))


lengthindex = 0 
for ii in serieslength:
    print(ii)
    for jj in range(100):
        print(jj)
        mydf = np.expand_dims(np.loadtxt('./lineardata/simdata_linear_'+str(ii)+'_sim'+str(jj+1)+'.txt'),2)
        
        nxps = 51
        inputdim = 1
        batchsize = 32
        latentdim = 16

        numofobs = mydf.shape[0]

        testlength = 20 


        trainset = [mydf[0:(numofobs-testlength-1),:], mydf[1:(numofobs - testlength),:]]
        testset  = mydf[-testlength:,:,:]


        mymod = ae_mod(latentdim, [nxps, inputdim])
        optimizer = tf.keras.optimizers.Adam()

        start = timeit.default_timer()
        mymod, trainingloss = fitting_wrap_noday(model = mymod, train_x = trainset, train_y = trainset,
                                            weightingvec= [1,1,1e-7],optimizer=optimizer,numofepochs=50)
        stop = timeit.default_timer()
        proprrunningtime[jj,lengthindex] = stop - start 


        stepdf = np.vstack([np.expand_dims(trainset[1][trainset[1].shape[0]-1,:],0),testset[0:(testlength-1),:,:]])
        onesteppred = np.squeeze(mymod([stepdf,stepdf])[1],2)

        np.savetxt('./linearsimresults/prop_simresults_linear_' + str(ii) + '_sim'+str(jj+1)+'.txt' ,onesteppred)
    lengthindex += 1 


np.savetxt('proprunningtime.txt',proprrunningtime)

'''



mydf = np.expand_dims(np.loadtxt('simdata_linear_100.txt'),2)


nxps = 51
inputdim = 1
batchsize = 32
latentdim = 16

numofobs = mydf.shape[0]

testlength = 20 


trainset = [mydf[0:(numofobs-testlength-1),:], mydf[1:(numofobs - testlength),:]]
testset  = mydf[-testlength:,:,:]


mymod = ae_mod(latentdim, [nxps, inputdim])
optimizer = tf.keras.optimizers.Adam()


mymod, trainingloss = fitting_wrap_noday(model = mymod, train_x = trainset, train_y = trainset,
                                       weightingvec= [1,1,1e-3],optimizer=optimizer,numofepochs=100)


reconlist = mymod(trainset)[0].numpy()
plt.plot(reconlist[10,:,:],label='proposed')
plt.plot(trainset[0][10,:,:],label='true')
plt.show()

stepdf = np.vstack([np.expand_dims(trainset[1][trainset[1].shape[0]-1,:],0),testset[0:(testlength-1),:,:]])
onesteppred = mymod([stepdf,stepdf])[1]


ftsa_pred = pd.read_csv('linear_ftsapred.csv',delimiter=',').to_numpy().T
fplsr_pred   = pd.read_csv('linear_plsr.csv',delimiter=',').to_numpy().T

testindex = 0
fig, ax = plt.subplots(figsize = (10,10))
ax.plot(testset[testindex,:,:].reshape(-1),label='true',color='k',linewidth=3)
ax.plot(onesteppred[testindex,:,:],label='proposed onestep',linewidth=3)
ax.plot(ftsa_pred[testindex,:],label='funseries_8',linewidth=3)
ax.plot(fplsr_pred[testindex,:],label='funseries_plsr',linewidth=3)
ax.legend()
plt.show()

properror_onestep = np.squeeze(onesteppred,2) - np.squeeze(testset,2) 
funseries_8   = ftsa_pred -np.squeeze(testset,2) 
plsr_error    = fplsr_pred -np.squeeze(testset,2) 
fig, ax = plt.subplots(figsize = (10,10))
ax.plot(range(1,21),np.mean(properror_onestep**2,1)[0:20],label='onestep',linewidth=3)
ax.plot(range(1,21),np.mean(funseries_8**2,1)[0:20],label='funseries_8')
ax.plot(range(1,21),np.mean(plsr_error**2,1)[0:20], label='plsr',linewidth=3)
ax.set_xticks(range(1,21))
ax.set_xlabel('Day +')
ax.set_ylabel('MSPE')
ax.legend()
plt.show()

'''