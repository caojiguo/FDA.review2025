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
        mydf = np.expand_dims(np.loadtxt('./nonlineardata/simdata_nonlinear_'+str(ii)+'_sim'+str(jj+1)+'.txt'),2)
        
        nxps = 51
        inputdim = 1
        batchsize = 32
        latentdim = 32

        numofobs = mydf.shape[0]

        testlength = 20 


        trainset = [mydf[0:(numofobs-testlength-1),:], mydf[1:(numofobs - testlength),:]]
        testset  = mydf[-testlength:,:,:]


        mymod = ae_mod(latentdim, [nxps, inputdim])
        optimizer = tf.keras.optimizers.Adam()

        start = timeit.default_timer()
        mymod, trainingloss = fitting_wrap_noday(model = mymod, train_x = trainset, train_y = trainset,
                                            weightingvec= [1,1,1e-7],optimizer=optimizer,numofepochs=30)
        stop = timeit.default_timer()
        proprrunningtime[jj,lengthindex] = stop - start 


        stepdf = np.vstack([np.expand_dims(trainset[1][trainset[1].shape[0]-1,:],0),testset[0:(testlength-1),:,:]])
        onesteppred = np.squeeze(mymod([stepdf,stepdf])[1],2)

        np.savetxt('./nonlinearsimresults/prop_simresults_nonlinear_' + str(ii) + '_sim'+str(jj+1)+'.txt' ,onesteppred)
    lengthindex += 1 

