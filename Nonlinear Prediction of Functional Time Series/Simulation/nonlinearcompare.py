import numpy as np 
import matplotlib.pyplot as plt 


prop_mspe       = np.zeros((4,100,3))
fplsr_mspe      = np.zeros((4,100,3))    
funseries_mspe  = np.zeros((4,100,3))
dfpca_mspe      = np.zeros((4,100,3))


serieslength = [100,500,1000,2000]


index = 0 
for ii in serieslength:
    for jj in range(100):
        mydf = np.expand_dims(np.loadtxt('./nonlineardata/simdata_nonlinear_'+str(ii)+'_sim'+str(jj+1)+'.txt'),2)
        prop_pred = np.loadtxt('./nonlinearsimresults/prop_simresults_nonlinear_' + str(ii) + '_sim'+str(jj+1)+'.txt')
        ftsa_pred = np.loadtxt('./nonlinearsimresults/funseries_simresults_nonlinear_' + str(ii) + '_sim'+str(jj+1)+'.txt').T
        fplsr_pred = np.loadtxt('./nonlinearsimresults/fplsr_simresults_nonlinear_' + str(ii) + '_sim'+str(jj+1)+'.txt').T
        dfpca_pred = np.loadtxt('./nonlinearsimresults/dfpca_simresults_nonlinear_' + str(ii) + '_sim'+str(jj+1)+'.txt')
        numofobs = mydf.shape[0]

        testlength = 20 


        trainset = [mydf[0:(numofobs-testlength-1),:], mydf[1:(numofobs - testlength),:]]
        testset  = mydf[-testlength:,:,:]

        

        properror_onestep = prop_pred - np.squeeze(testset,2) 
        funseries_8   = ftsa_pred -np.squeeze(testset,2) 
        plsr_error    = fplsr_pred -np.squeeze(testset,2) 
        dfpca_error   = dfpca_pred -np.squeeze(testset,2) 

        prop_mspe[index,jj,0] = np.quantile(np.mean(properror_onestep**2,1),0.05)
        prop_mspe[index,jj,1] = np.median(np.mean(properror_onestep**2,1))
        prop_mspe[index,jj,2] = np.quantile(np.mean(properror_onestep**2,1),0.95)
        
        fplsr_mspe[index,jj,0] = np.quantile(np.mean(plsr_error**2,1),0.05)
        fplsr_mspe[index,jj,1] = np.median(np.mean(plsr_error**2,1))
        fplsr_mspe[index,jj,2] = np.quantile(np.mean(plsr_error**2,1),0.95)
        
        funseries_mspe[index,jj,0] = np.quantile(np.mean(funseries_8**2,1),0.05)
        funseries_mspe[index,jj,1] = np.median(np.mean(funseries_8**2,1))
        funseries_mspe[index,jj,2] = np.quantile(np.mean(funseries_8**2,1),0.95)
        
        dfpca_mspe[index,jj,0] = np.quantile(np.mean(dfpca_error**2,1),0.05)
        dfpca_mspe[index,jj,1] = np.median(np.mean(dfpca_error**2,1))
        dfpca_mspe[index,jj,2] = np.quantile(np.mean(dfpca_error**2,1),0.95)
        
    index += 1

seriesindex = 3

np.mean(prop_mspe[seriesindex,:,:],0);np.std(prop_mspe[seriesindex,:,:],0)
np.mean(fplsr_mspe[seriesindex,:,:],0);np.std(fplsr_mspe[seriesindex,:,:],0)
np.mean(funseries_mspe[seriesindex,:,:],0);np.std(funseries_mspe[seriesindex,:,:],0)
np.mean(dfpca_mspe[seriesindex,:,:],0);np.std(dfpca_mspe[seriesindex,:,:],0)

