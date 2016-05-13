source("ExtendHiCSData.R")
## ---- parameters

datasetNumber <- '001'

numRelevantDim <- 30 # number dimensions to create correlated subspaces and place outliers
numNonRelevantDim <- 10 # number dimensions to create highly correlated low-dimensional subspaces without outliers

numObjects <- 1000 # total number of data objects

minSubspaceSize <- 2 # minimum dimensionality of subspaces that are created
maxSubspaceSize <- 3 # maximum dimensionality of subspaces that are created
numOutliersPerSubspace <- 4 #number of outliers that are placed in each of the relevant subspaces
intervals <- list(c(0, 0.4), c(0.6, 0.9)) # intervals to distinguish between regions of inliers and outliers

symmetricProportion <- 0 # proportion of subspaces that are correlated symmetrically

## ---- end 

## ---- sampleCall
set.seed(42)
generated <- generateDataSet(datasetNumber, 
                              numRelevantDim, 
                             numNonRelevantDim, 
                              numObjects, 
                              minSubspaceSize, 
                              maxSubspaceSize, 
                              numOutliersPerSubspace, 
                              intervals, 
                             symmetricProportion)

## ---- end
write.table(generated$data, file=paste0("synth_multidim_", numNonRelevantDim + numRelevantDim, "_", datasetNumber, "_labeled.csv"), sep=";")
invisible(lapply(generated$subspaces, write, paste0("synth_multidim_",numNonRelevantDim + numRelevantDim, "_", datasetNumber, "_labeled.info"), append=TRUE, ncolumns=1000))
