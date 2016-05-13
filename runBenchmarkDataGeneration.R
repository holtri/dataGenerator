set.seed(NULL)

source("ExtendHiCSData.R")

writeInfo <- function(parameter, file){
  write(paste(deparse(substitute(parameter)), ":", parameter), file, ncolumns=1000, append = T)
}

datasetNumber <- '003'

numRelevantDim <- 300 # number dimensions to create correlated subspaces and place outliers
numNonRelevantDim <- 0 # number dimensions to create highly correlated low-dimensional subspaces without outliers

numObjects <- 1000 # total number of data objects

minSubspaceSize <- 2 # minimum dimensionality of subspaces that are created
maxSubspaceSize <- 6 # maximum dimensionality of subspaces that are created
numOutliersPerSubspace <- 4 #number of outliers that are placed in each of the relevant subspaces
intervals <- list(c(0, 0.2), c(0.5,1)) # intervals to distinguish between regions of inliers and outliers
# intervals <- list(c(0.05, 1))

symmetric <- 0.2 # if true, the distribution of data objects in the subspace is pairwise symmetric 

generated <- generateDataSet(datasetNumber, 
                             numRelevantDim, 
                             numNonRelevantDim, 
                             numObjects, 
                             minSubspaceSize, 
                             maxSubspaceSize, 
                             numOutliersPerSubspace, 
                             intervals, 
                             symmetric)

generated
write.table(generated$data, file=paste0("synth_multidim_", numNonRelevantDim + numRelevantDim, "_", datasetNumber, "_labeled.csv"), sep=";")
outInfo <- paste0("synth_multidim_",numNonRelevantDim + numRelevantDim, "_", datasetNumber, "_labeled.info")


write("Config\n======", outInfo, ncolumns=1000)
writeInfo(numRelevantDim, outInfo)
writeInfo(numNonRelevantDim, outInfo)
writeInfo(numObjects, outInfo)
writeInfo(minSubspaceSize, outInfo)
writeInfo(maxSubspaceSize, outInfo)
writeInfo(numOutliersPerSubspace, outInfo)
writeInfo(intervals, outInfo)
writeInfo(symmetric, outInfo)

write("\nsubspaces\n=========", outInfo, ncolumns=1000, append=T)

invisible(lapply(mapply(function(x,y) paste0(x, " - " , y), 
                        lapply(seq_along(generated$subspaces), 
                               function(x) paste0(x, ": [", lapply(generated$subspaces[x], 
                                                                   function(x) paste0(x, collapse=",")), "]")),
                        generated$subspaceTypes) , 
                 write, outInfo, append=TRUE, ncolumns=1000))

write("\noutliers\n========", outInfo, ncolumns=1000, append=T)
write(paste("total number of outliers:", sum(generated$data$class >0), "\n"), outInfo, ncolumns=1000, append=T)
outliers <- data.table("index"=generated$data[, which(class>0)], "class"=generated$data[class>0, class])
outliers <- outliers[order(class, index),lapply(.SD, paste0, collapse=","), by=class]
outliers[, index:=lapply(index, function(x) paste0(" {",x,"}"))]
outliers <- apply(outliers, 1, function(x) paste(x, collapse = ":"))

invisible(lapply(outliers, write, outInfo, append=T, ncolumns=1000))






