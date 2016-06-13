set.seed(NULL)

source("ExtendHiCSData.R")

writeInfo <- function(parameter, file){
  write(paste(deparse(substitute(parameter)), ":", parameter), file, ncolumns=1000, append = T)
}

datasetNumber <- '018'

numRelevantDim <- 20 # number dimensions to create correlated subspaces and place outliers
numNonRelevantDim <- 230 # number dimensions to create highly correlated low-dimensional subspaces without outliers

numObjects <- 2000 # total number of data objects

minSubspaceSize <- 2 # minimum dimensionality of subspaces that are created
maxSubspaceSize <- 5 # maximum dimensionality of subspaces that are created
numOutliersPerSubspace <- 6 #number of outliers that are placed in each of the relevant subspaces
intervals <- list(c(0.05, 1)) # intervals to distinguish between regions of inliers and outliers
# intervals <- list(c(0.05, 1))

symmetric <- 1 # proportion of subspaces that are correlated symmetrically
onlyOutlierAsymm <- F


generated <- generateDataSet(datasetNumber, 
                             numRelevantDim, 
                             numNonRelevantDim, 
                             numObjects, 
                             minSubspaceSize, 
                             maxSubspaceSize, 
                             numOutliersPerSubspace, 
                             intervals, 
                             symmetric,
                             onlyOutlierAsymm)

# generated 
write.table(generated$data, file=paste0("synth_multidim_", numNonRelevantDim + numRelevantDim, "_", datasetNumber, "_labeled.csv"), sep=";", row.names = F)
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
writeInfo(onlyOutlierAsymm, outInfo)
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






