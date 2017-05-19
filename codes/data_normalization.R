# Citation: Singh, Bikesh Kumar, Kesari Verma, and A. S. Thoke. "Investigations on impact of feature normalization techniques on classifier's performance in breast tumor classification." International Journal of Computer Applications 116.19 (2015).

########################################################################################
# This technique normalizes data in range [0, 1].
# linear scaling to [0,1]
Linear.Scaling.function <- function(x)
{
  return ((x-min(x))/(max(x)-min(x)))
}

Linear.Scaling.Normalization <- function(x)
{
  as.data.frame(lapply(x, Linear.Scaling.function))
}

Min.Max.function <- function(x)
{
  min_new = -1;
  max_new = 1;
  return (((x-min(x))/(max(x)-min(x)))*(max_new-min_new)+min_new)
}

Min.Max.Normalization <- function(x)
{
  as.data.frame(lapply(x, Min.Max.function))
}


########################################################################################
# This functions transformation technique to normalize data in range [-1, 1].or they called standard normalization
#z score standarization (single column)
Z.Score <- function(x) 
{
  return ((x - mean(x) ) / sd(x))
}

Z.Score.Normalization <- function(x) 
{
  as.data.frame(lapply(x, Z.Score))
}

#z score standarization using base package (Multi columns)
Z.Score.Default.Normalization <- function(x){
  
  as.data.frame(scale(x))
}



########################################################################################
# Non linear normalization
# techniques may be used in cases where data are not evenly
# distributed around the mean.
# This function is based on nonlinear (i.e., exponential, logarithmic, sigmoid etc.) 
# function normalize data in range [0, 1].
Softmax.Scaling <- function(x)
{
  return (1/(1+(exp(-(x)))))
  
}

Softmax.Scaling.Normalization <- function(x)
{
  as.data.frame(lapply(x, Softmax.Scaling))
}


##############################
#############################################################################
# READ DATA
#############################################################################
# Path File
SetPathFile = "trainingdata.csv";


# -------------------------------------------------------
# Read Data and separate from class column
ReadDataFromFile <- read.csv(file=SetPathFile, header=FALSE, sep=",")
NumberColumn = ncol(ReadDataFromFile)
# Select Data only from dataset
Data_NoClassName = ReadDataFromFile[,1:NumberColumn-1]
# Select only class/category from Data
Data_OnlyClassName = ReadDataFromFile[,NumberColumn]
# -------------------------------------------------------



#############################################################################
# NORMALIZE THE DATA AND SAVE
#############################################################################
GetOnlyFileName = sub('\\..*$', '', basename(SetPathFile))

#-----------------------------------------------------------
# Linear.Scaling.Normalization
LinearScale = Linear.Scaling.Normalization(Data_NoClassName)
LinearScale <- cbind(LinearScale, Data_OnlyClassName) # add column class again

# Write Linear.Scaling.Normalization
write.table(LinearScale, file = paste(GetOnlyFileName, "_LinearScale.csv", sep = ""), quote=FALSE, row.names=FALSE, na="", col.names=FALSE, sep=",")

#-----------------------------------------------------------
# Min.Max.Normalization
Min.Max = Min.Max.Normalization(Data_NoClassName)
Min.Max <- cbind(Min.Max, Data_OnlyClassName) # add column class again

# Write Linear.Scaling.Normalization
write.table(Min.Max, file = paste(GetOnlyFileName, "_MinMax.csv", sep = ""), quote=FALSE, row.names=FALSE, na="", col.names=FALSE, sep=",")

#-----------------------------------------------------------
# Z.Score.Normalization
ZScore = Z.Score.Normalization(Data_NoClassName)
ZScore <- cbind(ZScore, Data_OnlyClassName) # add column class again

# Write Linear.Scaling.Normalization
write.table(ZScore, file = paste(GetOnlyFileName, "_ZScore.csv", sep = ""), quote=FALSE, row.names=FALSE, na="", col.names=FALSE, sep=",")

#-----------------------------------------------------------
# Softmax.Scaling.Normalization
SoftmaxScaling = Softmax.Scaling.Normalization(Data_NoClassName)
SoftmaxScaling <- cbind(SoftmaxScaling, Data_OnlyClassName) # add column class again

# Write Linear.Scaling.Normalization
write.table(SoftmaxScaling, file = paste(GetOnlyFileName, "_SoftmaxScaling.csv", sep = ""), quote=FALSE, row.names=FALSE, na="", col.names=FALSE, sep=",")

