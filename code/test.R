
################################################################################
# (1) Simulating a gene expression matrix.  

# 100 genes, 20 samples. 

gem <- matrix(rnorm(20 * 100, sd = 0.3), nrow = 100)

dim(gem) # genes in rows, and samples in columns. 

# Generating the labels os samples. 
labs <- rep(c("Asthma", "Control"), c(10, 10))
print(labs)

gem[1:10, ] # primary expression values for first ten genes. 
gem[1:10, 1:10] <- gem[1:10, 1:10] - 10 # down-regulated genes

gem[11:20, 1:10] <- gem[11:20, 1:10] + 10 # up-regulated genes. 

# iris.gem <- cbind(labs, t(gem))

iris.gem <- data.frame(as.factor(labs), t(gem))

rownames(iris.gem) <- paste("Sample", 1:20, sep = "-")
colnames(iris.gem) <- c("Type", paste("gene", 1:100, sep = "-"))

iris.gem[, 1:21]
################################################################################

################################################################################
# (2) Identifying the DEGs using SVM-RFE.

## (1) SVM-RFE

library(e1071)

source("D:/00-GitHub/LRC/src/msvmRFE.R")

set.seed(2022)

#---------------------------- first method ----------------------------------###

svmRFE(iris.gem, k = 10, halve.above = 100)

nfold = 10
nrows = nrow(iris.gem)
folds = rep(1:nfold, len=nrows)[sample(nrows)]
results = lapply(folds, svmRFE.wrap, iris.gem, k=5, halve.above=100)
length(results)
top.features = WriteFeatures(results, iris.gem, save=F)
head(top.features)

featsweep = lapply(1:5, FeatSweep.wrap, results, iris.gem)

str(featsweep)

no.info = min(prop.table(table(iris.gem[, 1])))
errors = sapply(featsweep, function(x) ifelse(is.null(x), NA, x$error))

PlotErrors(errors, no.info=no.info)

#---------------------------- Second method ---------------------------------###

library(caret)
set.seed(1)
x <- iris.gem[, -1]
logBBB <- as.factor(labs)
svmProfile <- rfe(x, logBBB,
                  sizes = c(5, 10, 20),
                  rfeControl = rfeControl(functions = caretFuncs,
                                          number = 200),
                  ## pass options to train()
                  method = "svmRadial") # 

svmProfile$variables
str(svmProfile)
































################################################################################
x1 <- c(99, 99.5, 65, 100, 99, 99.5, 99, 99.5, 99.5, 57, 100, 99.5, 
        99.5, 99, 99, 99.5, 89.5, 99.5, 100, 99.5)
y1 <- c(99, 99.5, 99.5, 0, 50, 100, 99.5, 99.5, 0, 99.5, 99.5, 90, 
        80, 0, 99, 0, 74.5, 0, 100, 49.5)

DV <- c(x1, y1)
IV <- factor(rep(c("A", "B"), c(length(x1), length(y1))))
library(coin)                    # for oneway_test(), pvalue()
pvalue(oneway_test(DV ~ IV, 
                   alternative = "greater", 
                   distribution = approximate(B = 9999)
))

library(perm)                    # for permTS()
permTS(DV ~ IV, 
       alternative = "greater", 
       method = "exact.mc", 
       control = permControl(nmc = 10 ^ 4 - 1))$p.value

library(exactRankTests)          # for perm.test()
perm.test(DV ~ IV,
          paired = FALSE,
          alternative = "greater",
          exact = TRUE)$p.value
################################################################################

## (1) SVM-RFE

library(e1071)

source("D:/00-GitHub/LRC/src/msvmRFE.R")

set.seed(2022)

load("D:/00-GitHub/LRC/data/input.Rdata")

input[1:6, 1:10]

dim(input)

table(input$DX2)

svmRFE(input, k=10, halve.above=5)


# first method
svmRFE(iris.gem, k = 10, halve.above = 100)

nfold = 10
nrows = nrow(iris.gem)
folds = rep(1:nfold, len=nrows)[sample(nrows)]
results = lapply(folds, svmRFE.wrap, iris.gem, k=5, halve.above=100)
length(results)
top.features = WriteFeatures(results, iris.gem, save=F)
head(top.features)

featsweep = lapply(1:5, FeatSweep.wrap, results, iris.gem)

str(featsweep)

no.info = min(prop.table(table(iris.gem[, 1])))
errors = sapply(featsweep, function(x) ifelse(is.null(x), NA, x$error))

PlotErrors(errors, no.info=no.info)


library(caret)
set.seed(1)
x <- iris.gem[, -1]
logBBB <- as.factor(labs)
svmProfile <- rfe(x, logBBB,
                  sizes = 1:100,
                  rfeControl = rfeControl(functions = caretFuncs,
                                          number = 200),
                  ## pass options to train()
                  method = "svmRadial") # 

svmProfile$variables
str(svmProfile)

################################################################################
### End of chunk-12.
################################################################################



# RF-RFE

x <- iris.gem[, -1]
y <- as.factor(labs)

library(randomForest)

RF_RFE <- function(x,y){
  n = ncol(x)
  
  survivingFeaturesIndices = c(1:n)
  featureRankedList = vector(length=n)
  rankedFeatureIndex = n
  
  while(length(survivingFeaturesIndices)>1)
  {
    #First find the apt number of trees
    numTrees<-round(10^seq(1,3,by=0.2))
    numTrees
    
    errorValues<-vector()
    print("Going to find the optimal trees for this subset.")
    
    #Iterate over every numTrees to find least OOB value
    for(nt in numTrees)
    {
      # print(nt)
      rf_Model_temp<-randomForest(x[,survivingFeaturesIndices], y, ntree=nt, proximity = FALSE) #by default mtry=sqrt(Number of variables)
      #rf_Model_temp$err.rate
      
      #collect the error Values  
      errorValues<-c(errorValues,rf_Model_temp$err.rate[nt,1])
      
    }
    
    cat("\nerrorValues", errorValues)
    
    #Find the number of trees of the least OOB error 
    OptimumTrees<-numTrees[as.integer(which.min(errorValues))] #which.min returns the index of the first min value
    cat("\nOptimumTrees", OptimumTrees)
    
    
    #training the support vector machine
    RFModel <- randomForest(x[,survivingFeaturesIndices], y, ntree=OptimumTrees, proximity = FALSE, importance=TRUE)
    cat("\nRFModel: ")
    print(RFModel)
    
    #Find the importance of the model
    imp<-importance(RFModel)
    
    #Take the MeanDecreaseGini Column
    MDG<-subset(imp, select = MeanDecreaseGini)
    cat("\nMDG: ")
    print(MDG)
    
    #rank the features
    ranking = sort(MDG, index.return = TRUE)$ix
    cat("\nranking: ", ranking)
    
    #update feature ranked list
    (featureRankedList[rankedFeatureIndex] = survivingFeaturesIndices[ranking[1]])
    rankedFeatureIndex = rankedFeatureIndex - 1
    
    #eliminate the feature with smallest ranking criterion. i.e. Least relevant is removed from further Execution
    survivingFeaturesIndices <- survivingFeaturesIndices[-ranking[1]]
    
    cat("\nsurvivingFeaturesIndices: ",survivingFeaturesIndices)
    cat("\ncurrent FeatureRankedList:", featureRankedList)
    cat("\n======================================================\n")
    
    if(length(survivingFeaturesIndices) == 1)
    {
      print("inside if")
      featureRankedList[rankedFeatureIndex] = survivingFeaturesIndices[1]
      cat("\ncurrent FeatureRankedList:", featureRankedList)
      
    }
  }
  
  return (featureRankedList)
}


res.rfrfe <- RF_RFE(iris.gem[, -1], iris.gem[, 1])

res.rfrfe

#-------------------------------------------------------------------------------

library(caret)
set.seed(1)
x <- iris.gem[, -1]
logBBB <- as.factor(labs)
rfProfile <- rfe(x, logBBB,
                 sizes = c(2, 5, 10, 20),
                 rfeControl = rfeControl(functions = rfFuncs, 
                                         number = 200))

dim(rfProfile$variables)
varImp(rf_Model_temp)


################################################################################

####################################################################
## Script to select best variables for a classification mode using genetic algorithms. 
## Based on `GA` library with custom fitness function. 
## This script is explained in the post: 
## Contact: https://twitter.com/pabloc_ds
####################################################################

# Install packages if missing
list.of.packages <- c("parallel", "doParallel", "caret", "randomForest", "funModeling", "tidyverse", "GA")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Load libraries
library(caret)
library(randomForest)
library(funModeling)
library(tidyverse)
library(GA)

# ------------------------- source("lib_ga.R") ----------------------------- ###



custom_fitness <- function(vars, data_x, data_y, p_sampling)
{
  # speeding up things with sampling
  ix=get_sample(data_x, percentage_tr_rows = p_sampling)
  data_2=data_x[ix,]
  data_y_smp=data_y[ix]
  
  # keep only vars from current solution
  names=colnames(data_2)
  names_2=names[vars==1]
  # get the columns of the current solution
  data_sol=data_2[, names_2]
  
  # get the roc value from the created model
  roc_value=get_roc_metric(data_sol, data_y_smp, names_2)
  
  # get the total number of vars for the current selection
  q_vars=sum(vars)
  
  # time for your magic
  fitness_value=roc_value/q_vars
  
  return(fitness_value)
}

get_roc_metric <- function(data_tr_sample, target, best_vars) 
{
  # data_tr_sample=data_sol
  # target = target_var_s
  # best_vars=names_2
  
  fitControl <- trainControl(method = "cv", 
                             number = 3, 
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)
  
  data_model=select(data_tr_sample, one_of(best_vars))
  
  mtry = sqrt(ncol(data_model))
  tunegrid = expand.grid(.mtry=round(mtry))
  
  fit_model_1 = train(x=data_model, 
                      y= target, 
                      method = "rf", 
                      trControl = fitControl,
                      metric = "ROC",
                      tuneGrid=tunegrid
  )
  
  metric=fit_model_1$results["ROC"][1,1]
  
  return(metric)
}




get_accuracy_metric <- function(data_tr_sample, target, best_vars) 
{
  data_model=select(data_tr_sample, one_of(best_vars))
  
  fitControl <- trainControl(method = "cv", 
                             number = 3, 
                             summaryFunction = twoClassSummary)
  
  data_model=select(data_tr_sample, one_of(best_vars))
  
  mtry = sqrt(ncol(data_model))
  tunegrid = expand.grid(mtry=round(mtry))
  
  fit_model_1 = train(x=data_model, 
                      y= target, 
                      method = "rf",
                      tuneGrid = tunegrid)
  
  
  
  metric=fit_model_1$results["Accuracy"][1,1]
  return(metric)
}  

# ---------------------------------------------------------------------------- #

data <- iris.gem

# Data preparation
# data2=na.omit(data) # <- use with care...

data_y <- as.factor(data$Type)
data_x <- select(data, -Type)

# GA parameters
param_nBits <- ncol(data_x)
col_names <- colnames(data_x)

# Executing the GA 
# Executing the GA 
ga_GA_1 = ga(fitness = function(vars) custom_fitness(vars = vars, 
                                                     data_x =  data_x, 
                                                     data_y = data_y, 
                                                     p_sampling = 0.7), # custom fitness function
             type = "binary", # optimization data type
             crossover = gabin_uCrossover,  # cross-over method
             # Para-1: number of best ind. to pass directly to next iteration.
             elitism = base::max(1, round(popSize*0.05)), 
             # Para-2: mutation rate prob
             pmutation = 0.03, 
             # Para-3: the number of indivduals / solutions
             popSize = 500, 
             pcrossover = 0.8, # the probability of crossover between pairs of chromosomes. 
             nBits = param_nBits, # total number of variables
             names = col_names, # variable name
             # Para-3: max iter without improvement (stopping criteria). 
             run = 10, 
             #/////# the number of total generations
             maxiter = 50, #/////# the number of total generations
             monitor = plot, # plot the result at each iteration
             keepBest = TRUE, # keep the best solution at the end
             parallel = TRUE, # allow parallel procesing
             seed = 84211 # for reproducibility purposes
             )


# Checking the results
summary(ga_GA_1)

# Following line will return the variable names of the final and best solution
best_vars_ga=col_names[ga_GA_1@solution[1,] == 1]

# Checking the variables of the best solution...
best_vars_ga

# Checking the accuracy
get_accuracy_metric(data_tr_sample = data_x, target = data_y, best_vars_ga)

################################################################################

# GA algorithm in Caret package. 

# ctrl <- gafsControl(functions = caretGA)

library(caret)
ctrl <- gafsControl(functions = caretGA,
                    # The resampling method
                    method = "LOOCV", 
                    # a two-element string that specifies what summary metric 
                    # -will be used to select the optimal number of iterations
                    metric = NULL,
                    # should the metrics be maximized or minimized?
                    maximize = NULL,
                    # Either the number of folds or number of resampling iterations
                    number = ifelse(grepl("cv", method), 10, 25),
                    # the number of complete sets of folds to compute
                    repeats = ifelse(grepl("cv", method), 1, 5),
                    # a logical for printing results
                    verbose = FALSE, 
                    # how much of the resampled summary metrics should be saved. 
                    returnResamp = "final",
                    # For leave-group out cross-validation: the training percentage 
                    p = 0.75, 
                    # a list with elements for each resampling iteration. 
                    index = NULL,
                    # which sample are held-out for each resample.
                    indexOut = NULL,
                    # a vector or integers that can be used to set the seed during each search.
                    seeds = NULL,
                    # the proportion of data in [0, 1) to be held-back 
                    holdout = 0, 
                    # if a parallel backend is loaded and available
                    genParallel = FALSE,
                    # if a parallel backend is loaded and available
                    allowParallel = TRUE)

ctrl <- gafsControl(functions = caretGA, 
                    genParallel = TRUE, 
                    allowParallel = TRUE)
obj <- gafs(x = iris.gem[, -1], 
            y = iris.gem[, 1],
            iters = 100,
            gafsControl = ctrl)


dim(ga_GA_1@solution)
