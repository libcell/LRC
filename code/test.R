
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

## SVM-RFE

### ---------------------------- First usage ------------------------------- ###

library(e1071)

source("D:/00-GitHub/LRC/src/msvmRFE.R")

set.seed(2022)

svmRFE(iris.gem, k = 10, halve.above = 5)

# fold change

### ---------------------------- Second usage ------------------------------ ###

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

### ---------------------------- Third usage ------------------------------- ###

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
# (3) Identifying the DEGs using Permutation test.
## 3) Permutation test

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

################################################################################
# (3) Identifying the DEGs using RF-RFE.
## 3) RF-RFE

### ---------------------------- First usage ------------------------------- ###

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

### ---------------------------- Second usage ------------------------------ ###

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

################################################################################
# (5) Identifying the DEGs using Genetic Algorithm.
## 5) GA-FS

### ---------------------------- First usage ------------------------------- ###

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

data_y <- as.factor(data$Type)
data_x <- select(data, -Type)

# GA parameters
param_nBits <- ncol(data_x)
col_names <- colnames(data_x)

# Executing the GA 
ga_GA <-  ga(fitness = function(vars) custom_fitness(vars = vars, 
                                                     data_x =  data_x, 
                                                     data_y = data_y, 
                                                     p_sampling = 0.7), # custom fitness function
             type = "binary", # optimization data type
             crossover = gabin_uCrossover,  # cross-over method
             # Para-1: the number of best ind. passing directly to next iteration.
             elitism = 5, 
             # Para-2: mutation rate prob
             pmutation = 0.1, 
             # Para-3: the number of indivduals / solutions
             popSize = 100, 
             # Para-4: probability of crossover between pairs of chromosomes. 
             pcrossover = 0.5, 
             nBits = param_nBits, # total number of variables
             names = col_names, # variable name
             # Para-4: the number of consecutive generations without improvement (stopping criteria). 
             run = 500, 
             # Para-5: the number of total generations
             maxiter = 500, #/////# the number of total generations
             monitor = plot, # plot the result at each iteration
             keepBest = TRUE, # keep the best solution at the end
             parallel = TRUE, # allow parallel processing
             seed = 2022 # for reproducibility purposes
)

# Checking the results
summary(ga_GA)
str(ga_GA)
apply(ga_GA@solution, 2, sum)

# Following line will return the variable names of the final and best solution
best_vars_ga=col_names[ga_GA@solution[1,] == 1]

# Checking the variables of the best solution...
best_vars_ga

# Checking the accuracy
get_accuracy_metric(data_tr_sample = data_x, target = data_y, best_vars_ga)

### ---------------------------- Second usage ------------------------------ ###

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

### ---------------------------- Third usage ------------------------------- ###

library(FSinR)
GA_test <- function(data, labels){
  
  filter_evaluator <- filterEvaluator('binaryConsistency')
  
  search_method <- searchAlgorithm('geneticAlgorithm',
                                   list(popSize =200,pcrossover = 0.8,
                                        pmutation = 0.1,maxiter = 10,
                                        run = 10))
  input <- as.data.frame(t(data))
  
  input$sam.lab <- labels
  
  result <- featureSelection(input, 'sam.lab', search_method, filter_evaluator)
  
  GA.features<-result$bestFeatures
  
  GADEG<-t(GA.features)
  
  GADEG1 <- as.data.frame(apply(GADEG, 1, sum))
  
  rownames(GADEG1) <- rownames(GADEG)
  colnames(GADEG1) <- "value"
  
  GADEG2 <- as.data.frame(GADEG1[rev(order(GADEG1[,1])),])
  
  rownames(GADEG2) <- rownames(GADEG1)[rev(order(GADEG1$value))]
  
  return(GADEG2)
}

deg.ga.all <- NULL

for(m in 1:length(eset.id)){
  
  deg.ga.a <- NULL
  
  for(n in 1:length(eset.id[[m]])){
    
    deg.ga <- NULL
    
    e <- eset.id[[m]][[n]]
    
    labels <- colnames(eset.id[[m]][[n]])
    
    deg.ga <- GA_test(e, labels)
    
    deg.ga.a[[n]] <- as.data.frame(deg.ga)
    
  }
  
  deg.ga.all[[m]] <- deg.ga.a
}

# save.image(deg.ga.all, file = "deg.ga.all.RData")

################################################################################

sample <- tibble::tibble(
  AreaShape_MinorAxisLength = c(10, 12, 15, 16, 8, 8, 7, 7, 13, 18),
  AreaShape_MajorAxisLength = c(35, 18, 22, 16, 9, 20, 11, 15, 18, 42),
  AreaShape_Area = c(245, 151, 231, 179, 50, 112, 53, 73, 164, 529)
)
variables <- c("AreaShape_MinorAxisLength", "AreaShape_MajorAxisLength", "AreaShape_Area")
svd_entropy(sample, variables, cores = 1)

################################################################################
# Parse the level5 dataset from LINCS2020. 

library(cmapR)
ds <- parse_gctx("level5_beta_trt_cp_n720216x12328.gctx")
str(ds)

# experiments in columns, and genes in rows. 
# The following is the gene expression matrix. 
pertubation_mat <- ds@mat

dim(pertubation_mat)

sig_info <- data.table::fread("siginfo_beta.txt", data.table = FALSE)

dim(sig_info)

# Total number of compounds in LINCS2020

length(table(sig_info$cmap_name))

DT::datatable(sig_info[1:100, ])

# The compounds which meet the condition of pert_type = trt_cp 

selected.sigs <- sig_info[sig_info$pert_type == "trt_cp", ]

nrow(selected.sigs)

length(table(selected.sigs$cmap_name))



sum(table(sig_info$cmap_name) == 1)

col_meta <- read_gctx_meta("level5_beta_trt_cp_n720216x12328.gctx", 
                           dim="col")

dim(col_meta)




b <- readRDS("btrees.rds")
b1 <- b$dtree
b2 <- b$ptree
b3 <- b$ltree

class(b1)

as.hclust(b2)

library(ape)
as.hclust.phylo(b1)


plot(b1)









class(Boston)
head(Boston)

summary(Boston)

Boston$chas

### example using Boston data in package MASS
data(Boston, package = "MASS")

## multiscale bootstrap resampling (non-parallel)
boston.pv <- pvclust(Boston, nboot=100, parallel=FALSE)

## CAUTION: nboot=100 may be too small for actual use.
##          We suggest nboot=1000 or larger.
##          plot/print functions will be useful for diagnostics.

## plot dendrogram with p-values
plot(boston.pv)

ask.bak <- par()$ask
par(ask=TRUE)

## highlight clusters with high au p-values
pvrect(boston.pv)

## print the result of multiscale bootstrap resampling
print(boston.pv, digits=3)

## plot diagnostic for curve fitting
msplot(boston.pv, edges=c(2,4,6,7))

par(ask=ask.bak)

## print clusters with high p-values
boston.pp <- pvpick(boston.pv)
boston.pp

### Using a custom distance measure

## Define a distance function which returns an object of class "dist".
## The function must have only one argument "x" (data matrix or data.frame).
cosine <- function(x) {
  x <- as.matrix(x)
  y <- t(x) %*% x
  res <- 1 - y / (sqrt(diag(y)) %*% t(sqrt(diag(y))))
  res <- as.dist(res)
  attr(res, "method") <- "cosine"
  return(res)
}

result <- pvclust(Boston, method.dist=cosine, nboot=100)
plot(result)

## Not run: 
### parallel computation
result.par <- pvclust(Boston, nboot=1000, parallel=TRUE)
plot(result.par)

## End(Not run)



################################################################################

install.packages("ampir")

library(ampir)

my_protein_df <- read_faa(system.file("extdata/little_test.fasta", package = "ampir"))

my_prediction <- predict_amps(my_protein_df, model = "precursor")

my_predicted_amps <- my_protein_df[my_prediction$prob_AMP > 0.8,]

df_to_faa(my_predicted_amps, tempfile("my_predicted_amps.fasta", tempdir()))

################################################################################

library(cmapR)

# access the data matrix
m <- mat(ds)

# access the row and column metadata
rdesc <- meta(ds, dimension = "row")
cdesc <- meta(ds, dimension = "column")

# access the row and column ids
rid <- ids(ds, dimension = "row")
cid <- ids(ds, dimension = "column")

# update the matrix data to set some values to zero
# note that the updated matrix must be the of the same dimensions as 
# the current matrix
m[1:10, 1:10] <- 0
mat(ds) <- m

# replace row and column metadata
meta(ds, dimension = "row") <- data.frame(x=sample(letters, nrow(m),
                                                   replace=TRUE))
meta(ds, dimension = "column") <- data.frame(x=sample(letters, ncol(m),
                                                      replace=TRUE))

# replace row and column ids
ids(ds, dimension = "row") <- as.character(seq_len(nrow(m)))
ids(ds, dimension = "column") <- as.character(seq_len(ncol(m)))

# and let's look at the modified object
ds

# create a variable to store the path to the GCTX file
# here we'll use a file that's internal to the cmapR package, but
# in practice this could be any valid path to a GCT or GCTX file
ds_path <- system.file("extdata", "modzs_n25x50.gctx", package="cmapR")
my_ds <- parse_gctx(ds_path)
my_ds

# read just the first 10 columns, using numeric indices
(my_ds_10_columns <- parse_gctx(ds_path, cid=1:10))

# read the column metadata
col_meta <- read_gctx_meta(ds_path, dim="col")

# figure out which signatures correspond to vorinostat by searching the 'pert_iname' column
idx <- which(col_meta$pert_iname=="vemurafenib")

# read only those columns from the GCTX file by using the 'cid' parameter
vemurafenib_ds <- parse_gctx(ds_path, cid=idx)

# get a vector of character ids, using the id column in col_meta
col_ids <- col_meta$id[idx]
vemurafenib_ds2 <- parse_gctx(ds_path, cid=col_ids)

x <- rnorm(50)
y <- runif(30)

ks.test(x, y)

ks.test(y, "punif")

ks.test(x, "pnorm")

x <- x + rnorm(50, 0, 10^-8)

x[2] <- x[2] + 0.000000001


library(spgs)
