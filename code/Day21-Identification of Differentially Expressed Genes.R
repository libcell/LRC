
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 28th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 20: Primary drawing in R.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# 1) For windows, work directory. 

pri.dir <- getwd()

if (!dir.exists("asthma")) dir.create("asthma")

setwd("asthma")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. Reading the raw data set for GSE470. 

# 1) setting the work directory. 

s <- "GSE470"
setwd(s)
# 2) # reading all files in *.cel format. 

# 3) Normalizing the data. 

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Identifying.  

### -------------------------- (1) LIMMA method ---------------------------- ###
# Linear models and empirical bayes methods. 

# i) loading the limma package. 

library(limma)

# ii) Simulating gene expression data: 10 microarrays and 100 genes. 

eset <- matrix(rnorm(100*10, sd = 0.3), 100, 10)
eset[1:5, 6:10] <- eset[1:5, 6:10] + 10
eset[6:10, 6:10] <- eset[1:5, 6:10] - 10
rownames(eset) <- paste("gene", 1:100, sep = "-")

# iii) Preparing the design matrix corresponds to expriment. 
disease <- factor(rep(c(1,2), times = c(5,5)))
levels(disease) <- c("normal","abnormal")
design <- model.matrix(~ 1 + disease)
colnames(design) <- c("normal", "abnormal_vs_normal")

# iv) Considering original two estimates plus difference between first 4 and last 6 arrays

fit <- lmFit(eset, design)
fit <- eBayes(fit)
res <-
  topTable(fit,
           coef = "abnormal_vs_normal",
           adjust = "BH",
           number = 100)

tail(res)

degs <- res[abs(res$logFC) > 1 & res$adj.P.Val < 0.05, ]

rownames(degs)

### ------------------------ (2) Permutation test -------------------------- ###
# i) Example case-1  

# 使用生长素后拟南芥侧根的数量
A <- c(24, 43, 58, 67, 61, 44, 67, 49, 59, 52, 62, 50) 

# 不使用生长的拟南芥侧根的数量
B <- c(42, 43, 65, 26, 33, 41, 19, 54, 42, 20, 17, 60, 37, 42, 55, 28)

# Question: 生长素是否能够促进拟南芥的生长？

# 解决步骤：
# S1-提出假设
#     H0: 加入的生长素能促进拟南芥的根系发育。
#     H1: 加入的生长素不能促进拟南芥的根系发育。
#    推论：若H0成立，那么A组数据的分布和B组数据的分布是一样的，也就是服从同个分布。
# S2-构造假设检验的统计量：
#    Diff: A组侧根数目的均值同B组侧根数目的均值之差。
#         Diff = mean(Xa) - mean(Xb)

Mix <- c(A, B)
all.Diff <- NULL
for (i in 1:999) {
  # 将A和B混合，从中挑出12个作为新的A组，剩余的为B组。
  loc <- sample(1:length(Mix), 12, replace = FALSE)
  Xa <- Mix[loc]
  Xb <- Mix[-loc]
  # 计算并记录第一步中A组同B组的均值之差。
  Diff <- mean(Xa) - mean(Xb)
  all.Diff <- c(all.Diff, Diff)
  # 对前两步重复999次（重复次数越多，得到的背景分布越”稳定“）
}

hist(all.Diff, breaks = 20, prob = TRUE)
lines(density(all.Diff))

# S3-检查一次抽样的观察值所在位置，并计算P值。 

Diff.AB <- mean(A) - mean(B)

abline(v = Diff.AB, col = 2)

# 抽样总体中大于14的数值有9个，所以估计的P-value是9/999=0.01

p <- (sum(all.Diff > Diff.AB) + 1) / (999 + 1)

# p<0.05，因此拒绝原假设，则生长素可以促进生长。

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

# for 

gem <- matrix(rnorm(20 * 100, sd = 0.3), nrow = 100)

labs <- rep(c("Asthma", "Control"), c(10, 10))

gem[1:10, 1:10] <- gem[1:10, 1:10] - 10 # down-

gem[11:20, 1:10] <- gem[11:20, 1:10] + 10 # up-

# iris.gem <- cbind(labs, t(gem))

iris.gem <- data.frame(as.factor(labs), t(gem))

rownames(iris.gem) <- paste("Sample", 1:20, sep = "-")
colnames(iris.gem) <- c("Type", paste("gene", 1:100, sep = "-"))

iris.gem[, 1:21]

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


