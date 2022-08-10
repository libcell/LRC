
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
# Linear models and empirical eBayes methods. 
#  基于以下标准的过滤：(FoldChange & Adjusted P value)

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

### ---------------------- (2) Other filter methods ------------------------ ###
# Linear models and empirical eBayes methods. 

# 1) Simulating a gene expression matrix.  

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

# 2) Filter method-1: sd过滤法  

# sd: 标准差

sd_seq <- NULL # 

for (i in 1:100) {
  
  tmp <- sd(iris.gem[, i + 1])
  
  sd_seq <- c(sd_seq, tmp)
  
}

sd_seq # all sd values for all genes. 

colnames(iris.gem)[order(sd_seq, decreasing = TRUE)[1:20] + 1]

# 2) Filter method-2: FC过滤法  
# fold change (FC) 变化差异: x1 - x2, x1/x2

fc <- function(x) mean(x[1:10]) - mean(x[11:20])

fc_seq <- apply(iris.gem[, -1], 2, fc)

order(fc_seq)

names(fc_seq)[order(fc_seq)[1:10]] # down-
names(fc_seq)[order(fc_seq)[91:100]] # up-

# 3) Filter method-3: Student t-test过滤法  
# Student t-test. 

gene1 <- as.vector(iris.gem[, 2])
names(gene1) <- rownames(iris.gem)

g1.asthma <- gene1[1:10]

g1.control <- gene1[11:20]

res <- t.test(x = g1.asthma, y = g1.control)

res$p.value

f_ttest <- function(x) {
  res <- t.test(x[1:10], x[11:20])
  res$p.value
}

ttest.pvalue <- apply(iris.gem[, -1], 2, f_ttest)

table(ttest.pvalue < 0.05)

degs <- names(ttest.pvalue)[ttest.pvalue < 0.05]

degs

op <- par(mfrow = c(1, 2))

# gene-41, 无变化

iris.gem[1:10, 41 + 1]  # in asthma
iris.gem[11:20, 41 + 1]  # in control

boxplot(iris.gem[1:10, 41 + 1], iris.gem[11:20, 41 + 1], notch = TRUE)

# gene-42, 有变化
iris.gem[1:10, 42 + 1]  # in asthma
iris.gem[11:20, 42 + 1]  # in control

boxplot(iris.gem[1:10, 42 + 1], iris.gem[11:20, 42 + 1], notch = TRUE)

par(op)

# iv) limma method.

# 4) Filter method-4: Permutation test  

### ------------------------ (2) Permutation test -------------------------- ###
# i) Example case-1  

# 使用生长素后拟南芥侧根的数量
A <- c(24, 43, 58, 67, 61, 44, 67, 49, 59, 52, 62, 50) 

# 不使用生长素的拟南芥侧根的数量
B <- c(42, 43, 65, 26, 33, 41, 19, 54, 42, 20, 17, 60, 37, 42, 55, 28)

# Question: 生长素是否能够促进拟南芥的生长？

# 解决步骤：
# S1-提出假设
#     H0: 加入的生长素不能促进拟南芥的根系发育。
#     H1: 加入的生长素能促进拟南芥的根系发育。
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

abline(v = Diff.AB, col = 2, lwd=2)

# 抽样总体中大于14的数值有9个，所以估计的P-value是9/999=0.01

p <- (sum(all.Diff > Diff.AB) + 1) / (999 + 1)

# p<0.05，因此拒绝原假设，则生长素可以促进生长。

# 若直接用Student t-test: 
# t.test(A, B)

### End of Step-03.
### ****************************************************************************



