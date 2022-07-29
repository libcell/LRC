
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 14th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 07: Data Distribution and Normalization.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. Simulating a gene expression matrix. 

# 1) Simulating a GEM showing expression levels of 2000 genes in 100 samples. 

# 随机生成200000个数值，这些数值来自均值为8标准差为2的正态分布
gem <- rnorm(2000*100, mean = 8, sd = 2)

length(gem)

is.matrix(gem)

gem <- matrix(gem, nrow = 2000)

dim(gem)

head(gem)

# 2) Naming the data distribution.  

rownames(gem) <- paste("mRNA", 1:2000, sep = "-")

colnames(gem) <- paste("sample", 1:100, sep = "-")

head(gem)

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Checking the distribution of gene expression data. 

# 1) Installing and loading the related R package. 

# install.packages("fBasics")

library(fBasics)


# 2) Computing the statistics, skewness and kurtosis.

gem1 <- as.data.frame(gem)

skewness(gem1)

kurtosis(gem1)

# 3) Generating the histogram and fitting lines. 

dim(gem)

g1 <- gem[1, ]
g2 <- gem[2, ]
g3 <- gem[3, ]
g4 <- gem[4, ]

hist(gem, breaks = 20, probability = TRUE, ylim = c(0, .25))

lines(density(g1), col = "red", lwd = 3)
lines(density(g2), col = "green", lwd = 3)
lines(density(g3), col = "blue", lwd = 3)

# 4） Another way for step-3. 

op <- par(mfrow = c(1, 2))

hist(gem, breaks = 20, probability = TRUE, ylim = c(0, .25))

for (i in 1:10) {
  lines(density(gem[i, ]), col = rainbow(10)[i], lwd = 2)
}

boxplot(gem[, 1:10], col = 1:10)

par(op)

### End of Step-03.
### **************************************************************************** 

### ****************************************************************************
### Step-04. Understanding centering and scaling for datasets in bio-medicine. 

# 1) Taking the iris score analysis for two classes as the example.

set.seed(714)

S1 <- runif(26, min = 20, max = 80)

S2 <- runif(20, min = 50, max = 100)

S1 <- round(S1);  S2 <- round(S2)

names(S1) <- LETTERS

names(S2) <- letters[1:20]

# 2) Comparing the score of student D in class-1 and student d in class-2. 

D1 <- S1["D"]
d1 <- S2["d"]
D1 > d1

D2 <- D1 - mean(S1)
d2 <- d1 - mean(S2)
D2 > d2

D3 <- (D1 - mean(S1)) / sd(S1) 
d3 <- (d1 - mean(S2)) / sd(S2) 
D3 > d3

D4 <- ((D1 - min(S1)) / (max(S1) - min(S1))) * 100
d4 <- ((d1 - min(S2)) / (max(S2) - min(S2))) * 100
D4 > d4

# 3) Which one is the best way to compare the score between student D and d?

### End of Step-04.
### ****************************************************************************

################################################################################
### End of chunk-01.
################################################################################
