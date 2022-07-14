
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 05th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 07: Installing & Running R Language.
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

gem <- rnorm(2000*100, mean = 8, sd = 2)

length(gem)

is.matrix(gem)

gem <- matrix(gem, nrow = 2000)

dim(gem)

head(gem)

rownames(gem) <- paste("mRNA", 1:2000, sep = "-")

colnames(gem) <- paste("sample", 1:100, sep = "-")

head(gem)

# 2) Checking the data distribution.  

# 3) Computing the general statistics. 

gem["mRNA-1", ]

gem[, "sample-1"]

gem["mRNA-1", "sample-1"]

gem[1, 1]

# 1) average value of gene-1. 

mean(gem[1, ])
mean(gem["mRNA-1", ])

range(gem["mRNA-1", ])

summary(gem["mRNA-1", ])

# 2) correlation

r.value <- cor(gem)

dim(r.value)

head(r.value)

cov(gem)

# 4) Normalizing the data. 

library(DT)

datatable(gem)

datatable(iris)

boxplot(iris[, -5], col = 2:5)

boxplot(scale(iris[, -5]), col = 2:5)

# centering, 

S1 <- runif(26, min = 20, max = 80) # random sampling 
S1 <- round(S1)
names(S1) <- LETTERS
print(S1)

S2 <- runif(20, min = 50, max = 100) # random sampling 
S2 <- round(S2)
names(S2) <- letters[1:20]
print(S2)

mean(S1)
mean(S2)

S1
S2

# D vs. d

D <- 68
d <- 77

op <- par(mfrow = c(1, 2))
boxplot(S1)
boxplot(S2)
par(op)

D1 <- 68 - mean(S1)
d1 <- 77 - mean(S2)

D1
d1

D2 <- (68 - mean(S1))/sd(S1)
d2 <- (77 - mean(S2))/sd(S2)
D2
d2

# 归一化

D3 <- ((68 - min(S1))/(max(S1) - min(S1)))*100 
d3 <- ((77 - min(S2))/(max(S2) - min(S2)))*100 

D3
d3

# S1

# 5) Visualizing the data. 

### End of Step-02.
### ****************************************************************************

 
### ****************************************************************************
### Step-03. Skewness and kurtosis. 

# 1) Installing the related R package. 

install.packages("fBasics")

library(fBasics)

skewness(iris[, -5])

kurtosis(iris[, -5])

# 2) Computing the statistics.


### End of Step-03.
### ****************************************************************************



dim(gem)

g1 <- gem[1, ]
g2 <- gem[2, ]
g3 <- gem[3, ]
g4 <- gem[4, ]

hist(gem, breaks = 20, probability = TRUE, ylim = c(0, .25))

lines(density(g1), col = "red", lwd = 3)
lines(density(g2), col = "green", lwd = 3)
lines(density(g3), col = "blue", lwd = 3)

hist(gem, breaks = 20, probability = TRUE, ylim = c(0, .25))

for (i in 1:10) {
  lines(density(gem[i, ]), col = rainbow(10)[i], lwd = 2)
}


boxplot(gem[, 1:10], col = 1:10)


# par(mfrow = c(1, 1))



### ****************************************************************************
### Step-04. Understanding the types of data. 

# 1) Continuous data 


# 2) Discrete data 


### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. Data visualization of gene expression data.  

# 1) Reading the XML files. 


# 2) Writing the XML files. 

### End of Step-05.
### ****************************************************************************

################################################################################
### End of chunk-01.
################################################################################
