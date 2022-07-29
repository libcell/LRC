
################################################################################
#    &&&....&&&    % Learning R Course in Summer, teached by Bo Li             #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 06th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 02: Understanding the data objects and their structure.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. About objects in R. 

# 1) object and its attribute. 

# 2) object and its type.

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. About vector in R. 

# 1) Generating vectors. 

w <- c(1, 3, 4, 5, 6, 7)

class(w)

is.character(w)
is.numeric(w)
is.logical(w)

length(w)

e <- c(20.30, 20.01, 19.02, 20.00, 15.03)

mean(e)

nm <- c("Zhang San", "Wang Wu", "Li Xiao", "XXX", "YYY")

class(nm)

is.numeric(nm)

mx <- c(1, 2.3, "Shanghai")

class(mx)

print(mx)

# 2) vector

x <- seq(from = 1, to = 100, by = 5)

print(x)

x <- seq(from = 1, to = 100, length = 30)

length(x)

print(x)

x2 <- rep(1, 5)

length(x2)

print(x2)

x3 <- rep(c(1, 2), 5)

print(x3)

x4 <- rep(c(1, 2), times = c(3, 5))

x4

x5 <- rnorm(1000, mean = 5, sd = 2)   # randomly sampling from normal distribution

x5

mean(x5)

sd(x5)


# 引用向量中的元素. 

x5[997]

x5[1:5]

x5[c(3, 2)]

x6 <- x5[1:20]

x7 <- x6[1:19]

x8 <- x6[-20]

x9 <- (x7 == x8)

x9[2] <- FALSE 

x9

x10 <- x7[x9]

max(x10)
min(x10)

which.max(x10)
which.min(x10)

max(x10) == x10[which.max(x10)]



### matrix. 

x <- rnorm(20*10, mean = 0, sd = 1)

y <- matrix(x, nrow = 20, ncol = 10)

print(y)

class(y)

dim(y)

y[20, 10]

y[20, 9:10]

y[20, ]

y[, 1]

length(y)




a <- 1:10
b <- 2:11

y1 <- cbind(a, b)

class(y1)

y1

y2 <- rbind(a, b)

y2

dim(y2)
dim(y1)

y2[2, 6] <- "Beijing"

y2

y3 <- as.vector(y2) # 矩阵转为向量

dim(y3)
dim(y2)


print(iris)

class(iris)

dim(iris)


getwd()

setwd("D:/")

dir()

y1

write.csv(y1, file = "y1.csv")

dir()

y5 <- read.csv("y1.csv")

y5

options(prompt = "R>", continue = "++++++")

(.packages())
?hclust
### End of Step-03.
### ****************************************************************************

################################################################################
### End of chunk-02.
################################################################################
