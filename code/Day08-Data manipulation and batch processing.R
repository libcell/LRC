
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 15th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 08: Data Manipulation and Batch Processing.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. Common functions in R.  

# 1) 生成向量x
x <- rnorm(100, mean = 0, sd = 2)

print(x)

# 2) 数学函数

abs(-10)

abs_x <- abs(x)

print(abs_x)


round(x)
ceiling(x)
floor(x)

?round
round(x, digits = 2)

y <- sin(x)
y

plot(x, y) # 

# 3) 统计函数

mean(x)
median(x)
max(x)
min(x)
range(x)
sum(x)

cumsum(x)

quantile(x, seq(from = 0, to = 1, len = 10))

# 4) 概率函数

# 正态分布

rnorm(n = 10, mean = 0, sd = 1)
qnorm(p = (1:5)/10, mean = 0, sd = 1)
pnorm((1:5)/10)
dnorm((1:5)/10)

plot(dnorm((1:5)/10))

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Simulating a gene expression matirx in R. 

# 1) Simulating a GEM showing expression levels of 2000 genes in 100 samples. 

# 随机生成20000个数值，这些数值来自均值为8标准差为8的正态分布

gem <- rnorm(n = 200*100, mean = 8, sd = 8)

gem <- matrix(gem, nrow = 200)

class(gem)

gem <- as.data.frame(gem)

class(gem)

# 2) Naming the data distribution.  

rownames(gem) <- paste("gene", 1:200, sep = "-")
colnames(gem) <- paste("sample", 1:100, sep = "-")

DT::datatable(gem)

gem1 <- round(gem, digits = 2)

class(gem1)

DT::datatable(gem1)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. Family of apply functions. 

# 1) 向量化操作

a <- 1:4
b <- 5:10

a + 1
b + 1

a + b

# 2) apply. 

dim(gem1)

class(gem1)

gem2 <- as.matrix(gem1)

gem2[1:10, 1:6]

mean(gem2[1, ])
mean(gem2[2, ])
mean(gem2[200, ])

m <- apply(gem2, 1, mean)
print(m)

ma <- apply(gem2, 1, max)
print(ma)

s <- apply(gem2, 2, mean)
print(s)

# 2) lapply.

print(iris)
iris_list <- as.list(iris)
iris_list
length(iris_list)

iris2 <- as.list(iris[, -5])
iris2

class(iris2)

g <- lapply(iris2, mean)

class(g)

# lapply(gem2, mean) # warning!

# 3) sapply. 

g1 <- sapply(iris2, mean)

class(g1)

g1

### End of Step-04.
### **************************************************************************** 

### ****************************************************************************
### Step-05. Parallel Computing. . 

# 1) Using Reduce function.

#. add <- function(x) Reduce("+", x)
#. add(list(1, 2, 3))
#. 
#. add_accuml <- function(x) Reduce("+", x, accumulate = TRUE)
#. add_accuml(list(1, 2, 3))
#. 
#. cumsum(list(1, 2, 3))

# 2) Using parallel. 

library(parallel) # 载入parallel包

# 计算可用线程数，并设置并行使用线程数

no_cores <- detectCores() - 1

# 初始化
cl <- makeCluster(no_cores)

# 修改原本我们lapply()的命令：

gem3 <- as.list(gem1)

length(gem3)

parLapply(cl, gem3, mean)

stopCluster(cl)

system.time(lapply(gem3, mean))

# 3) Rmpi

# 加载 R 包
library(Rmpi)
# 检测可用的逻辑 CPU 核心数
parallel::detectCores()
# 虚拟机分配四个逻辑CPU核 
# 1个 master 2个 worker 主机 cloud
mpi.spawn.Rslaves(nslaves = 2)

# 调用 mpi.apply 函数
set.seed(1234)
mpi.apply(c(10, 20), runif)

# 用完要关闭
mpi.close.Rslaves()

### End of Step-05.
### ****************************************************************************

################################################################################
### End of chunk-08.
################################################################################
