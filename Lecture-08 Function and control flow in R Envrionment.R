
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
### Step-02. Common functions in R. 

# 1) Simulating a GEM showing expression levels of 2000 genes in 100 samples. 

# 随机生成200000个数值，这些数值来自均值为8标准差为2的正态分布


# 2) Naming the data distribution.  


### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Family of apply functions. 

# 1) apply. 


# 2) lapply.


# 3) sapply. 


### End of Step-03.
### **************************************************************************** 

### ****************************************************************************
### Step-04. Parallel Computing. . 

# 1) Using Reduce function.

add <- function(x) Reduce("+", x)
add(list(1, 2, 3))

add_accuml <- function(x) Reduce("+", x, accumulate = TRUE)
add_accuml(list(1, 2, 3))

cumsum(list(1, 2, 3))

# 2) Using parallel. 

library(parallel)

parallel::detectCores()

lapply(1:3, function(x) c(x, x ^ 2, x ^ 3))


library(parallel) # 载入parallel包

# 计算可用线程数，并设置并行使用线程数
no_cores <- detectCores() - 1

# 初始化
cl <- makeCluster(no_cores)

# 修改原本我们lapply()的命令：
parLapply(cl, 1:3, function(x) c(x, x ^ 2, x ^ 3))


stopCluster(cl)


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

### End of Step-04.
### ****************************************************************************

################################################################################
### End of chunk-01.
################################################################################
