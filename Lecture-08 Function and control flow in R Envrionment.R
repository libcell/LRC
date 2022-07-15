
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
norm

rnorm(n = 10, mean = 0, sd = 1)
qnorm(p = (1:5)/10, mean = 0, sd = 1)
pnorm((1:5)/10)
dnorm((1:5)/10)

plot(dnorm((1:5)/10))

### End of Step-02.
### ****************************************************************************






### ****************************************************************************
### Step-02. Common functions in R. 

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

setwd(pri.dir)

################################################################################
### End of chunk-01.
################################################################################
