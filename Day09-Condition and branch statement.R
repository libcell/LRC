
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
### code chunk number 09: Condition and branch statement in R Language.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. Expression in R. 

# 任何一个语句都可以看成是一个表达式。 
# 表达式之间以分号分隔或用换行分隔。

# 1) the first form

{x <- 15; x}

# 2) the second form

{
  x <- 15
  x
}

# 3) the third form

{
  x <- 15
  print(x) # not return!!!
}


# 3) using expression in plot

curve((x^3 + x^2 + x + 1), -10, 10, bty="l", xlab="x1", ylab="y")
abline(h=0, v=0, lty=2, col="gray")
text(0.5, -0.5, "0", col="gray")
text(8, 10, "y = x")

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. branch structure in R. 

# 1) 分支结构包括if结构：

# 比如：
# if (条件) 表达式1
# 或
# if (条件) 表达式1 else 表达式2

# 2) 其中的“条件”为一个标量的真或假值, 不允许取缺失值， 如

if(is.na(lambda)) lambda <- 0.5

# 3) Examples: 

# A: Eg1. 

if (x>1) {
  y <- 2.5
} else {
  y <- -y
}

# B: Eq2. 

x <- c(0.05, 0.6, 0.3, 0.9)
for (i in seq(along = x)) {
  if (x[i] <= 0.2) {
    cat("Small\n")
  } else if (x[i] <= 0.8) {
    cat("Medium\n")
  } else {
    cat("Large\n")
  }
}

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. Replacing branch structures with logical subscripts.   

# 1) for a element in R. 

if (x > 0) y <- 1 else y <- 0

# 2) for a vector in R. 

# x为一个向量, 要定义y与x等长, 且y的每一个元素当且仅当x的对应元素为正数时等于1, 否则等于零。

y <- numeric(length(x))
y[x > 0] <- 1
y

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. Function: if and ifelse in R.   

# 1) function if in R

x <- 50L
if (is.integer(x)) {
  print("X 是一个整数")
}

# 2) if...else in R. 

# A: Eg-1. 

x <- c("google","runoob","taobao")

if("runoob" %in% x) {
  print("包含 runoob")
} else {
  print("不包含 runoob")
}

# B: Eg-2. 

x <- c("google","runoob","taobao")

if("weibo" %in% x) {
  print("第一个 if 包含 weibo")
} else if ("runoob" %in% x) {
  print("第二个 if 包含 runoob")
} else {
  print("没有找到")
}

### End of Step-05.
### ****************************************************************************

### ****************************************************************************
### Step-06. Function switch in R. 

# 1) switch 语句允许测试一个变量等于多个值时的情况。每个值称为一个 case。
# switch(expression, case1, case2, case3....)

x <- switch(
  3,
  "google",
  "runoob",
  "taobao",
  "weibo"
)
print(x)

# 2) 如果是字符串返回字符串变量对应的值. 

# A: the first way

you.like <- "runoob"
switch(you.like,
       google = "www.google.com",
       runoob = "www.runoob.com",
       taobao = "www.taobao.com")

# B: the second way

x <- switch(
  A,
  A = "google",
  B = "runoob",
  C = "taobao",
  D = "weibo"
)
print(x)

# 3) 如果整数不在范围内的则返回 NULL

x <- switch(4,"google","runoob","taobao")
x
NULL
x <- switch(4,"google","runoob","taobao")
x
NULL
### End of Step-04.
### **************************************************************************** 

################################################################################
### End of chunk-08.
################################################################################
