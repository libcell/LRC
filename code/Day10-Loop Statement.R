
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 20th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 10: Loop structure.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. for... in R. 

# R 编程语言中 for 循环语句可以重复执行指定语句，重复次数可在 for 语句中控制。

# 1) the first form

v <- LETTERS[1:4]

for (i in v) {
  
  print("+++++++++++++++++++++ start ++++++++++++++++++++++++++++")
  
  print(i)
  
  print("+++++++++++++++++++++++ end ++++++++++++++++++++++++++++")
  
  Sys.sleep(5)
  
}

# 2) the second form

v <- LETTERS[1:4]

for (i in 1:length(v)) {
  
  print(v[i])
  
}

# 3) for example
# generating the y which has the same length with x.

x <- rnorm(10)
y <- ifelse(x > 0, 1, 0)

# using loop

y1 <- NULL

for (i in 1:length(x)) {
  
  if (x[i] > 0) 
    
    y1[i] <- 1 
  
  else 
    
    y1[i] <- 0
  
}

print(y1)

print(y)

all(y == y1)

y == y1

table(y == y1)

# 4) using loop in plot

# using loop. 

curve((x ^ 3 + x ^ 2 + x + 1),
      -10,
      10, 
      ylim = c(-2000, 2000), 
      lwd = 2, 
      col = "lightblue", 
      bty = "l", # line
      xlab = "The value of x",
      ylab = "The value of y")

h.value <- seq(-2000, 2000, by = 200)

for (i in 1:length(h.value)) {
  
  abline(h = h.value[i], 
         lty = 2, 
         col = rainbow(length(h.value))[i])
  
  Sys.sleep(2)
  
}

# using vector

curve((x ^ 3 + x ^ 2 + x + 1),
      -10,
      10, 
      ylim = c(-2000, 2000), 
      lwd = 2, 
      col = "lightblue", 
      bty = "l", # line
      xlab = "The value of x",
      ylab = "The value of y")

h.value <- seq(-2000, 2000, by = 200)

abline(h = h.value, lty = 2, col = rainbow(length(h.value)))

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. repeat in R. 

# 1) repeat 循环会一直执行代码，直到条件语句为 true 时才退出循环，退出要使用到 break 语句：

#. 语法格式如下：
#. repeat { 
#.   // 相关代码 
#.   if(condition) {
#.     break
#.   }
#. }
# 2) 其中的“条件”为一个标量的真或假值, 不允许取缺失值， 如

v <- c("Google","Runoob")

cnt <- 2

repeat {
  
  # part-1
  print(v)
  
  # part-2
  cnt <- cnt + 1
  
  # part-3
  if (cnt > 5) {
    break
  }
}

print(cnt)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. while ... in R.   

# 1) 只要给定的条件为 true，R 语言中的 while 循环语句会重复执行一个目标语句。

#. while(condition)
#. {
#.   statement(s);
#. }

# 2) example on while. 

v <- c("Google","Runoob")
cnt <- 2
while (cnt < 7) {
  print("start-------------------------------------------------")
  text <- paste("cnt", cnt, sep = "=")
  print(text)
  print(v)
  cnt = cnt + 1
  print("end---------------------------------------------------")
  Sys.sleep(2)
}

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. Function: controling loops in R.   

# 1) break in R

v <- c("Google","Runoob")
cnt <- 2

repeat {
  print(v)
  cnt <- cnt+1
  
  if(cnt > 5) {
    break
  }
}

# 2) next in R. 

v <- LETTERS[1:6]

for (i in v) {
  
  if (i != "D") {  # D 不会输出，跳过这次循环，进入下一次
    next
  }
  
  print(i)
  
}


# 用循环打印1-100这100个整数中的偶数，分别用for, while和repeat来实现。
v <- 1:100

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. Case study.   

# 1) Reading the XML files. 

ids <- c("C00000001", "C00000002", "C00000003", "C00000004", "C00000005", 
         "C00000006", "C00000007", "C00000008", "C00000009", "C00000010")

library(XML)
library(RCurl)
url <- "http://www.knapsackfamily.com/knapsack_core/information.php?sname=C_ID&word=C00000001"
content <- getURL(url, .encoding = character())
tables <- readHTMLTable(content)
length(tables)
class(tables)
tables[[1]]
tables[[2]]
metaDB <- tables[[3]]

dim(metaDB)

paste("I", "am", "a", "student", sep = "")

### End of Step-05.
### ****************************************************************************

################################################################################
### End of chunk-09.
################################################################################
