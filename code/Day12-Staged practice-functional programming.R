
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
### code chunk number 11: Staged practice one: functional programming.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. simple function. 

# 1) the first form


# z = x^2 + y^3 which return z value when input x and y values. 

# 2) the second form

# 4) using loop in plot

# using loop. 


# using vector

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Case study-1. 

# 1) repeat 循环会一直执行代码，直到条件语句为 true 时才退出循环，退出要使用到 break 语句：

# 3) for example

# using function

# input:
#   x - the first matrix. 
#   y - the second matrix. 
# output: 
#  the result that ...

# improved POM. 

### End of Step-03.
### ****************************************************************************

# z = f(x,y) = x^2 + y^2

# a function which returns a barplot. 

### ****************************************************************************
### Step-05. Case study.   

# 1) Reading the XML files. 

getCompound <- function(id = "C00000001") {
# please write the content of function here!!!
}

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

################################################################################
### End of chunk-12.
################################################################################
