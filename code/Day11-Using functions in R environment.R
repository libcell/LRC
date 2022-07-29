
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 21th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 11: Using functions in R.
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
# y = x^2 + 1, which return y value when input x

ls()

y <- function(x) {x^2 + 1}

ls()

print(y)

y(x = 2)
y(x = 3)
y(2); y(.2)

# z = x^2 + y^3 which return z value when input x and y values. 

z.value <- function(x, y) {
  x^2 + y^3
}

z.value(x = 1, y = 2)
z.value(1, 2)

z.value1 <- function(x, y) {
  d <- x^2 + y^3
  return(d)
}

z.value1(1, 2)

compute_zvalue <- function(x1, z) {
  e <- x1^2 + z^3
  return(e)
}

print(e)
print(x1)
print(z)

compute_zvalue(1, 2)

compute_zvalue(x1 = iris, z = iris)


z <- function(x, y) {
  d <- x^2 + y^3
}

z(x=1, y=2)

z <- function(x, y) {
  d <- x^.5 + y^3
  return(d)
}

f <- z(x=1, y=2)
print(f)

z(x=-1, y=2)


z <- function(x, y) {
  if (x >= 0) {
    d <- x^.5 + y^3
    return(d)
  } else 
    warning("x must be not negtive!")
}

z(1, 2)
z(-1, 2)

# 2) the second form

# 3) using loop in plot

# using loop. 

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
### Step-03. Case study-1. 

# 1) repeat 循环会一直执行代码，直到条件语句为 true 时才退出循环，退出要使用到 break 语句：

# 3) for example

# using function

# input:
#   x - the first matrix. 
#   y - the second matrix. 
# output: 
#  the result that ...

POM <- function(x, y)
{
  m1 <- ncol(x)
  n <- nrow(y)
  if (m1 != n)
  {
    print('error dimension is not siutable')
    return(0)
  }
  m <- nrow(x)
  n1 <- ncol(y)
  s <-matrix(0, m, n1)
  for(i in 1:m)
    for(j in 1:n1)
      s[i, j] <- sum(x[i, ] * y[, j])
  return(s)
}

x <- matrix(c(1:6), 2, 3, byrow = TRUE)
y <- matrix(c(1:6), 3, 2, byrow = FALSE)

res <- POM(x = x, y = y)
print(res)

POM(x = 1, y = 2)

POM(x = 1:10, y = 1:5)

dim(1:10)
dim(1:5)

# improved POM. 

iPOM <- function(x, y) {
  
  # if (!is.matrix(x)) stop("Please check the first input!")
  # if (!is.matrix(y)) stop("Please check the second input!")
  if (!is.matrix(x) | !is.matrix(y)) 
    stop("Please check the type of input data!")
  
  m1 <- ncol(x)
  n <- nrow(y)
  if (m1 != n)
  {
    print('error dimension is not siutable')
    return(0)
  }
  m <- nrow(x)
  n1 <- ncol(y)
  s <-matrix(0, m, n1)
  for(i in 1:m)
    for(j in 1:n1)
      s[i, j] <- sum(x[i, ] * y[, j])
  return(s)
  
}

iPOM(x, 1:5)
iPOM(1:5, 1:10)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. Case study.   

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

### End of Step-04.
### ****************************************************************************

################################################################################
### End of chunk-09.
################################################################################
