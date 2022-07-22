
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

dir.create("D:/testLRC")

setwd("D:/testLRC")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. The examples for calculating the statistics. 

# 1) the first one

# ---------------------- (1) -------------------------
w <- function(x) {
  d <- 0
  for (i in 1:length(x)) {
    tmp <- abs(x[i] - median(x))
    d <- sum(d, tmp)
  }
  d/length(x)
}

x1 <- 1:10
w(x1)

# ---------------------- (2) -------------------------

f1 <- function(x) {
  n <- length(x)
  mhat <- median(x)
  s <- 0.0
  for(i in 1:n){
    s <- s + abs(x[i] - mhat)
  }
  s <- s/n
  return(s)
}

f1(x1)

# ---------------------- (3) -------------------------

x1 <- 1:10
m <- median(x1) 
mean(abs(x1 - m))

cal_W <- function(x) mean(abs(x-mean(x))) # median instead of mean

cal_W(1:10)

# compare the run-time using system.time(). 

nrep <- 1000
x <- runif(10000)
y1 <- numeric(nrep); y2 <- y1
system.time(for(i in 1:nrep) y1[i] <- f1(x) )[3]

system.time(for(i in 1:nrep) y1[i] <- f2(x) )[3]


# compare the run-time using microbenchmark(). 

x <- runif(10000)
microbenchmark::microbenchmark(
  f1(x),
  f2(x)
)


# 2) the second one

f1 <- function(x){
  n <- length(x)
  y <- numeric(n)
  
  for(i in seq_along(x)){
    if(x[i] >= 0) y[i] <- 1
    else y[i] <- 0
  }
  
  y
}

# another one?

# 3) the third one

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. The examples for drawing the pictures. 

# 1) the first one

# 2) the second one

# 3) the third one

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. The examples for downloading the gene expression data sets. 

# 1) the first one

# downloading the R software. 



# 2) the second one

# 3) the third one

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. The examples for crawling the data sets from internet pages.   

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

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-06. The examples for other purposes. 

# 1) the first form


# z = x^2 + y^3 which return z value when input x and y values. 

# 2) the second form

# 4) using loop in plot

# using loop. 


# using vector

### End of Step-04.
### ****************************************************************************

################################################################################
### End of chunk-12.
################################################################################
