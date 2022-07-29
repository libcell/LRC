
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 13th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 06: Description and Statistics of Data.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. mean(), median(). 

# 1) Mean. 

x <- c(1:10)

av <- mean(x)

print(av)

x[3] <- NA

print(x)

av1 <- mean(x, na.rm = TRUE)

print(av1)

?mean # remove

# 2) Median. 

mv <- median(x)

print(mv)

# 3) fivenum

x <- 1:10

fivenum(x)

summary(x)

# 4) var, sd

var(x)

x1 <- var(x)

x1^0.5

sd(x)

# 5) data distribution

x <- 1:1000

hist(x)

x1 <- rnorm(1000, mean = 5, sd = 1)

mean(x1); sd(x1); var(x1)

hist(x1, breaks = 100) # 直方图

hist(x1, breaks = 30, probability = TRUE)

lines(density(x1), col = "red", lwd = 3)

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. For example. 

# 1) Installing the related R package. 

install.packages("fBasics")

# 2) Computing the statistics.

# 3) Computing other incicates.

x1

quantile(x1)
median(x1)

quantile(x1)[3] == median(x1)

boxplot(x1) # 触须

quantile(x1, 0.75) - quantile(x1, 0.25) # !!!

IQR(x1)

range(x1)

# 

e <- c(2.1, 2.0, 1.9, 4.5, 2.2, 2.3, 1.89, 23, 2.0, 2.01)

te1 <- mean(e)

te2 <- median(e)

te3 <- mean(e[c(-4, -8)])

print(te3)

boxplot(e)


### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. Reading and writing xlsx format files. 

# 1) Reading the xlsx files. 


# 2) Writing the xlsx files. 


### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. Reading and writing XML format files. 

# 1) Reading the XML files. 


# 2) Writing the XML files. 

### End of Step-05.
### ****************************************************************************

### ****************************************************************************
### Step-06. Reading and writing json format files. 

# 1) Reading the json files. 


# 2) Writing the json files. 

### End of Step-06.
### ****************************************************************************

################################################################################
### End of chunk-01.
################################################################################
