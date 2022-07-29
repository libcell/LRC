
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 25th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 13: Discussion and Revisiting the previous contents in R.
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

# z = f(x,y) = x^2 + y^2

computeZvalue <- function(x, y) {
  d <- x^2 + y^2
  return(d)
}

computeZvalue(1, 2)

computeZvalue <- function(x = 1, y) {
  d <- x^2 + y^2
  return(d)
}

computeZvalue(1, 2)

computeZvalue(y=2)

computeZvalue <- function(x = 1, y = 2) {
  d <- x^2 + y^2
  return(d)
}

res <- computeZvalue(5, 10)
res


addPoint <- function(x = 0, y = 0) {
  x1 <- seq(-2*pi, 2*pi, by = pi/20)
  y1 <- sin(x1)
  plot(x1, y1)
  points(x, y, pch = 15, col = "red")
  abline(h = y, col = "red")
  abline(v = x, col = "red")
}

addPoint(-3, .5)


addElements <- function(x = 0, y = 0, yz = "red") {
  x1 <- seq(-2*pi, 2*pi, by = pi/20)
  y1 <- sin(x1)
  plot(x1, y1)
  points(x, y, pch = 15, col = yz)
  abline(h = y, col = yz)
  abline(v = x, col = yz)
}

addElements(-3, .5)
addElements(-3, .5, "blue")
addElements(x = -3, y = .5, yz = "blue")

# a function which returns a barplot. 

drawBarplot <- function(x) {
  if (!is.numeric(x)) 
    stop("input x must be a numeric vector!")
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  barplot(x, col = rainbow(length(x)))
}

drawBarplot(x = 1:10)
drawBarplot(letters)

### End of Step-02.
### ****************************************************************************

################################################################################
### End of chunk-09.
################################################################################
