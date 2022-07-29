
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
### Step-02. Preparing the raw data set for GSE470. 

# ---------------------- (1) downloading the data set ------------------------ #

# ---------------------- (2) data pre-processing ----------------------------- #

library(affy)

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Checking the data quality. 

# 1) the first histogram

# Create data for the graph.
v <- c(19, 23, 11, 5, 16, 21, 32, 14, 19, 27, 39)

# Create the histogram.
hist(v, 
     xlab = "No.of Articles", 
     col = "green",
     border = "black", 
     xlim = c(0, 50),
     ylim = c(0, 5), 
     breaks = 10)

hist(v, 
     xlab = "No.of Articles", 
     col = 1:7,
     border = "black", 
     xlim = c(0, 50),
     ylim = c(0, 5), 
     breaks = 10)

# 2) the second one

# Creating data for the graph.
v <- c(19, 23, 11, 5, 16, 21, 32, 14, 19,
       27, 39, 120, 40, 70, 90)

# Creating the histogram.
m <- hist(
  v,
  xlab = "Weight",
  ylab = "Frequency",
  col = "darkmagenta",
  ylim = c(0, 6.5), 
  border = "pink",
  breaks = 5
)

class(m)
print(m)
# plot(m)

# Setting labels
text(m$mids, m$counts, labels = m$counts,
     adj = c(0.5, -0.5))

# 3) the third one

# Creating data for the graph.
v <- c(19, 23, 11, 5, 16, 21, 32, 14,
       19, 27, 39, 120, 40, 70, 90)

# Creating the histogram.
hist(
  v,
  xlab = "Weight",
  ylab = "Frequency",
  xlim = c(0, 150),
  col = "darkmagenta",
  border = "pink",
  breaks = c(5, 55, 60, 70, 75,
             80, 100, 140)
)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. The bar chart. 

# 1) the first one

# Create the data for the chart
A <- c(17, 32, 8, 53, 1)

# Plot the bar chart
barplot(A,
        xlab = "X-axis",
        ylab = "Y-axis",
        main = "Bar-Chart")

# 2) the second one

# Create the data for the chart
A <- c(17, 32, 8, 53, 1)

# Plot the bar chart
barplot(
  A,
  horiz = TRUE,
  xlab = "X-axis",
  ylab = "Y-axis",
  main = "Bar-Chart"
)

# 3) the third one

# Create the data for the chart
A <- c(17, 2, 8, 13, 1, 22)
B <- c("Jan", "feb", "Mar", "Apr", "May", "Jun")

# Plot the bar chart
barplot(
  A,
  names.arg = B,
  xlab = "Month",
  ylab = "Articles",
  col = "green",
  main = "GeeksforGeeks-Article chart"
)

# 4) the fourth one

colors <- c("green", "orange", "brown")
months <- c("Mar", "Apr", "May", "Jun", "Jul")
regions <- c("East", "West", "North")

# Create the matrix of the values.
Values <- matrix(c(2, 9, 3, 11, 9, 4, 8, 7, 3, 12, 5, 2, 8, 10, 11),
                 nrow = 3, ncol = 5, byrow = TRUE)

# Create the bar chart
length(Values)
barplot(
  Values,
  main = "Total Revenue",
  names.arg = months,
  xlab = "Month",
  ylab = "Revenue",
  col = colors,
  beside = TRUE
)

# Add the legend to the chart
legend("topleft", regions, cex = 0.7, fill = colors)

# the fifth one

colors = c("green", "orange", "brown")
months <- c("Mar", "Apr", "May", "Jun", "Jul")
regions <- c("East", "West", "North")

# Create the matrix of the values.
Values <- matrix(
  c(2, 9, 3, 11, 9, 4, 8, 7, 3, 12, 5, 2, 8, 10, 11),
  nrow = 3,
  ncol = 5,
  byrow = TRUE
)

# Create the bar chart
barplot(
  Values,
  main = "Total Revenue",
  names.arg = months,
  xlab = "Month",
  ylab = "Revenue",
  col = colors
)

# Add the legend to the chart
legend("topleft", regions, cex = 0.7, fill = colors)

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. The pie diagram.   

# 1) the first one.

# Create data for the graph.
geeks<- c(23, 56, 20, 63)
labels <- c("Mumbai", "Pune", "Chennai", "Bangalore")

# Plot the chart.
pie(geeks, labels)

# 2) the second one.

# Create data for the graph.
geeks<- c(23, 56, 20, 63)
labels <- c("Mumbai", "Pune", "Chennai", "Bangalore")

# Plot the chart with title and rainbow
# color pallet.
pie(geeks, 
    labels, 
    main = "City pie chart",
    col = rainbow(length(geeks)))

# 3) the third one

# Create data for the graph.
geeks <- c(23, 56, 20, 63)
labels <- c("Mumbai", "Pune", "Chennai", "Bangalore")

piepercent <- round(100 * geeks / sum(geeks), 1)

# Plot the chart.
pie(geeks,
    labels = piepercent,
    main = "City pie chart",
    col = rainbow(length(geeks)))
legend(
  "topright",
  c("Mumbai", "Pune", "Chennai", "Bangalore"),
  cex = 0.5,
  fill = rainbow(length(geeks))
)

# 4) the fourth one

# Get the library.
# install.packages("plotrix")

library(plotrix)

# Create data for the graph.
geeks <- c(23, 56, 20, 63)
labels <- c("Mumbai", "Pune", "Chennai", "Bangalore")

piepercent <- round(100 * geeks / sum(geeks), 1)

# Plot the chart.
pie3D(geeks, 
      labels = piepercent,
      main = "City pie chart", 
      col = rainbow(length(geeks)))
legend("topright", 
       c("Mumbai", "Pune", "Chennai", "Bangalore"),
       cex = 0.5, 
       fill = rainbow(length(geeks)))


labels1 <- paste0(labels, "\n", piepercent, "%")

barplot(piepercent, 
        names.arg = labels1, 
        col = rainbow(length(geeks)))

### End of Step-05.
### ****************************************************************************

### ****************************************************************************
### Step-06. The layout of diagrams. 

# 1) using notch parameter. 
iris

boxplot(iris[1:4], col =2:5)

boxplot(Sepal.Length ~ Species, data = iris, col = 2:5)

boxplot(Sepal.Length ~ Species, 
        data = iris, 
        col = 2:5, 
        notch = TRUE)

# 2) using the function par

op <- par(mfrow = c(2, 2))

boxplot(iris[1:4], col = 2:5) # 1

boxplot(Sepal.Length ~ Species, data = iris, col = 2:5) # 2

boxplot(Sepal.Length ~ Species, 
        data = iris, 
        col = 2:5, 
        notch = TRUE) # 3

par(op) # or, using: par(no.readonly = TRUE)

### End of Step-06.
### ****************************************************************************

################################################################################
### End of chunk-12.
################################################################################
