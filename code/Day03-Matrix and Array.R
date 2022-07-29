
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 07th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 03: Understanding the data objects and their structure.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. About objects in R. 

# 1) object and its attribute. 
# 2) object and its type.

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. About vector in R. 

# 1) Generating vectors. 
# 2) vector

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. About matrix in R. 

# 1) Generating matrix.  
# 2) Accessing the matrix. 

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. About matrix in R. 

# 1) Generating matrix. 

# Elements are arranged sequentially by row.
M <- matrix(c(3:14), nrow = 4, byrow = TRUE)
print(M)

# Elements are arranged sequentially by column.
N <- matrix(c(3:14), nrow = 4, byrow = FALSE)
print(N)

# Define the column and row names.
rownames  <- c("row1", "row2", "row3", "row4")
colnames  <- c("col1", "col2", "col3")

rownames(N) <- rownames
colnames(N) <- colnames

print(N)

P <- matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(rownames, colnames))
print(P)

# 2) Accessing the matrix. 

# Define the column and row names.
rownames = c("row1", "row2", "row3", "row4")
colnames = c("col1", "col2", "col3")

# Create the matrix.
P <- matrix(c(3:14), nrow = 4, byrow = TRUE, dimnames = list(rownames, colnames))

# Access the element at 3rd column and 1st row.
print(P[1,3])

# Access the element at 2nd column and 4th row.
print(P[4,2])

# Access only the  2nd row.
print(P[2,])

# Access only the 3rd column.
print(P[,3])

### End of Step-05.
### ****************************************************************************

### ****************************************************************************
### Step-06. About array in R. 

# 1) Generating array.  

# Create two vectors of different lengths.
vector1 <- c(5, 9, 3)
vector2 <- c(10, 11, 12, 13, 14, 15)

# Take these vectors as input to the array.
result <- array(c(vector1, vector2), dim = c(3, 3, 2))
print(result)

# 2) Naming Columns and Rows. 

# Create two vectors of different lengths.
vector1 <- c(5, 9, 3)
vector2 <- c(10, 11, 12, 13, 14, 15)
column.names <- c("COL1", "COL2", "COL3")
row.names <- c("ROW1", "ROW2", "ROW3")
matrix.names <- c("Matrix1", "Matrix2")

# Take these vectors as input to the array.
result <- array(c(vector1,vector2),dim = c(3,3,2),
                dimnames = list(row.names,
                                column.names, 
                                matrix.names))
print(result)

# ====================================================

x <- matrix(1:100, nrow = 10, byrow = TRUE)

print(x)

rownames(x) <- paste("row", 1:10, sep = "-")

colnames(x) <- paste("col", 1:10, sep = "=")

print(x)


# 3) Accessing array elements. 

print(result)

result[1, 1, 1]

result[1, 1, 2]


# Create two vectors of different lengths.
vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
column.names <- c("COL1","COL2","COL3")
row.names <- c("ROW1","ROW2","ROW3")
matrix.names <- c("Matrix1","Matrix2")

# Take these vectors as input to the array.
result <-
  array(
    c(vector1, vector2),
    dim = c(3, 3, 2),
    dimnames = list(row.names,
                    column.names, matrix.names)
  )

# Print the third row of the second matrix of the array.
print(result[3,,2])

# Print the element in the 1st row and 3rd column of the 1st matrix.
print(result[1,3,1])

# Print the 2nd Matrix.
print(result[,,2])

## calculate

x

# 行平均值

mean(x[1, ])
mean(x[2, ])
mean(x[3, ])

help("apply")

apply(x, 1, mean)

apply(x, 2, mean)

mean(x[, 1])
mean(x[, 2])
mean(x[, 3])
mean(x[, 4])
mean(x[, 5])
mean(x[, 6])

apply(x, 1, max)
apply(x, 1, min)
apply(x, 1, var)


# 4) Image and array. 

# install EBImage package

install.packages("BiocManager")

library(BiocManager)

BiocManager::install("EBImage")

library(EBImage)

getwd()

img <- readImage("D:/00-GitHub/LRC/data/cqnu-logo.jpg")

print(img)

display(img)

hist(img)

str(img)

img@.Data[, , 1][img@.Data[, , 1] < 0.5] <- 1

img <- readImage("D:/cqnu-logo.jpg")

img@.Data[, , 2][img@.Data[, , 2] < 0.5] <- 1

img@.Data[1:100, 1:100, 1] <- 0
img@.Data[1:100, 1:100, 2] <- 0.5
img@.Data[1:100, 1:100, 3] <- 0.8

display(img)

### End of Step-06.
### ****************************************************************************

################################################################################
### End of chunk-03.
################################################################################
