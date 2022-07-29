
################################################################################
#    &&&....&&&    % Lecture-02: Objects in R Environment                      #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 08th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 04: Understanding the data objects and their structure.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. About data frame in R. 

# 1) Generating data frame. 

# Create the data frame.
emp.data <- data.frame(
  emp_id = c(1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3, 515.2, 611.0, 729.0, 843.25), 
  
  start_date = as.Date(c("2012-01-01", 
                         "2013-09-23", 
                         "2014-11-15", 
                         "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)

# Print the data frame.			
print(emp.data) 

# Get the Structure of the Data Frame
str(emp.data)

# Print the summary.
summary(emp.data)

# 2) Accessing the data frame. 

# Extract Specific columns.
result <- data.frame(emp.data$emp_name, 
                     emp.data$salary)
print(result)

df <- data.frame(A = emp.data$emp_name, 
                 B = emp.data$salary)

df

# Extract first two rows.
result <- emp.data[1:2, ]
print(result)

# Extract 3rd and 5th row with 2nd and 4th column.
result <- emp.data[c(3, 5), c(2, 4)]
print(result)

class(result)

# typeof(result)

# 3) Edit the data frame. 

# Add the "dept" coulmn.
emp.data$dept <- c("IT", "Operations", "IT", "HR", "Finance")
v <- emp.data
print(v)

# Create the first data frame.
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  dept = c("IT","Operations","IT","HR","Finance"),
  stringsAsFactors = FALSE
)

# Create the second data frame
emp.newdata <- 	data.frame(
  emp_id = c (6:8), 
  emp_name = c("Rasmi","Pranab","Tusar"),
  salary = c(578.0,722.5,632.8), 
  start_date = as.Date(c("2013-05-21","2013-07-30","2014-06-17")),
  dept = c("IT","Operations","Fianance"),
  stringsAsFactors = FALSE
)

# Bind the two data frames.
emp.finaldata <- rbind(emp.data, emp.newdata)
print(emp.finaldata)

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. About list in R. 

# 1) Generating list. 

# Create the list.

# Create a list containing strings, numbers, vectors and a logical
# values.
list_data <- list("Red", 
                  "Green", 
                  c(21,32,11), 
                  TRUE, 
                  51.23, 
                  119.1)
print(list_data)

# 2) Naming List Elements.  

# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan", "Feb", "Mar"),
                  matrix(c(3, 9, 5, 1, -2, 8), nrow = 2),
                  list("green", 12.3))

# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")

# Show the list.
print(list_data)

# 3) Accessing List Elements. 

# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))

# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")

# Access the first element of the list.
print(list_data[1])

x <- list_data[1]

class(x)

print(list_data[[1]])

y <- list_data[[1]]

class(y)

# Access the thrid element. As it is also a list, all its elements will be printed.
print(list_data[3])

# Access the list element using the name of the element.
print(list_data$A_Matrix)

# 4) Manipulating List Elements. 

# Create a list containing a vector, a matrix and a list.
list_data <- list(c("Jan","Feb","Mar"), matrix(c(3,9,5,1,-2,8), nrow = 2),
                  list("green",12.3))

# Give names to the elements in the list.
names(list_data) <- c("1st Quarter", "A_Matrix", "A Inner list")


length(list_data)


list_data$`1st Quarter`[3] <- "May"

print(list_data)


# Add element at the end of the list.
list_data[4] <- "New element"
print(list_data[4])

list_data

names(list_data)[4] <- "New"

list_data

# Remove the last element.
list_data[4] <- NULL

list_data

length(list_data)

# Print the 4th Element.
print(list_data[4])

# Update the 3rd Element.
list_data[3] <- "updated element"
print(list_data[3])

list_data

# 5) Merging Lists. 

# Create two lists.
list1 <- list(1, 2, 3)

list2 <- list("Sun", "Mon", "Tue")

# Merge the two lists.
merged.list <- c(list1, list2)

# Print the merged list.
print(merged.list)

class(merged.list)

# 6) Converting List to Vector. 

# Create lists.
list1 <- list(1:5)
print(list1)

list2 <-list(10:14)
print(list2)

class(list1)

# Convert the lists to vectors.
v1 <- unlist(list1)
v2 <- unlist(list2)

print(v1)
print(v2)

# Now add the vectors
result <- v1+v2
print(result)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. Read and write files in R. 

# 1) Generating list. 

# Create the list.

dat <- read.delim("clipboard", header = TRUE)

print(dat)


score <- read.csv("../data/L2_simulated_student_score.csv", header = TRUE)

dim(score)

head(score)
tail(score)

print(score)

write.csv(score, file = "F:/new_score.csv")

# read.table(file = choose(), header = TRUE)

### End of Step-04.
### ****************************************************************************

################################################################################
### End of chunk-04.
################################################################################
