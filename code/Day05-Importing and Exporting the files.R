
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 12th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 05: Importing and Exporting the files.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. Reading and writing CSV format files. 

# 1) Reading the CSV files. 

x <- read.csv("iris.csv", header = TRUE)
print(x)

# 2) Writing the CSV files. 

dir()

# write.csv(iris, "iris.csv")
write.csv(x = iris, file = "iris.csv")

dir()

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Reading and writing TXT format files. 

# 1) Reading the TXT files. 

y <- read.table("iris.csv", sep = ",", header = TRUE)

print(y)

# 2) Writing the TXT files. 

write.table(y, "newiris.txt")

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. Reading and writing xlsx format files. 

library(openxlsx)

# 1) Reading the xlsx files. 

z <- read.xlsx("newiris2.xlsx", sheet = 1)

head(z)

# 2) Writing the xlsx files. 

write.xlsx(iris, file = "newiris2.xlsx")

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. Reading and writing XML format files. 

library(RCurl)
library(XML)

# 1) Reading the XML files. 

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

# 2) Writing the XML files. 

### End of Step-05.
### ****************************************************************************

### ****************************************************************************
### Step-06. Reading and writing json format files. 

# 1) Reading the json files. 
install.packages("rjson")
library(rjson)
content <- fromJSON("test.json")

save(iris, "iris.RData")
save(iris, file = "iris.RData")

content <- fromJSON(file = "test.json")


#-------------- testing this error!-------------------------------

write(content, file = "newdata.json") # !!! 

# 2) Writing the json files. 

### End of Step-06.
### ****************************************************************************

################################################################################
### End of chunk-01.
################################################################################
