
################################################################################
#    &&&....&&&    % Lecture-02: Objects in R Environment                      #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 06th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 02: Understanding the data objects and their structure.
################################################################################

### ****************************************************************************
### Step-01. Obtain the gene ontology. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

# 1) Vector


library(XML)
library(RCurl)

url <- "http://www.knapsackfamily.com/knapsack_core/information.php?word=C00000001"

