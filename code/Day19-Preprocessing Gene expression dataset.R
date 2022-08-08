
################################################################################
#    &&&....&&&    % Learning R Course in Summer of 2022                       #
#  &&&&&&..&&&&&&  % Teacher: Bo Li, Mingwei Liu                               #
#  &&&&&&&&&&&&&&  % Date: Jul. 28th, 2022                                     #
#   &&&&&&&&&&&&   %                                                           #
#     &&&&&&&&     % Environment: R version 4.1.1;                             #
#       &&&&       % Platform: x86_64-w64-mingw32/x64 (64-bit)                 #
#        &         %                                                           #
################################################################################

################################################################################
### code chunk number 17: Primary drawing in R.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# 1) For windows, work directory. 

pri.dir <- getwd()

if (!dir.exists("asthma")) dir.create("asthma")

setwd("asthma")

# 2) Installing the packages

pkgs <- c("GEOquery", # downloading the data set. 
          "affy", # processing DNA microarray from Affymetrix. 
          "arrayQualityMetrics") # for QC of microarray.

if (!require("BiocManager")) install.packages("BiocManager")

BiocManager::install(pkgs)

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. Downloading. 

# 1) Getting the basic information of data set. 

library(GEOquery)  

s <- "GSE470"

gse <- getGEO(s, GSEMatrix = FALSE) 

gse

# 2) Downloading the raw data of data set. 

library(GEOquery)  

s <- "GSE470"

getGEOSuppFiles(s, 
                makeDirectory = TRUE, 
                baseDir = getwd(), 
                fetch_files = TRUE, 
                filter_regex = NULL)

# 3) Decompressing the raw data file. 

setwd(s)

rd <- dir()

untar(rd)

file.remove(rd)

setwd("..")

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Reading the raw data set for GSE470. 

# 1) 

setwd(s)

library(affy)

dat <- ReadAffy() # reading all files in *.cel format. 

class(dat)

print(dat)

# 2) 

d <- matrix(rnorm(100), 10, 10)

d[1, 6] <- 5

d

image(d)

image(dat[, 1])

# 3) using boxplot. 

rGEP <- exprs(dat)

boxplot(rGEP, col = 1:length(dat), las = 2)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. Preprocessing and Checking the quality of raw data set for GSE470. 

# 1) Normalizing the GSE470 data set. 

###--------------------------- S1. RMA algorithm ----------------------------###

eset <- rma(dat)

###--------------------------- S2. MAS algorithm ----------------------------###

###-------------------------- S3. dChip algorithm ---------------------------###

###-------------------------- S4. gcRMA algorithm ---------------------------###

###-------------------------- S5. PLIER algorithm ---------------------------###

###--------------------------- S6. VSN algorithm ----------------------------###



# 2) Rechecking the quality using the arrayQualityMetrics

library(arrayQualityMetrics)

err.samples <- NULL

dir.nam <- paste("QC_report_for_processed", s, sep = "_")

err.pos <- arrayQualityMetrics(expressionset = dat, 
                               outdir = dir.nam, 
                               force = TRUE)

err.cel <- which(err.pos$arrayTable == "x", arr.ind = TRUE)[, 1]

err.sam <- err.pos$arrayTable$sampleNames[as.numeric(names(table(err.cel))[table(err.cel) > 0])]

err.samples <- c(err.samples, err.sam)

setwd(pri.dir)

# 3) Normalizing the GSE470 data set. 

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. Preprocessing and Checking the quality of raw data set for GSE470. 

head(exprs(eset))

### End of Step-05.
### ****************************************************************************

### ****************************************************************************
### Step-06. Annotation.  

# probe which mapped only one gene. 
# probe which mapped only more gene. 
# 1) one probe to one gene
# 2) more probes to one gene. 
# 3) one probe to more genes.
# 3) Obtaining the final gene expression matrix for GSE470. 

### End of Step-05.
### ****************************************************************************



################################################################################
### End of chunk-12.
################################################################################
