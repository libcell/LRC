
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

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. Reading the raw data set for GSE470. 

# 1) setting the work directory. 

s <- "GSE470"

setwd(s)

# 2) # reading all files in *.cel format. 

library(affy)

dat <- ReadAffy() 

class(dat)

print(dat)

# 3) using boxplot to check data distribution. 

rGEP <- exprs(dat)

boxplot(rGEP, col = 1:length(dat), las = 2)

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Preprocessing and Checking the quality of raw data set for GSE470. 

# 1) Normalizing the GSE470 data set. 

###--------------------------- S1. RMA algorithm ----------------------------###

eset.r <- rma(dat)
class(eset.r)
print(eset.r)

###--------------------------- S2. MAS algorithm ----------------------------###

eset.m <- mas5(dat)
class(eset.m)
print(eset.m)

head(exprs(eset.m))
head(exprs(eset.m))

###-------------------------- S3. dChip algorithm ---------------------------###

eset.d <- expresso(dat, 
                   normalize.method = "invariantset", 
                   bg.correct = FALSE, 
                   pmcorrect.method = "pmonly", 
                   summary.method = "liwong")
class(eset.d)
print(eset.d)

head(exprs(eset.d))
head(exprs(eset.d))

###-------------------------- S4. gcRMA algorithm ---------------------------###

library(gcrma)
eset.g <- gcrma(dat)
class(eset.g)
print(eset.g)

head(exprs(eset.g))
head(exprs(eset.g))

###-------------------------- S5. PLIER algorithm ---------------------------###

library(plier)
eset.p <- justPlier(dat, 
                    normalize = TRUE)
class(eset.p)
print(eset.p)

head(exprs(eset.p))
head(exprs(eset.p))

###--------------------------- S6. VSN algorithm ----------------------------###

library(vsn)
eset.v <- vsnrma(dat)
class(eset.p)
print(eset.p)

head(exprs(eset.p))
head(exprs(eset.p))

###--------------------------- S6. VSN algorithm ----------------------------###

# 2) Data Visualization

op <- par(mfrow = c(2, 4))
boxplot(rGEP, col = 1:length(dat), main = "Raw")
boxplot(log2(rGEP), col = 1:length(dat), main = "Raw-Log2")
boxplot(exprs(eset.r), col = 1:length(dat), main = "RMA")
boxplot(log2(exprs(eset.m)), col = 1:length(dat), main = "MAS5")
boxplot(log2(exprs(eset.d)), col = 1:length(dat), main = "dChip")
boxplot(exprs(eset.g), col = 1:length(dat), main = "GCRMA")
boxplot(exprs(eset.p), col = 1:length(dat), main = "PLIER")
boxplot(exprs(eset.v), col = 1:length(dat), main = "VSN")
par(op)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. Annotation.  

# 1) Downloading the GPL8300-10787.txt from GEO for GPL8300 platform.
# or, using the following code. 

gse <- GEOquery::getGEO("GSE470", GSEMatrix = FALSE)

anno.file <- gse@gpls$GPL8300@dataTable@table

dim(anno.file)

anno.file <- anno.file[, c(1, 10:12)]

# 2) Classifying the probes into three types, for GPL8300 platform. 

# i) one probe to one gene

# o2o.probe

# ii) multiple probes to one gene. 

# m2o.probe


# iii) one probe to multiple genes.

# o2m.probe


# 3) Obtaining the final gene expression matrix for GSE470. 

### End of Step-04.
### ****************************************************************************


################################################################################
### End of chunk-12.
################################################################################
