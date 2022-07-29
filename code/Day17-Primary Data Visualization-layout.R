
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
### code chunk number 16: Primary drawing in R.
################################################################################

### ****************************************************************************
### Step-01. About working directory in R. 

# For windows, work directory. 

pri.dir <- getwd()

setwd("D:/00-GitHub/LRC/tmp/")

### End of Step-01.
### ****************************************************************************

### ****************************************************************************
### Step-02. A contour line.. 

# --------------------------- (1) the first one ------------------------------ #

# 1) 调用计算二维核密度的包
library(KernSmooth)  
mtcars1 <- mtcars[, c("wt", "mpg")]

# 2) 计算二维核密度
est <- bkde2D(mtcars1, apply(mtcars1, 2, dpik)) 

# 3) 画等高图
contour(x = est$x1, 
        y = est$x2, 
        z = est$fhat,
        nlevels = 15,
        col = "darkgreen",
        xlab = "wt",
        ylab = "mpg")  

# 4) 添加散点
points(mtcars1) 

# --------------------------- (2) the second one ----------------------------- #

# 1) 对一个密度矩阵绘图

library(datasets)
data("volcano")

# 2) 用plot3D包中的image2d绘图

library(plot3D)

op <- par(mfrow = c(2, 2))

image2D(z = volcano)
image2D(z = volcano, col = "grey", shade = 0.2, contour = TRUE)
image2D(z = volcano, colkey = FALSE)
image2D(z = volcano, colkey = list(plot = FALSE, side = 3))
colkey (side = 3, add = TRUE, clim = range(volcano))

par(op) # or, using: par(no.readonly = TRUE)

# --------------------------- (3) the third one ------------------------------ #

op <- par(mfrow = c(2, 2))

# picture-1
contour(x = est$x1, 
        y = est$x2, 
        z = est$fhat,
        nlevels = 15,
        col = "darkgreen",
        xlab = "wt",
        ylab = "mpg")  

points(mtcars1)  # 添加散点

# picture-2
image2D(z = est$fhat, 
        x = est$x1, 
        y = est$x2)

# picture-3
contour2D(z = est$fhat, 
          x = est$x1, 
          y = est$x2)

# picture-4
contour3D(x = est$x1,
  y = est$x2,
  z = 0.6,
  colvar = est$fhat,
  zlim = c(0, 1),
  clab = c("height", "m")
)

par(op) # or, using: par(no.readonly = TRUE)

# --------------------------- (4) the fourth one ----------------------------- #

# 通过另一种方式进行核密度估计
# install.packages("MASS")
library(MASS)

# Data
x <- rnorm(500)
y <- rnorm(500)
z <- kde2d(x, y, n = 50)

# p1
plot(x, y, pch = 19)
contour(z, lwd = 2, add = TRUE,
        col = hcl.colors(10, "Spectral")) 

# p2
filled.contour(z)

# p3
filled.contour(z, nlevels = 10)

# p4
filled.contour(z, color.palette = terrain.colors)

# p5
filled.contour(z, plot.axes = {
  axis(1)
  axis(2)
  contour(z, add = TRUE, lwd = 2)
}
)

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Preparing the raw data set for GSE470. 

# ---------------------- (1) downloading the data set ------------------------ #

dir.create("asthma")

setwd("asthma/")

url.asthma <- "https://www.ncbi.nlm.nih.gov/geo/download/?acc=GSE470&format=file"

file.asthma <- "GSE470_RAW.tar"

download.file(url = url.asthma, 
              destfile = file.asthma, 
              mode = "wb")

untar(file.asthma)

file.remove(file.asthma)

# ---------------------- (2) Preprocessing the dataset ----------------------- #

# installing the affy package. 

# BiocManager::install("affy")

library(affy)

dat <- ReadAffy()

class(dat)

str(dat)

print(dat)

# checking the primary dataset. 

raw.data <- exprs(dat)

head(raw.data)

dim(raw.data)

boxplot(raw.data, col = 1:12)

summary(raw.data)

hist(raw.data)

# data normalization. 

eset <- rma(dat)

class(eset)

final.data <- exprs(eset)

boxplot(final.data, col = 1:12)

hist(final.data)

# ---------------------- (3) Checking data distribution ---------------------- #

op <- par(mfrow = c(3, 4))

for (i in 1:12) {
  
  hist(final.data[, i], freq = FALSE) 
  
  lines(density(final.data[, i]), col = i, lwd = 3)

}

par(op)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. the layout of cartons. 

# ---------------------- (1) Setting parameter par() ------------------------- #

op <- par(mfrow = c(2, 2), bg = "cyan")

plot(1:10, pch = "1")
plot(1:10, pch = "2")
plot(1:10, pch = "3")
plot(1:10, pch = "4")

par(op)

# 
par(bg = "cyan")
par(mar = c(4, 4, .5, .5))
set.seed(1000)
plot(rnorm(10))

# 
par(bg = "cyan")
par(mar = c(4, 4, 0.5, 0.5))
set.seed(1000)
par(mgp = c(2, 0, 0)) # xlab/ylab的位置位置，坐标轴标签位置，坐标轴轴线位置， 
plot(rnorm(10))

# 
usr <- par("usr") 

xy <- locator(n = 1)

# 
par(mfrow = c(2, 2))
par(mar = c(3, 3, 0.5, 0.5))

plot(rnorm(100), pch = "1")

plot(rnorm(100), pch = "2")

plot(rnorm(100), pch = "3")

plot(rnorm(100), pch = "4")

# 
par(mfcol = c(2, 2))
par(mar = c(3, 3, 0.5, 0.5))
plot(rnorm(100), pch = "1")

plot(rnorm(100), pch = "2")

plot(rnorm(100), pch = "3")

plot(rnorm(100), pch = "4")

#

op <- par(mar = c(2, 2, 0.5, 0.5)) # 边距
mat <- matrix(1:9, nrow = 3, byrow = TRUE)
mat <- matrix(c(1, 1, 2, 1, 1, 2, 3, 4, 5), nrow = 3, byrow = TRUE)

layout(mat)

plot(1:10)



par(mar = c(2, 2, 0.5, 0.5))
mat <- matrix(c(1, 1, 2, 3, 4, 4), nrow = 2, byrow = TRUE)

layout(mat)

plot(rnorm(100), pch = "1")

plot(rnorm(100), pch = "2")

plot(rnorm(100), pch = "3")

plot(rnorm(100), pch = "4")


# 
par(mar = c(2, 2, 0.5, 0.5))
mat <- matrix(c(1, 2, 3, 4, 4, 4), nrow = 2, byrow = TRUE)

layout(mat)

plot(rnorm(100), pch = "1")

plot(rnorm(100), pch = "2")

plot(rnorm(100), pch = "3")

plot(rnorm(100), pch = "4")

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-08. The violin diagram. 

# install.packages("vioplot")
library("vioplot")
vioplot(iris[, 1:4], col = 2:5)
legend("topright",
       legend = colnames(iris)[1:4],
       fill = 2:5,
       cex = 1.0)

### End of Step-09.
### ****************************************************************************

################################################################################
### End of chunk-12.
################################################################################
