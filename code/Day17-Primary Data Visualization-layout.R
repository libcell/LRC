
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
z <- kde2d(x, y, n = 50)  # to density

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
### Step-05. the colors of cartons in R. 

# ---------------------- (1) Colors 1, 2, and 3 ------------------------------ #

set.seed(19)
x <- rnorm(30)
y <- rnorm(30)
plot(x, y, col = rep(1:3, each = 10), pch = 19)
legend("bottomright", legend = paste("Group", 1:3), col = 1:3, pch = 19, bty = "n")


par(mfrow = c(length(colors()) %/% 60 + 1, 1))  # 画布分割
par(mar = c(0.1, 0.1, 0.1, 0.1),
    xaxs = "i",
    yaxs = "i")
for (i in 1:(length(colors()) %/% 60 + 1)) {
  barplot(rep(1, 60),
          col = colors()[((i - 1) * 60 + 1):(i * 60)],
          border = colors()[((i - 1) * 60 + 1):(i * 60)],
          axes = FALSE)
  box()  # 加边框
}

palette() 

#重新设置调色板为colors的前10种颜色
palette(colors()[1:10]) 
palette()      
palette('default')


par(mfrow = c(3, 2))
image(volcano, col = cm.colors(10), main = "cm.colors()")
image(volcano, col = heat.colors(10), main = "heat.colors()")
image(volcano, col = terrain.colors(10), main = "terrain.colors()")
image(volcano, col = topo.colors(10), main = "topo.colors()")
image(volcano, col = rainbow(10), main = "rainbow()")


rgb<-rgb(red=255,green=1:255,blue=0,max=255)
par(mfrow=c(6,1))
par(mar=c(0.1,0.1,2,0.1), xaxs="i", yaxs="i")
barplot(rep(1,255),col= rgb,border=rgb,main="rgb")
barplot(rep(1,100),col=rainbow(100),border=rainbow(100),main="rainbow(100))")
barplot(rep(1,100),col=heat.colors(100),border=heat.colors(100),main="heat.colors(100))")
barplot(rep(1,100),col=terrain.colors(100),border=terrain.colors(100),main="terrain.colors(100))")
barplot(rep(1,100),col=topo.colors(100),border=topo.colors(100),main="topo.colors(100))")
barplot(rep(1,100),col=cm.colors(100),border=cm.colors(100),main="cm.colors(100))")


par(mfrow = c(1,3))
library(RColorBrewer)
par(mar=c(0.1,3,0.1,0.1))
display.brewer.all(type="seq")
display.brewer.all(type="div")
display.brewer.all(type="qual")


# 左图
library(RColorBrewer)
my_col <- brewer.pal(3, 'RdYlGn') # brewer.pal(n, name),其中n为颜色的数量，name表示颜色组的名称
plot(iris$Sepal.Length, iris$Sepal.Width, col = rep(my_col, each =50))
# 右图
plot(iris$Sepal.Length, iris$Sepal.Width, col = rep(rainbow(3), each = 50))


# ---------------------- (2) Connecting colors with data --------------------- #

# colorRamp()

pal <- colorRamp(c("red", "blue"))
pal(0)

## blue
pal(1)

## purple-ish
pal(0.5)

pal(seq(0, 1, len = 10))

# colorRampPalette()

pal <- colorRampPalette(c("red", "yellow"))
## Just return red and yellow
pal(2)
pal(10)
rgb(0, 0, 234, maxColorValue = 255)

# RColorBrewer Package
library(RColorBrewer)
display.brewer.all()
cols <- brewer.pal(3, "BuGn")
cols

pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

# smoothScatter()

set.seed(1)
x <- rnorm(10000)
y <- rnorm(10000)
smoothScatter(x, y)

# Adding transparency
rgb(1, 0, 0, 0.1)

set.seed(2)
x <- rnorm(2000)
y <- rnorm(2000)
plot(x, y, pch = 19)

plot(x, y, pch = 19, col = rgb(0, 0, 0, 0.15))


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

plot(
  c(1:12),
  col = "white",
  xaxt = "n",
  yaxt = "n",
  ann = FALSE
)
axis(1,
     at = 1:12,
     col.axis = "red",
     labels = month.abb)
axis(
  2,
  at = seq(1, 12, length = 10),
  col.axis = "red",
  labels = 1:10,
  las = 2
)
axis(
  3,
  at = seq(1, 12, length = 7),
  col.axis = "blue",
  cex.axis = 0.7,
  tck = -0.01,
  labels = c("Mon", "Tues", "Wed", "Thu", "Fri", "Sat", "Sun")
)
axis(
  4,
  at = seq(1, 12, length = 11),
  col.axis = "blue",
  cex.axis = 0.7,
  tck = -0.01,
  labels = seq(0, 1, 0.1),
  las = 2
)

local = c(
  "bottomright",
  "bottom",
  "bottomleft",
  "left",
  "topleft",
  "top",
  "topright",
  "right",
  "center"
)
par(mar = c(4, 2, 4, 2), pty = 'm')
plot(c(0:10), col = "white")
legend(3, 8, "图例在(3,8)")
legend(1, 13, "图例在(11,11)", xpd = T)
for (i in 1:9) {
  legend(local[i], paste("图例在", local[i]))
}

par(mfrow = c(1, 1))

# ggsci

################################################################################
### End of chunk-12.
################################################################################
