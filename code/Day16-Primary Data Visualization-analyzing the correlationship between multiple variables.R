
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
### Step-02. Preparing the raw data set for GSE470. 

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

### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. corrgram showing the relationships between multiple variables.  

# --------------------------- 1) using corrgram ------------------------------ #

# install.packages("corrgram")
library("corrgram")
head(baseball)
round(cor(baseball[, 5:14], use="pair"),2)

vars2 <- c("Assists","Atbat","Errors","Hits","Homer","logSal",
           "Putouts","RBI","Runs","Walks","Years")
corrgram(baseball[,vars2], order=TRUE,
         main="Baseball data PC2/PC1 order",
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

corrgram(iris[, 1:4])
corrgram(iris[, 1:4], 
         lower.panel = panel.shade, 
         upper.panel = panel.ellipse, 
         diag.panel=panel.minmax, 
         text.panel=panel.txt)

# --------------------------- 2) using corrplot ------------------------------ #

# seven methods: "circle", "square", "ellipse", "number", "shade", "color", "pie".

library(corrplot)
mtcars
dim(mtcars)

M <- cor(mtcars)

corrplot(M, method = "circle")

corrplot.mixed(M, lower = "ellipse", upper = "circle")
corrplot.mixed(M, lower = "ellipse", upper = "number")
corrplot(M, type = "upper")

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. density diagram.   

# install.packages("reshape2") # 安装包
library(reshape2) # 加载包
attach(tips) # 绑定数据集
head(tips) # 查看数据集前6行

plot(density(tip),  # 绘制密度图
     main = "绘制tip变量的密度图",  # 添加标题
     col = "blue", # 设置线的颜色
     lwd = 2) # 设置线宽

ran = rnorm(1000000) # 生成一组随机数字
plot(density(ran), # 绘制ran密度图
     main = "tip密度图与正态分布之间的差异", # 设置图像标题
     xlim = c(-4,10)) # 设置坐标轴范围
polygon(density(ran), col = "burlywood") # 绘制填充曲线
lines(density(tip),  # 添加曲线
      col = "blue", # 设置曲线颜色
      lwd = 2) # 设置曲线宽度

mean(tip) # 计算tip均值
# [1] 2.998279
sd(tip) # 计算tip标准差
# [1] 1.383638
newtip = (tip-2.998)/1.384 # 创建新变量newtip

plot(density(ran), # 绘制正态分布图形
     ylim = c(0,.48), # 设置y范围
     main = "newtip变量与正态分布差异", # 设置图形
     xlim = c(-4,8)) # 设置x范围
polygon(density(ran), # 设置填充曲线
        col = "burlywood") # 设置颜色
lines(density(newtip), # 添加曲线
      col = "blue", #设置曲线颜色
      lwd = 2) # 设置线宽

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. QQ diagrams. 

summary(tip)
qtip = quantile(tip, seq(0,1,.1))
qtip
qtip2 = quantile(tip, seq(0,1,.005)) # 计算tip变量的分位数结果
qqplot(ran, qtip2, #绘制QQ图
       main = "QQ图", # 设置图形标题
       xlim = c(-3,3),  # 设置x范围
       col = "skyblue2") # 设置颜色
qqline(qtip2, # 设置参考线
       col = "burlywood", # 设置颜色
       lwd = 2) # 设置线宽
grid(lty = "dotted", # 设置网格
     col = "gray75") # 设置颜色

qqnorm(tip, # 绘制图形的变量
       main = "快速绘制QQ plot", # 图形标题
       col = "blue", # 设置颜色
       ylab = "tip quantiles") # 设置y轴标签
qqline(tip, # 设置参考线
       col = "burlywood", # 设置颜色
       lwd = 2) # 设置线宽
grid(lty = "dotted", # 设置网格
     col = "gray75") # 设置颜色

logtip = log10(tip) # 设置对数转换
qqnorm(logtip, # 绘图变量
       main = "log10(tip)的QQ图",
       col = "blue4") # 设置颜色
qqline(logtip, # 设置参考线
       col = "burlywood3", # 设置颜色
       lwd = 2) # 设置线宽

### End of Step-05.
### ****************************************************************************

### ****************************************************************************
### Step-06. A contour line.. 

# 1) the first one.

library(KernSmooth)  # 计算二维核密度的包
mtcars1 = data.frame(wt, mpg)
est = bkde2D(mtcars1, apply(mtcars1, 2, dpik))     # 计算二维核密度
contour(est$x1, est$x2, est$fhat, nlevels = 15, col = "darkgreen", xlab = "wt", ylab = "mpg")  # 画等高图
points(mtcars1)  # 添加散点

# 2) the second one. 

library(datasets)
data("volcano")

library(plot3D)

op <- par(mfrow = c(2, 2))

image2D(z = volcano)
image2D(z = volcano, col = "grey", shade = 0.2, contour = TRUE)
image2D(z = volcano, colkey = FALSE)
image2D(z = volcano, colkey = list(plot = FALSE, side = 3))
colkey (side = 3, add = TRUE, clim = range(volcano))

par(op) # or, using: par(no.readonly = TRUE)

### End of Step-06.
### ****************************************************************************

### ****************************************************************************
### Step-07. The star and radar diagram. 

# ------------------------- (1) star diagram --------------------------------- #

stars(iris[1:12, 1:4])

data(mtcars)
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 2),
      main = "Motor Trend Cars", full = FALSE) 

stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 2),
      main = "Motor Trend Cars", draw.segments = TRUE)

stars(iris[1:20, 1:4], draw.segments = TRUE, key.loc = c(2, 2))

data(USJudgeRatings)
stars(USJudgeRatings, labels = abbreviate(case.names(USJudgeRatings)),
      key.loc = c(13, 1.5), main = "Judge not ...", len = 0.8)

# ------------------------- (2) radar diagram -------------------------------- #

set.seed(1)
df <- data.frame(rbind(rep(10, 8), rep(0, 8),
                       matrix(sample(0:10, 8),
                              nrow = 1)))
colnames(df) <- paste("Var", 1:8)

set.seed(1)
df2 <- data.frame(rbind(rep(10, 8), rep(0, 8),
                        matrix(sample(0:10, 24,
                                      replace = TRUE),
                               nrow = 3)))
colnames(df2) <- paste("Var", 1:8)

# install.packages("fmsb")
library(fmsb)

radarchart(df)

# install.packages("fmsb")
library(fmsb)

radarchart(df,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           cglwd = 1,       # Line width of the grid
           pcol = 4,        # Color of the line
           plwd = 2,        # Width of the line
           plty = 1)        # Line type of the line 

# install.packages("fmsb")
library(fmsb)

radarchart(df,
           cglty = 1, cglcol = "gray",
           pcol = 4, plwd = 2,
           pfcol = rgb(0, 0.4, 1, 0.25)) 


# install.packages("fmsb")
library(fmsb)

radarchart(df,
           cglty = 1, cglcol = "gray",
           pcol = 1, plwd = 2,
           pdensity = 10,
           pangle = 40)  


# install.packages("fmsb")
library(fmsb)

radarchart(df2) 

# install.packages("fmsb")
library(fmsb)

radarchart(df2,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1)        # Line type for each line  

# install.packages("fmsb")
library(fmsb)

# Fill colors
areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))

radarchart(df2,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas)   # Color of the areas   

# install.packages("fmsb")
library(fmsb)

areas <- c(rgb(1, 0, 0, 0.25),
           rgb(0, 1, 0, 0.25),
           rgb(0, 0, 1, 0.25))

radarchart(df2,
           cglty = 1,       # Grid line type
           cglcol = "gray", # Grid line color
           pcol = 2:4,      # Color for each line
           plwd = 2,        # Width for each line
           plty = 1,        # Line type for each line
           pfcol = areas)   # Color of the areas   

legend("topright",
       legend = paste("Group", 1:3),
       bty = "n", pch = 20, col = areas,
       text.col = "grey25", pt.cex = 2) 

# ------------------------- (3) face diagram --------------------------------- #
# 15 indexes: 
# 1-脸的高度; 2-脸的宽度; 3-脸型; 4-嘴巴厚度; 5-嘴巴宽度; 6-微笑; 7-眼睛的高度; 
# 8-眼睛宽度; 9-头发长度; 10-头发宽度; 11-头发风格; 12-鼻子高度; 13-鼻子宽度; 
# 14-耳朵宽度; 15-耳朵高度.

# install.packages("aplpack") # or using TeachingDemos package. 
library(aplpack)

data("longley")
dim(longley)
faces(longley[1:9,], face.type = 0)


# ------------------------- (4) face diagram --------------------------------- #

andrews.curves <- function(xdf, cls, npts=101, title="Classes") {
  n <- nrow(xdf)
  clss <- as.factor(cls)
  xpts <- seq(0, 2*pi, length=npts)
  X <- xpts
  for (i in 1:n) {
    xi <- unname(unlist(xdf[i, ]))
    ys <- andrews.function(xi, npts)
    X <- cbind(X, ys)
  }
  ymin <- min(X[, 2:(n+1)])
  ymax <- max(X[, 2:(n+1)])
  plot(0, 0, type="n", xlim=c(0, 2*pi), ylim=c(ymin, ymax),
       main="Andrews' Curves", xlab="", ylab="")
  
  clrs <- as.integer(clss)
  for (i in 2:(n+1)) {
    lines(X[, 1], X[, i], col=clrs[i-1])
  }
  legend(4, ymax, levels(clss), col=c(1:nlevels(clss)), lty=1)
  # return(X)
}

andrews.function <- function (xs, no.pts=101) {
  n <- length(xs)
  xpts <- seq(0, 2*pi, length=no.pts)
  ypts <- c()
  for (p in xpts) {
    y <- xs[1]
    for (i in 2:n) {
      if (i %% 2 == 1) { y <- y + xs[i]*sin((i %/% 2)*p) }
      else             { y <- y + xs[i]*cos((i %/% 2)*p) }
    }
    ypts <- c(ypts, y)
  }
  return(ypts)
}

data(iris)
old <- par(bg = "whitesmoke")
andrews.curves(iris[, 1:4], 
               iris[, 5], 
               title = "Iris Data")
par(old)



### End of Step-07.
### ****************************************************************************

### ****************************************************************************
### Step-08. The violin diagram. 

x <- c(6, 9, 0, 19, -1, 8, 12, 5, 3, 7,
       2, 4, 3, -8, -9, 8, 4, 12, 5, 14)

# install.packages("vioplot")
library("vioplot")

vioplot(x)
vioplot(x, horizontal = TRUE)

vioplot(x,
        col = 2,               # Color of the area
        rectCol = "red",       # Color of the rectangle
        lineCol = "white",     # Color of the line
        colMed = "green",      # Pch symbol color
        border = "black",      # Color of the border of the violin
        pchMed = 16,           # Pch symbol for the median
        plotCentre = "points") # If "line", plots a median line

stripchart(x, method = "jitter", col = "blue",
           vertical = TRUE, pch = 19, add = TRUE)

box <- boxplot(x)

x <- x[!(x %in% box$out)]

vioplot(x)

par(mfrow = c(1, 2))

vioplot(1:10)
vioplot(1:10, ylog = TRUE)

par(mfrow = c(1, 1))

set.seed(1)

# Multimodal data
n <- 10000
ii <- rbinom(n, 1, 0.5)
data <- rnorm(n, mean = 130, sd = 10) * ii +
  rnorm(n, mean = 80, sd = 5) * (1 - ii)

# Histogram
hist(data, probability = TRUE, col = "grey", axes = FALSE,
     main = "", xlab = "",  ylab = "")

# X-axis
axis(1)

# Density
lines(density(data), lwd = 2, col = "red")

# Add violin plot
par(new = TRUE)
vioplot(data, horizontal = TRUE, yaxt = "n", axes = FALSE,
        col = rgb(0, 1, 1, alpha = 0.15))


tail(chickwts) # Last rows

data <- chickwts

vioplot(data$weight ~ data$feed, col = 2:length(levels(data$feed)),
        xlab = "Feed", ylab = "Weight")

stripchart(data$weight ~ data$feed, vertical = TRUE, method = "jitter",
           pch = 19, add = TRUE, col = 3:8)

tail(trees) # Last rows

data <- trees

vioplot(data, col = 2:4, border = 2:4)

# Equivalent to:
stacked_data <- stack(trees)
vioplot(stacked_data$values ~ stacked_data$ind, col = 2:4,
        border = 2:4)


par(mfrow = c(1, 2))

data <- chickwts

#----------------
# Lower to higher
#----------------

medians <- reorder(data$feed, data$weight, median)
# medians <- with(data, reorder(feed, weight, median)) # Equivalent

vioplot(data$weight ~ medians, col = 2:(length(levels(data$feed)) + 1),
        xlab = "", ylab = "Weight", las = 2)

#----------------
# Higher to lower
#----------------

medians <- reorder(data$feed, -data$weight, median)
# medians <- with(data, reorder(feed, -weight, median)) # Equivalent

vioplot(data$weight ~ medians, col = 2:(length(levels(data$feed)) + 1),
        xlab = "", ylab = "Weight", las = 2)

par(mfrow = c(1, 1))



par(mfrow = c(1, 2))

# Exponential data
set.seed(5)
x <- rexp(20)

#-------------------
# Vertical vioplot
#-------------------

vioplot(x, col = 4)

# Add mean point
points(mean(x), pch = 19, col = "green", cex = 1.5)

#-------------------
# Horizontal vioplot
#-------------------

vioplot(x, col = 4, horizontal = TRUE)

# Add mean point
points(mean(x), 1, pch = 19, col = "green", cex = 1.5)

legend("topright", pch = c(21, 19), col = c("black", "green"),
       bg = "white", legend = c("Median", "Mean"), cex = 1.25)

par(mfrow = c(1, 1))


par(mfrow = c(1, 2))

set.seed(5)
df <- data.frame(x = rexp(20), y = rexp(20), z = rexp(20))

#--------------------------
# Vertical vioplot by group
#--------------------------

vioplot(df, col = 2:4)

# Add mean points
means <- apply(df, 2, mean)
means <- colMeans(df) # Equivalent (more efficient)

points(means, pch = 19, col = "green", cex = 1.25)

legend("top", pch = c(21, 19), col = c("black", "green"),
       bg = "white", legend = c("Median", "Mean"), cex = 1.25)

#----------------------------
# Horizontal vioplot by group
#----------------------------

vioplot(df, col = 2:4,
        horizontal = TRUE)

# Add mean points
means <- apply(df, 2, mean)
means <- colMeans(df) # Equivalent (more efficient)

points(means, 1:ncol(df), pch = 19, col = "green", cex = 1.25)

par(mfrow = c(1, 1))


data <- trees

tall <- trees[trees$Height >= 76, ]
small <- trees[trees$Height < 76, ]

vioplot(tall, side = "left", plotCentre = "line", col = 2)
vioplot(small, side = "right", plotCentre = "line", col = 3, add = TRUE)

legend("topleft", legend = c("Tall", "Small"), fill = c(2, 3), cex = 1.25)

### End of Step-09.
### ****************************************************************************

### ****************************************************************************
### Step-09. the layout of cartons. 

op <- par(mfrow = c(2, 2), 
          bg = "cyan")

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

### End of Step-09.
### ****************************************************************************

################################################################################
### End of chunk-12.
################################################################################
