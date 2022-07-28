
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
### code chunk number 15: Primary drawing in R.
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
### End of Step-02.
### ****************************************************************************

### ****************************************************************************
### Step-03. Scatter plots. 

# ----------------------- (1) General description ---------------------------- #

# create a scatter plot in R Programming Language using the plot() function
# Syntax: plot(x, y, main, xlab, ylab, xlim, ylim, axes)
# Parameters: 
#  x: This parameter sets the horizontal coordinates.
#  y: This parameter sets the vertical coordinates.
#  xlab: This parameter is the label for horizontal axis.
#  ylab: This parameter is the label for vertical axis.
#  main: This parameter main is the title of the chart.
#  xlim: This parameter is used for plotting values of x.
#  ylim: This parameter is used for plotting values of y.
#  axes: This parameter indicates whether both axes should be drawn on the plot.

# --------------------- (2) Simple Scatterplot Chart ------------------------- #

# Get the input values.
input <- mtcars[, c('wt', 'mpg')]
print(head(input))
# Plot the chart for cars with
# weight between 1.5 to 4 and
# mileage between 10 and 25.
plot(x = input$wt, y = input$mpg,
     xlab = "Weight",
     ylab = "Milage",
     xlim = c(1.5, 4),
     ylim = c(10, 25),	
     main = "Weight vs Milage"
)

# ----------------------- (3) Scatterplot Matrices --------------------------- #

# Plot the matrices between
# 4 variables giving 12 plots.

# One variable with 3 others
# and total 4 variables.
pairs(~ wt + mpg + disp + cyl, data = mtcars,
       main = "Scatterplot Matrix")

# ------------------------- (4) 3D Scatterplot ------------------------------- #

# 1) using scatterplot3d package

library(scatterplot3d)
data(iris)
head(iris)

# Basic 3d graphics
scatterplot3d(iris[,1:3])

# Change the angle of point view
scatterplot3d(iris[,1:3], angle = 55)

# Change the main title and axis labels
scatterplot3d(iris[,1:3],
              main="3D Scatter Plot",
              xlab = "Sepal Length (cm)",
              ylab = "Sepal Width (cm)",
              zlab = "Petal Length (cm)")

# Change the shape and the color of points
scatterplot3d(iris[,1:3], pch = 16, color="steelblue")

# Change point shapes by groups
shapes = c(16, 17, 18) 
shapes <- shapes[as.numeric(iris$Species)]
scatterplot3d(iris[,1:3], pch = shapes)

# Change point colors by groups
colors <- c("#999999", "#E69F00", "#56B4E9")
colors <- colors[as.numeric(iris$Species)]
scatterplot3d(iris[,1:3], pch = 16, color=colors)

# Change the global appearance of the graph
scatterplot3d(iris[,1:3], pch = 16, color = colors,
              grid=TRUE, box=FALSE)

# Add bars
scatterplot3d(iris[,1:3], pch = 16, type="h", 
              color=colors)

# Add legends
# Specify the legend position using xyz.convert()
s3d <- scatterplot3d(iris[,1:3], pch = 16, color=colors)
legend(s3d$xyz.convert(7.5, 3, 4.5), legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)
# Specify the legend position using keywords
# "right" position
s3d <- scatterplot3d(iris[,1:3], pch = 16, color=colors)
legend("right", legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16)
# Use the argument inset
s3d <- scatterplot3d(iris[,1:3], pch = 16, color=colors)
legend("right", legend = levels(iris$Species),
       col = c("#999999", "#E69F00", "#56B4E9"), pch = 16, inset = 0.1)
# "bottom" position
s3d <- scatterplot3d(iris[,1:3], pch = 16, color=colors)
legend("bottom", legend = levels(iris$Species),
       col = c("#999999", "#E69F00", "#56B4E9"), pch = 16)

# Customize the legend position
# Custom colors
s3d <- scatterplot3d(iris[,1:3], pch = 16, color=colors)
legend("bottom", legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), pch = 16, 
       inset = -0.13, xpd = TRUE, horiz = TRUE)

# Custom shapes/colors
s3d <- scatterplot3d(iris[,1:3], pch = shapes, color=colors)
legend("bottom", legend = levels(iris$Species),
       col =  c("#999999", "#E69F00", "#56B4E9"), 
       pch = c(16, 17, 18), 
       inset = -0.13, xpd = TRUE, horiz = TRUE)

# Add point labels
scatterplot3d(iris[,1:3], pch = 16, color=colors)
text(s3d$xyz.convert(iris[, 1:3]), labels = rownames(iris),
     cex= 0.7, col = "steelblue")

# Add regression plane and supplementary points
data(trees)
head(trees)

# 3D scatter plot
s3d <- scatterplot3d(trees, type = "h", color = "blue",
                     angle=55, pch = 16)
# Add regression plane
my.lm <- lm(trees$Volume ~ trees$Girth + trees$Height)
s3d$plane3d(my.lm)
# Add supplementary points
s3d$points3d(seq(10, 20, 2), seq(85, 60, -5), seq(60, 10, -10),
             col = "red", type = "h", pch = 8)

# 2) using plot3D package
pm <- par(mfrow = c(2, 2))
pmar <- par(mar = c(5.1, 4.1, 4.1, 2.1))
par(mfrow = c(2,1))
x <- seq(0, 2*pi, length.out = 30)
y <- sin(x)
# Note: this forgets the names of the x and y-variables.
colorkeyplot(x = x, y = y, col = createKey(y), pch = 18, 
             main = "colorkeyplot with 'plot'",
             colorkey = list(clim = range(y)))
abline (v = 4)
abline (h = 0.4)
legendplot(x = x, y = y, col = c("red", "blue")[(y > 0)+1],
           main = "legendplot with 'plot'", pch = 18,
           xlab = "x", ylab = "y",
           legend = list(col = c("red","blue"), pch = 18,
                         legend = c(">0", "<0")))
abline (v = pi)
abline (h = 0)
par(mfrow = c(1,1))
legendplot(x = x, y = y, col = c("red", "blue")[(y > 0)+1],
           main = "legendplot with 'plot'", pch = 18,
           legend.side = 1, las = 1,
           legend = list(col = c("red","blue"), pch = 18,
                         horiz = TRUE, legend = c(">0", "<0")))
abline (v = pi)
abline (h = 0)


# (2) plot3d package

par(mfrow = c(2, 2))
z <- seq(0, 10, 0.2)
x <- cos(z)
y <- sin(z)*z

# greyish background for the boxtype (bty = "g") 
scatter3D(x, y, z, phi = 0, bty = "g",
          pch = 20, cex = 2, ticktype = "detailed")
# add another point
scatter3D(x = 0, y = 0, z = 0, add = TRUE, colkey = FALSE, 
          pch = 18, cex = 3, col = "black")

# add text
text3D(x = cos(1:10), y = (sin(1:10)*(1:10) - 1), 
       z = 1:10, colkey = FALSE, add = TRUE, 
       labels = LETTERS[1:10], col = c("black", "red"))

# line plot
scatter3D(x, y, z, phi = 0, bty = "g", type = "l", 
          ticktype = "detailed", lwd = 4)

# points and lines
scatter3D(x, y, z, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5))

# vertical lines
scatter3D(x, y, z, phi = 0, bty = "g",  type = "h", 
          ticktype = "detailed")

# 3) using rgl package

library(rgl)
attach(iris)

#颜色
mycolors <- c('#6181BD4E', '#F348004E', '#64A10E4E"')
data$color <- mycolors[ as.numeric(Species) ]

# 绘图
plot3d(
  x = Sepal.Length,
  y = Sepal.Width,
  z = Petal.Length,
  col = rep(c('#6181BD4E', '#F348004E', '#64A10E4E'),
            c(8, 8, 8)),
  size = 50,
  type = 's',
  radius = .1,
  xlab = "Sepal Length",
  ylab = "Sepal Width",
  zlab = "Petal Length"
)

### End of Step-03.
### ****************************************************************************

### ****************************************************************************
### Step-04. corrgram. 

# 1) using corrgram
install.packages("corrgram")
library("corrgram")
head(baseball)
round(cor(baseball[, 5:14], use="pair"),2)

vars2 <- c("Assists","Atbat","Errors","Hits","Homer","logSal",
           "Putouts","RBI","Runs","Walks","Years")
corrgram(baseball[,vars2], order=TRUE,
         main="Baseball data PC2/PC1 order",
         lower.panel=panel.shade, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

# 2) using corrplot 

# seven methods: "circle", "square", "ellipse", "number", "shade", "color", "pie".

library(corrplot)
M <- cor(mtcars)
corrplot(M, method="circle")

corrplot.mixed(M, lower="ellipse", upper="circle")

corrplot(M, type="upper")

### End of Step-04.
### ****************************************************************************

### ****************************************************************************
### Step-05. density diagram.   

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

### End of Step-05.
### ****************************************************************************

### ****************************************************************************
### Step-06. QQ diagrams. 

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

### End of Step-06.
### ****************************************************************************

### ****************************************************************************
### Step-07. A contour line.. 

# 1) 等高线图

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

### End of Step-04.
### ****************************************************************************


################################################################################
### End of chunk-12.
################################################################################
