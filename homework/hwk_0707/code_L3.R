
### 安装EBImage包
install.packages("BiocManager")
library("BiocManager")
install("EBImage")

library(EBImage)


### 读取图像
library(EBImage)
Image2 <- readImage("./cqnu.jpg")
Image3 <- readImage("./liudehua.jpg")
print(Image2)

# plot data
hist(Image2)
hist(Image3)
### 亮度调整
a1 <- Image2 + 0.2
display(a1)
display(Image2)

a2 <- Image2 - 0.4
display(a2)
hist(a1)
hist(a2)

### 组合图像
c <- combine(Image2, Image3)
display(c)
hist(c)

### 合并图像到一个对象
a3 <- Image2 + Image3
a4 <- Image2/2 + Image3/3
a5 <- Image2/2 + Image3
display(a3)
display(a4)
display(a5)
hist(a4)

### 调整对比度
a6 <- Image2*2
a7 <- Image2*0.5
display(a6)
display(a7)

### 相机Gamma校正
a8 <- Image2^0.5
a9 <- Image2^2
display(a8)
display(a9)

### 颜色调整
colorMode(Image2) <- Grayscale
print(Image2)
display(Image2)
colorMode(Image2) <- Color
display(Image2)

### Cropping 
k <- Image2[201:400, 201:300, ]
display(k)

### 保存图像
writeImage(k, "new.jpg")

### Flip, flop, rotate and resize images.
l <- flip(Image2)
display(l)
p <- flop(Image2)
display(p)
f <- rotate(Image2, 45)
display(f)
print(f)
o <- resize(Image2, 400)
display(o)

### Low-pass filter
low <- makeBrush(81, shape = "disc", step = FALSE)^2
low <- low/sum(low)
Image2.low <- filter2(Image2, low)
display(Image2.low)

high <- matrix(1, 3, 3)
high[2, 2] <- -5
Image2.high <- filter2(Image2, high)
display(Image2.high)
