
# Copyright 2023 Felipe Queiroz. Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions: The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software. The software is provided "as is", without warranty of any kind, express or implied, including but not limited to the warranties of merchantability, fitness for a particular purpose and noninfringement. In no event shall the authors or copyright holders be liable for any claim, damages or other liability, whether in an action of contract, tort or otherwise, arising from, out of or in connection with the software or the use or other dealings in the software.

## Tabelas - Estatística
require(xtable)


## Normal

z <- seq(0, 3.99, 0.01)
p <- round(pnorm(z) - 0.5, 5)
tab.normal <- matrix(p, nrow = 40, byrow = TRUE)
rownames(tab.normal) <- seq(0, 3.99, 0.1)
colnames(tab.normal) <- seq(0, 9, 1)
tab.normal

sink(file="rawtablenormal.txt")
xtable(tab.normal, digits = 5)
sink(file=NULL)

# Figura
pdf("plotnormal.pdf")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(-2, -2, -2, -2)) 
a1 <- 0
a2 <- 1
gera <- rnorm(100, a1, a2)
x <- seq(-4,4, 0.01)
y <- dnorm(x, a1, a2)
plot(x, y, type = "l", axes = FALSE, ylim = c(0, max(y) + 0.007), ylab = "", xlab = "")
x1 <- seq(0, 1.5, 0.01) 
y1 <- dnorm(x1, a1, a2)
polygon(c(0, 0, x1, 1.5, 0), c(0, 0, y1, 0, 0), col = "lightblue", border = FALSE)
lines(x, y) 
text(1.5, -0.009, expression(z), cex = 0.8)
text(0, -0.009, expression(0), cex = 0.8)
text(0.8, 0.1, expression(p), cex = 0.9)

lines(x = c(0,0), y = c(0,dnorm(0)))
lines(x = c(1.5,1.5), y = c(0,dnorm(1.5)))
lines(x = c(-4,4), y = c(0,0))
dev.off()


## t-Student

nu <- c(2:30, 35, 40, 50, 60, 120)
p  <- c(90, 80, 70, 60, 50, 40, 30, 20, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0.5, 0.2, 0.1)/100
quant.t <- round(qt(p/2, nu[2], lower.tail = FALSE), 3)

tab.t <- matrix(rep(0, length(p)*length(nu)) ,ncol = length(p))
for(i in 1:length(nu)){
  tab.t[i,] <- round(qt(p/2, nu[i], lower.tail = FALSE), 3)
}
colnames(tab.t) = round(p*100, 2)
rownames(tab.t) = nu

sink(file="rawtablet.txt")
xtable(tab.t, digits = 3)
sink(file=NULL)

# Figura
pdf("plott.pdf")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(-2.3, -2, -2, -2)) 
gera <- rt(100, df = 4)
x <- seq(-4, 4, 0.01)
y <- dt(x, 4)
plot(x, y, type = "l", axes = FALSE, ylim = c(0, max(y) + 0.007), ylab = "", xlab = "")
x1 <- seq(-4, qt(0.2/2, 4), 0.01) 
y1 <- dt(x1, 4)
polygon(c(-4, x1, qt(0.2/2, 4), -4,  0), c(0, y1, 0, 0,0), col = "lightblue", border = FALSE)
x1 <- seq(qt(0.2/2, 4, lower.tail = FALSE), 4,  0.01) 
y1 <- dt(x1, 4)
polygon(c(qt(0.2/2, 4, lower.tail = FALSE), x1, 4,  0), c(0, y1, 0,0), col = "lightblue", border = FALSE)
lines(x, y) 
text(qt(0.2/2, 4, lower.tail = FALSE), -0.009, expression(t), cex = 0.8)
text(qt(0.2/2, 4, lower.tail = TRUE), -0.009, expression(-t), cex = 0.8)
text(0, -0.009, expression(0), cex = 0.8)
text(-2, 0.03, expression(p/2), cex = 0.9)
text(2, 0.03, expression(p/2), cex = 0.9)
lines(x = c(qt(0.2/2, 4, lower.tail = FALSE), qt(0.2/2, 4, lower.tail = FALSE)), 
      y = c(0, dt(qt(0.2/2, 4, lower.tail = FALSE), 4)) )
lines(x = c(qt(0.2/2, 4, lower.tail = TRUE), qt(0.2/2, 4, lower.tail = TRUE)), 
      y = c(0, dt(qt(0.2/2, 4, lower.tail = TRUE), 4)) )
lines(x = c(-4,4), y = c(0,0) )
dev.off()


## Chi-quadrado

nu <- c(1:30, 35, 40, 45, 50)
p  <- c(99, 98, 97.5, 95, 90, 80, 70, 60, 50, 40, 30, 20, 10, 5, 4, 2.5, 2, 1, 0.2, 0.1)/100
quant.chi <- round(qt(p/2, nu[2], lower.tail = FALSE), 3)

quant.chi <- matrix(rep(0, length(p)*length(nu)) ,ncol = length(p))
for(i in 1:length(nu)){
  quant.chi[i,] <- round(qchisq(p, nu[i], lower.tail = FALSE), 3)
}
colnames(quant.chi) = round(p*100, 2)
rownames(quant.chi) = nu

sink(file="rawtablechi.txt")
xtable(quant.chi, digits = 3)
sink(file=NULL)

# Figura
pdf("plotchi.pdf")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(-2.3, -2, -2, -2)) 
gera <- rchisq(100, df = 4)
x <- seq(0, 17, 0.01)
y <- dchisq(x, 4)
plot(x, y, type = "l", axes = FALSE, ylim = c(0, max(y) + 0.007), ylab = "", xlab = "")
x1 <- seq(qchisq(0.1, 4, lower.tail = FALSE), 17,  0.01) 
y1 <- dchisq(x1, 4)
polygon(c(qchisq(0.1, 4, lower.tail = FALSE), x1, 17,  0), c(0, y1, 0,0), col = "lightblue", border = FALSE)
lines(x, y) 

text(qchisq(0.1, 4, lower.tail = FALSE), -0.004, expression(q), cex = 0.8)
text(8.5, 0.015, expression(p), cex = 0.9)
lines(x = c(qchisq(0.1, 4, lower.tail = FALSE), qchisq(0.1, 4, lower.tail = FALSE)), 
      y = c(0, dchisq(qchisq(0.1, 4, lower.tail = FALSE), 4)) )
lines(x = c(0,17), y = c(0,0) )
dev.off()


## F de Snedecor p=0.1

#num <- c(1:16, 18, 20, 30, 40, 60, 120)
#den <- c(2:30, 40, 60, 120)

num <- c(5:30, 40, 60, 120)
den <- c(5:30, 40, 60, 120)

quant.f0.1 <- matrix(rep(0, length(num)*length(den)), ncol = length(num))
for(i in 1:length(num)){
  quant.f0.1[,i] <- round(qf(0.1, num[i], den, lower.tail = FALSE), 2)
}
colnames(quant.f0.1) = num
rownames(quant.f0.1) = den

sink(file="rawtablef01.txt")
xtable(quant.f0.1, digits = 2)
sink(file=NULL)

# Figura
pdf("plotf01.pdf")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(-2.3, -2, -2, -2)) 
gera <- rf(100, 3, 20)
x <- seq(0, 6, 0.01)
y <- df(x, 3, 20)
plot(x, y, type = "l", axes = FALSE, ylim = c(0, max(y) + 0.007), ylab = "", xlab = "")
x1 <- seq(qf(0.1, 3, 20, lower.tail = FALSE), 6,  0.01) 
y1 <- df(x1, 3, 20)
polygon(c(qf(0.1, 3, 20, lower.tail = FALSE), x1, 6,  0), c(0, y1, 0,0), col = "lightblue", border = FALSE)
lines(x, y) 

text(qf(0.1, 3, 20, lower.tail = FALSE), -0.013, expression(q), cex = 0.8)
text(3.4, 0.09, expression(p == 0.1))
lines(x = c(qf(0.1, 3, 20, lower.tail = FALSE), qf(0.1, 3, 20, lower.tail = FALSE)), 
      y = c(0, df(qf(0.1, 3, 20, lower.tail = FALSE), 3, 20)) )
lines(x = c(0,6), y = c(0,0) )
dev.off()


## F de Snedecor p=0.05

#num <- c(1:16, 18, 20, 30, 40, 60, 120)
#den <- c(2:30, 40, 60, 120)

num <- c(5:30, 40, 60, 120)
den <- c(5:30, 40, 60, 120)

quant.f0.05 <- matrix(rep(0, length(num)*length(den)), ncol = length(num))
for(i in 1:length(num)){
  quant.f0.05[,i] <- round(qf(0.05, num[i], den, lower.tail = FALSE), 2)
}
colnames(quant.f0.05) = num
rownames(quant.f0.05) = den

sink(file="rawtablef005.txt")
xtable(quant.f0.05, digits = 2)
sink(file=NULL)

# Figura
pdf("plotf005.pdf")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(-2.3, -2, -2, -2)) 
gera <- rf(100, 3, 20)
x <- seq(0, 6, 0.01)
y <- df(x, 3, 20)
plot(x, y, type = "l", axes = FALSE, ylim = c(0, max(y) + 0.007), ylab = "", xlab = "")
x1 <- seq(qf(0.05, 3, 20, lower.tail = FALSE), 6,  0.01) 
y1 <- df(x1, 3, 20)
polygon(c(qf(0.05, 3, 20, lower.tail = FALSE), x1, 6,  0), c(0, y1, 0,0), col = "lightblue", border = FALSE)
lines(x, y) 

text(qf(0.05, 3, 20, lower.tail = FALSE), -0.013, expression(q), cex = 0.8)
text(4, 0.08, expression(p == 0.05))
lines(x = c(qf(0.05, 3, 20, lower.tail = FALSE), qf(0.05, 3, 20, lower.tail = FALSE)), 
      y = c(0, df(qf(0.05, 3, 20, lower.tail = FALSE), 3, 20)) )
lines(x = c(0,6), y = c(0,0) )
dev.off()


## F de Snedecor p=0.025

#num <- c(1:16, 18, 20, 30, 40, 60, 120)
#den <- c(2:30, 40, 60, 120)

num <- c(5:30, 40, 60, 120)
den <- c(5:30, 40, 60, 120)

quant.f0.025 <- matrix(rep(0, length(num)*length(den)), ncol = length(num))
for(i in 1:length(num)){
  quant.f0.025[,i] <- round(qf(0.025, num[i], den, lower.tail = FALSE), 2)
}
colnames(quant.f0.025) = num
rownames(quant.f0.025) = den

sink(file="rawtablef0025.txt")
xtable(quant.f0.025, digits = 2)
sink(file=NULL)

# Figura
pdf("plotf0025.pdf")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(-2.3, -2, -2, -2)) 
gera <- rf(100, 3, 20)
x <- seq(0, 6, 0.01)
y <- df(x, 3, 20)
plot(x, y, type = "l", axes = FALSE, ylim = c(0, max(y) + 0.007), ylab = "", xlab = "")
x1 <- seq(qf(0.025, 3, 20, lower.tail = FALSE), 6,  0.01) 
y1 <- df(x1, 3, 20)
polygon(c(qf(0.025, 3, 20, lower.tail = FALSE), x1, 6,  0), c(0, y1, 0,0), col = "lightblue", border = FALSE)
lines(x, y) 

text(qf(0.025, 3, 20, lower.tail = FALSE), -0.013, expression(q), cex = 0.8)
text(4.5, 0.08, expression(p == 0.025))
lines(x = c(qf(0.025, 3, 20, lower.tail = FALSE), qf(0.025, 3, 20, lower.tail = FALSE)), 
      y = c(0, df(qf(0.025, 3, 20, lower.tail = FALSE), 3, 20)) )
lines(x = c(0,6), y = c(0,0) )
dev.off()


## F de Snedecor p=0.01

#num <- c(1:16, 18, 20, 30, 40, 60, 120)
#den <- c(2:30, 40, 60, 120)

num <- c(5:30, 40, 60, 120)
den <- c(5:30, 40, 60, 120)

quant.f0.01 <- matrix(rep(0, length(num)*length(den)), ncol = length(num))
for(i in 1:length(num)){
  quant.f0.01[,i] <- round(qf(0.01, num[i], den, lower.tail = FALSE), 2)
}
colnames(quant.f0.01) = num
rownames(quant.f0.01) = den

sink(file="rawtablef001.txt")
xtable(quant.f0.01, digits = 2)
sink(file=NULL)

# Figura
pdf("plotf001.pdf")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(-2.3, -2, -2, -2)) 
gera <- rf(100, 3, 20)
x <- seq(0, 6, 0.01)
y <- df(x, 3, 20)
plot(x, y, type = "l", axes = FALSE, ylim = c(0, max(y) + 0.007), ylab = "", xlab = "")
x1 <- seq(qf(0.01, 3, 20, lower.tail = FALSE), 6,  0.01) 
y1 <- df(x1, 3, 20)
polygon(c(qf(0.01, 3, 20, lower.tail = FALSE), x1, 6,  0), c(0, y1, 0,0), col = "lightblue", border = FALSE)
lines(x, y) 

text(qf(0.01, 3, 20, lower.tail = FALSE), -0.013, expression(q), cex = 0.8)
text(5.2, 0.08, expression(p == 0.01))
lines(x = c(qf(0.01, 3, 20, lower.tail = FALSE), qf(0.01, 3, 20, lower.tail = FALSE)), 
      y = c(0, df(qf(0.01, 3, 20, lower.tail = FALSE), 3, 20)) )
lines(x = c(0,6), y = c(0,0) )
dev.off()


## F de Snedecor p=0.005
#num <- c(1:16, 18, 20, 30, 40, 60, 120)
#den <- c(2:30, 40, 60, 120)

num <- c(5:30, 40, 60, 120)
den <- c(5:30, 40, 60, 120)

quant.f0.005 <- matrix(rep(0, length(num)*length(den)), ncol = length(num))
for(i in 1:length(num)){
  quant.f0.005[,i] <- round(qf(0.005, num[i], den, lower.tail = FALSE), 2)
}
colnames(quant.f0.005) = num
rownames(quant.f0.005) = den

sink(file="rawtablef0005.txt")
xtable(quant.f0.005, digits = 2)
sink(file=NULL)

# Figura
pdf("plotf0005.pdf")
mar.default <- c(5,4,4,2) + 0.1
par(mar = mar.default + c(-2.3, -2, -2, -2)) 
gera <- rf(100, 3, 20)
x <- seq(0, 8, 0.005)
y <- df(x, 3, 20)
plot(x, y, type = "l", axes = FALSE, ylim = c(0, max(y) + 0.007), ylab = "", xlab = "")
x1 <- seq(qf(0.005, 3, 20, lower.tail = FALSE), 8,  0.005) 
y1 <- df(x1, 3, 20)
polygon(c(qf(0.005, 3, 20, lower.tail = FALSE), x1, 8,  0), c(0, y1, 0,0), col = "lightblue", border = FALSE)
lines(x, y) 

text(qf(0.005, 3, 20, lower.tail = FALSE), -0.013, expression(q), cex = 0.8)
text(6, 0.08, expression(p == 0.005))
lines(x = c(qf(0.005, 3, 20, lower.tail = FALSE), qf(0.005, 3, 20, lower.tail = FALSE)), 
      y = c(0, df(qf(0.005, 3, 20, lower.tail = FALSE), 3, 20)) )
lines(x = c(0,8), y = c(0,0) )
dev.off()


