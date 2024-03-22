setwd("D:/nguli/Semester 5/Analisis Mediasi Statistik/PROYEK")
d1 <- read.csv("dataAMSB2.csv")
str(d1)
d1 <- d1[-c(1:2)]
str(d1)
d1$jenis.kelamin <- as.factor(d1$jenis.kelamin)
d1$angkatan <- as.factor(d1$angkatan)
d1$prodi <- as.factor(d1$prodi)
sapply(d1, function(x) sum(length(which(is.na(x))))) # cek NA
str(d1)

# STAT DESC | DEMOGRAFI ====
sex <- table(d1$jenis.kelamin); 
sex/sum(sex)*100
angkatan <- table(d1$angkatan); 
angkatan/sum(angkatan)*100
prodi <- table(d1$prodi); 
prodi/sum(prodi)*100


# STAT DESC | SKALA ====
#COPING
cop <- factor(c(d1$y1, d1$y2, d1$y3, d1$y4, d1$y5,
                d1$y6, d1$y7, d1$y8, d1$y9))
t.cop <- table(cop)
t.cop/length(cop)*100

#PROKRASTINASI
pro <- factor(c(d1$x1, d1$x2, d1$x3, d1$x4, d1$x5, 
                d1$x6, d1$x7, d1$x8, d1$x9, d1$x10)) 
t.pro <- table(pro)
t.pro/length(pro)*100

#STRES
stres <- factor(c(d1$m1, d1$m2, d1$m3, d1$m4, d1$m5, d1$m6,
                  d1$m7, d1$m8, d1$m9, d1$m10, d1$m11, d1$m12))
t.stres <- table(stres)
t.stres/length(stres)*100

# KORELASI ====
d1$y <- rowSums(d1[c(4:12)])
d1$x <- rowSums(d1[c(13:22)])
d1$m <- rowSums(d1[23:34])

library(ggplot2)
library(psych)
pairs.panels(d1[,c("x", "y", "m")],
             method = "pearson",
             hist.col = "steelblue",
             density = TRUE,
             ellipses = FALSE,
             lm = TRUE)

# VALIDITAS ====
d1$y <- rowSums(d1[c(6:7, 9:11)])
d1$x <- rowSums(d1[c(13:14, 16:17, 19:22)])
d1$m <- rowSums(d1[23:34])

critical.r <- function( n, alpha = .05 ) {
  df <- n - 2
  critical.t <- qt(alpha/2, df, lower.tail = F)
  critical.r <- sqrt( (critical.t^2) / ( (critical.t^2) + df ) )
  return(critical.r)
}
critical.r(n=nrow(d1), alpha = 0.05)

cor.t <- function(x, y){
  cor.test(x, y)$estimate
}

# COPING
c11 <- cor.t(d1$y1, d1$y) #v3, ga dirun
c12 <- cor.t(d1$y2, d1$y) #v4, ga dirun
c13 <- cor.t(d1$y3, d1$y)
c14 <- cor.t(d1$y4, d1$y)
c15 <- cor.t(d1$y5, d1$y) #v2, ga dirun
c16 <- cor.t(d1$y6, d1$y)
c17 <- cor.t(d1$y7, d1$y)
c18 <- cor.t(d1$y8, d1$y) 
c19 <- cor.t(d1$y9, d1$y) #v1, ga dirun
round(rbind(c13, c14,c16, c17, c18), 3) #y

# PROKRASTINASI
c21 <- cor.t(d1$x1, d1$x)
c22 <- cor.t(d1$x2, d1$x)
c23 <- cor.t(d1$x3, d1$x) #v1, ga dirun
c24 <- cor.t(d1$x4, d1$x)
c25 <- cor.t(d1$x5, d1$x)
c26 <- cor.t(d1$x6, d1$x) #v2, ga dirun
c27 <- cor.t(d1$x7, d1$x)
c28 <- cor.t(d1$x8, d1$x)
c29 <- cor.t(d1$x9, d1$x)
c210 <- cor.t(d1$x10, d1$x) 
round(rbind(c21, c22, c24, c25, c27, c28, c29, c210), 3) #x

# STRES
c31 <- cor.t(d1$m1, d1$m)
c32 <- cor.t(d1$m2, d1$m)
c33 <- cor.t(d1$m3, d1$m)
c34 <- cor.t(d1$m4, d1$m)
c35 <- cor.t(d1$m5, d1$m)
c36 <- cor.t(d1$m6, d1$m)
c37 <- cor.t(d1$m7, d1$m)
c38 <- cor.t(d1$m8, d1$m)
c39 <- cor.t(d1$m9, d1$m)
c310 <- cor.t(d1$m10, d1$m)
c311 <- cor.t(d1$m11, d1$m)
c312 <- cor.t(d1$m12, d1$m)
round(rbind(c31, c32, c33, c34, c35, c36, c37, c38, c39, c310, c311, c312), 3)

# RELIABILITAS ====
library(ltm)
cronbach.alpha(data.frame(d1[c(6:7, 9:11)])) #y
cronbach.alpha(data.frame(d1[c(13:14, 16:17, 19:22)])) #x
# d1[c(13:14, 16:17, 19:22)] == 0.632
cronbach.alpha(data.frame(d1[23:34])) #m

# ASUMSI PERSAMAAN 1 ====
#NORMALITAS
mod1 <- lm(m~x, data=d1)
shapiro.test(residuals(mod1))

#GLEJSER
gmod1 <- lm(abs(residuals(mod1))~x, data=d1)
summary(gmod1)

plot(fitted(mod1), rstandard(mod1), xlab = "Fitted values", 
     ylab = "Standardized resdiuals")
abline(h=0, lty=2, col="red")


# ASUMSI PERSAMAAN 2 ====
#NORMALITAS
mod2 <- lm(y~x+m, data=d1)
shapiro.test(residuals(mod2))

#GLEJSER
gmod2 <- lm(abs(residuals(mod2))~x+m, data=d1)
summary(gmod2)

#MULTIKOLINEARITAS
library(car)
vif(mod2)


# KETEPATAN MODEL ====
summary(mod1)
summary(mod2)
mod3 <- lm(y~x, data = d1)
summary(mod3)

# UJI SOBEL ====
a <- mod1$coefficients[2]
sa <- summary(mod1)[["coefficients"]][, "Std. Error"][2]
b <- mod2$coefficients[3]
sb <- summary(mod2)[["coefficients"]][, "Std. Error"][3]
sab <- sqrt(a^2*sb^2+b^2*sa^2)

sobel <- a*b/sab; abs(sobel)
2*pnorm(q=sobel, lower.tail = FALSE)

library(bda)
mediation.test(d1$m, d1$x, d1$y)
