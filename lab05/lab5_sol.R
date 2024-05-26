###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 5            ###
###########################################



# Cel: podstawy wnioskowania statystycznego; ------------------------------
# estymacja punktowa i przedziałowa ---------------------------------------


# Zadanie 7 ---------------------------------------------------------------

temp <- scan(nlines = 3)
330.0 322.0 345.0 328.6 331.0 342.0
342.4 340.4 329.7 334.0 326.5 325.8
337.5 327.3 322.6 341.0 340.0 333.0

prz.ufn.mi <- function(x, alfa){
  n <- length(x)
  m <- mean(x)
  s <- sd(x)
  q <- qt(1 - alfa / 2, n - 1)
  prz <- m + c(-1, 1) * s * q / sqrt(n)
  prz
}

prz.ufn.mi(temp, 0.05)

t.test(temp, conf.level = 0.95)$conf

prz.ufn.war <- function(x, alfa) {
  n <- length(x)
  q1 <- qchisq(1 - alfa / 2, n - 1)
  q2 <- qchisq(alfa / 2, n - 1)
  prz <- (n - 1) * var(x) / c(q1, q2)
  list(wariancja = prz, odchylenie = sqrt(prz))
}

prz.ufn.war(temp, 0.05)$odch


# Zadanie 8 ---------------------------------------------------------------

k <- 578
n <- 1014

?binom.test
binom.test(k, n, conf.level = 0.95)$conf


# Zadanie 9 ---------------------------------------------------------------

k <-  3
n <-  12
binom.test(k, n, conf.level = 0.95)$conf


# Zadanie 10 --------------------------------------------------------------

iris
vir <- iris$Petal.Length[iris$Species == "virginica"]

prz.ufn.mi(vir, 0.01)

prz.ufn.war(vir, 0.05)


# Zadanie 11 --------------------------------------------------------------

k <- 19
n <- 150
binom.test(k, n, conf.level = 0.96)$conf


# Zadanie 12 --------------------------------------------------------------

x <- chickwts$weight[chickwts$feed == "soybean"]
prz.ufn.war(x, 0.07)$war


# Zadanie 13 --------------------------------------------------------------

t.test(faithful$waiting, conf.level=0.99)$conf

prz.ufn.mi(faithful$waiting, 0.01)


# Zadanie 14 --------------------------------------------------------------

prz.ufn.war(Orange$circumference, 0.01)$odch


# Zadanie 15 --------------------------------------------------------------

library(MASS)

k <- length(Pima.te$age[Pima.te$type=="Yes"])
n <- length(Pima.te$age)

binom.test(k,n,conf.level = 0.95)$conf


k <- length(Pima.te$age[Pima.te$type=="Yes" & Pima.te$age >= 35])
n <- length(Pima.te$age[Pima.te$age >= 35])

binom.test(k,n,conf.level = 0.95)$conf
