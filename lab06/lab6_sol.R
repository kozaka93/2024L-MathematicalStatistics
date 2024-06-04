###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 6            ###
###########################################



# Cel: weryfikacja hipotez ------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

wytrzymalosc <- c(1.36, 1.14, 1.27, 1.15, 1.20, 1.29, 1.27, 1.18, 1.23, 1.36,
                  1.38, 1.37, 1.30, 1.21, 1.33, 1.28, 1.32, 1.29, 1.33, 1.25)


# H: mu = mu0
# K: mu > mu0

mu0 <- 1.2
sigma <- 0.07
alpha <- 0.04
n <- length(wytrzymalosc)


### I sposób ----------------------------------------------

# statystyka testowa

t <- (mean(wytrzymalosc) - mu0) / sigma * sqrt(n)

# 4.823518

# obszar krytyczny 

qnorm(1 - alpha)

# [1.750686, Inf)

# Odp: t należy do obszaru krytycznego, zatem odrzucamy hipotezę

### II sposób ----------------------------------------------

# p-value 
# P(T > t) = 1 - P(T < t) 

1 - pnorm(t)

# p-value = 7.052399e-07 < alpha

# Odp: odrzucamy hipotezę



# Zadanie 2 ---------------------------------------------------------------

waga <- c(142, 151, 148, 151, 145, 150, 141)

# H: mu = mu0
# K: mu != mu0

mu0 <- 150
alpha <- 0.05
n <- length(waga)
s <- sd(waga)

### I sposób ----------------------------------------------

# statystyka testowa

t <- (mean(waga) - mu0) / s * sqrt(n)

# -1.970369

# obszar krytyczny 

qt(1 - alpha / 2, n - 1)

# (-Inf, -2.446912] + [2.446912, Inf)

# Odp: t nie należy do obszaru krytycznego, zatem nie ma podstaw
# do odrzucenia H

### II sposób ----------------------------------------------

# p-value 
# 2*min{P(T > t), P(T < t)} = 2*min{1 - P(T < t), P(T < t)} 

2*min(1 - pt(t, n - 1), pt(t, n - 1))

# p-value = 0.09630099 > alpha

# Odp: nie ma podstaw do odrzucenia H

### III sposób ----------------------------------------------

t.test(waga, mu = 150)

# p-value = 0.0963 > alpha

# Odp: nie ma podstaw do odrzucenia H



# Zadanie 3 ---------------------------------------------------------------

czas <- c(2852, 3060, 2631, 2819, 2805, 2835, 2955, 2595, 2690, 2723, 2815, 2914)

# b)

mu0 <- 2900
alpha <- 0.05
n <- length(czas)

# H: próbka jest z rozkładu normalnego
# K: próbka nie jest z rozkładu normalnego

shapiro.test(czas)

# p-value = 0.9532 > alpha 

# Odp: nie ma podstaw do odrzucenia H

# zakładamy, że próbka jest z rozkładu normalnego

s <- sd(czas)

### I sposób ----------------------------------------------

# statystyka testowa

t <- (mean(czas) - mu0) / s * sqrt(n)

# -2.385525

# obszar krytyczny 

qt(1 - alpha, n - 1)

# (-Inf, -1.795885] 

# Odp: t należy do obszaru krytycznego, zatem odrzucamy H

### II sposób ----------------------------------------------

t.test(czas, mu = 2900, alternative = "less")

# p-value = 0.01807 < alpha

# Odp: odrzucamy H



# Zadanie 4 ---------------------------------------------------------------

metoda1 <- scan(nlines = 1)
145 150 153 148 141 152 146 154 139 148

metoda2 <- scan(nlines = 1)
152 150 147 155 140 146 158 152 151 143 153

alpha <- 0.05

# H: mu1 = mu2
# K: mu1 < mu2

### I sposób ----------------------------------------------

# statystyka testowa

stat_testowa <- function(x, y){
  
  p1 <- mean(x) - mean(y)
  
  p2 <- ((length(x) - 1)*sd(x)^2 + (length(y) - 1)*sd(y)^2)/(length(x) + length(y) -2)
  
  p3 <- (length(x) + length(y))/(length(x)*length(y))
  
  p1/sqrt(p2*p3)  
  
}

stat_testowa(metoda1, metoda2)

# -0.9466366

# obszar krytyczny 

qt(-stat_testowa(metoda1, metoda2), length(metoda1) + length(metoda2) - 2)

# (-Inf, - 1.693319]

# Odp: t nie należy do obszaru krytycznego, zatem nie mamy podstaw 
# do odrzucenia H

### II sposób ----------------------------------------------

t.test(metoda1, metoda2, alt = "less", var.equal = TRUE)

# p-value = 0.1779 > alpha -> nie ma podstaw do odrzucenia H



# Zadanie 5 ---------------------------------------------------------------

umy <- scan(nlines = 1)
14 17 7 33 2 24 26 22 12

fiz <- scan(nlines = 1)
13 15 3 2 25 4 1 18 6 9 20 11 5 1 7

alpha <- 0.05

# Nie mamy informacji o wariancji, zatem przeprowadzamy test na równość wariancji

# H: wariancje są równe
# K: wariancje nie są równe 

var.test(umy, fiz)

# p-value = 0.3557 > alpha -> nie ma podstaw do odrzucenia H

# zakładamy równość wariancji

# H: mu1 = mu2
# K: mu1 > mu2

t.test(umy, fiz, alternative = "greater", var.equal = TRUE)

# p-value = 0.01587 < alpha -> odrzucamy H
