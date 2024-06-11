###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 7            ###
###########################################



# Cel: weryfikacja hipotez ------------------------------------------------


# Zadanie 6 ---------------------------------------------------------------

przed <- scan(nlines = 1)
27 21 34 24 30 27 33 31 22 27

po <- scan(nlines = 1)
29 32 29 27 31 26 35 30 29 28

alpha <- 0.05

### I sposób -------------------------------------------------------

# z_i = przed_i - po_i

# H: mu_z = 0
# K: mu_z < 0

# test na sprawdzenie czy rozkład jest normalny -> nie ma podstaw do odrzucenia H

shapiro.test(przed - po)

# p-value = 0.504 > alpha

# zakładamy, że rozkład jest normalny

t.test(przed - po, mu = 0, alternative = "less")

# p-value = 0.09322 > alpha -> nie ma podstaw do odrzucenia H

### II sposób -------------------------------------------------------

# H: mu_przed = mu_po
# K: mu_przed < mu_po

shapiro.test(przed) # p-value = 0.6694 > alpha -> nie ma podstaw do odrzucenia H
shapiro.test(po) # p-value = 0.666 > alpha -> nie ma podstaw do odrzucenia H

# zakładamy, że rozkład "przed" i "po" jest normalny

t.test(przed, po, alternative = "less", paired = TRUE)

# p-value = 0.09322 > alpha -> nie ma podstaw do odrzucenia H



# Zadanie 8 ---------------------------------------------------------------

# H: p = 0.04
# K: p > 0.04

p0 <- 0.04
p_d <- k/n
n <- 200
k <- 14
alpha <- 0.05

### I sposób -------------------------------------------------------------

# statystyka testowa

t <- (p_d - p0) / sqrt((p0 * (1 - p0))/n)

# 2.165064

# obszar krytyczny

qnorm(1 - alpha)

# [1.644854, Inf)

# Odp: t należy do obszaru krytycznego, zatem odrzucamy H

### II sposób -------------------------------------------------------------

prop.test(k, n, p = 0.04, alt = "greater")

# p-value = 0.02359 < alpha -> odrzucamy H

binom.test(k, n, p = 0.04, alt = "greater")

# p-value = 0.03121 < alpha -> odrzucamy H



# Zadanie 9 ---------------------------------------------------------------

# H: p_tech = p_lic
# K: p_tech > p_lic


### I sposób ---------------------------------------------------------

k_tech <- 455
n_tech <- 700
p_d_tech <- k_tech / n_tech

k_lic <- 517
n_lic <- 1320
p_d_lic <- k_lic / n_lic

p <- (k_tech + k_lic) / (n_tech + n_lic)
n <- (n_tech * n_lic) / (n_tech + n_lic)

# statystyka testowa

t <- (p_d_tech - p_d_lic) / (sqrt((p * (1 - p)) / n))

# 11.05804

# obszar krytyczny

qnorm(1 - alpha)

# [1.644854, Inf)

# Odp: t należy do obszaru krytycznego -> odrzucamy H

### II sposób ---------------------------------------------------------

prop.test(c(455, 517), c(700, 1320), alt = "gr")

# p-value < 2.2e-16 < alpha -> odrzucamy H



# Zadanie 10 --------------------------------------------------------------

# a) 

y <- 0:20
p <- 1 - pbinom(y, 20, 0.2)
y[9]

# b) 

N <- 100000

wyniki <- replicate(N, {rbinom(1, 20, 0.2) > 8})
rozmiar <- mean(wyniki)
rozmiar

# lub

mean(rbinom(N, 20, 0.2) > 8)

# c)

alfa <- 0.02

gamma <- (0.02 - rozmiar)/dbinom(8, 20, 0.2)
gamma



# Zadanie 11 --------------------------------------------------------------

# H: mu = 2
# K: mu > 2

mu <- seq(1, 4, 0.1)

moc <- c()
k <- qt(.95, 9)
N <- 1000

for(i in 1:length(mu)){
  wynik <- replicate(N, {
    x <- rnorm(10, mu[i], 1)
    t <- (mean(x) - 2)/sd(x)*sqrt(10)
    t > k 
  })
  moc[i] <- mean(wynik)
} 

plot(mu, moc, las = 1, type = "l",
     xlab = expression(mu),
     ylab = expression(M(mu)),
     ylim = c(0, 1))
    
abline(v = 2, lty = 2)
text(1.2, 0.08, expression(alpha==0.05))
abline(h = 0.05, lty = 2)


# b)

mu <- seq(1, 4, 0.1)

moc <- c()
k <- qt(.975, 9)
N <- 1000

for(i in 1:length(mu)){
  wynik <- replicate(N, {
    x <- rnorm(10, mu[i], 1)
    t <- (mean(x) - 2)/sd(x)*sqrt(10)
    t > k | t < -k
  })
  moc[i] <- mean(wynik)
} 

plot(mu, moc, las = 1, type = "l",
     xlab = expression(mu),
     ylab = expression(M(mu)),
     ylim = c(0, 1))

abline(v = 2, lty = 2)
text(1.2, 0.08, expression(alpha==0.05))
abline(h = 0.05, lty = 2)


