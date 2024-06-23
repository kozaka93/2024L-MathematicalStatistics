###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 8            ###
###########################################



# Cel: testowanie zgodności, jednorodności i niezależności ----------------


# ZESTAW 5 ----------------------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

n <- rnorm(200)
c <- rcauchy(200)
u <- runif(200)
w <- rexp(200)
aw <- w*(-1)

par(mfrow = c(3,2))
qqnorm(n, main = "N(0,1)")
qqline(n)
qqnorm(c, main = "C(0,1)")
qqline(c)
qqnorm(u, main = "U([0,1])")
qqline(u)
qqnorm(w, main = "Exp(1)")
qqline(w)
qqnorm(aw, main = "NegExp(1)")
qqline(aw)



# Zadanie 2 ---------------------------------------------------------------

par(mfrow = c(1, 2))
N <- 200
u <- runif(N)
qu <- qunif(ppoints(N))
qqplot(qu, u)

u <- rexp(N)
qu <- qexp(ppoints(N))
qqplot(qu, u)

ppoints(5, a = 1 / 2)
? ppoints



# Zadanie 3 ---------------------------------------------------------------

# a)

# H: p1 = p2 = p3 = p4 = 1/4
# K: !H

k <- 4
N <- 200
n <- c(73, 74, 34, 19)
p <- n/N
alpha <- 0.05

# I sposób 

# statystyka testowa

t <- sum((n - N * 1/4) ^ 2 /(N  * 1/4))

#  46.44

# obszar krytyczny

qchisq(1 - alpha, k - 1)

# [7.814728, Inf)

# t należy do obaszru krytycznego -> odrzucamy hipotezę 

# II sposób 

chisq.test(n, p = rep(0.25, 4))

# p-value = 4.572e-10 < alpha -> odrzucamy H

# b) 

# H: p1 = 0.367, p2 = 0.378, p3 = 0.186, p4 = 0.076
# K: !H

p <- c(0.367, 0.371, 0.186, 0.076)
chisq.test(n, p = p)

# p-value = 0.7463 > alpha -> nie ma podstaw do odrzucenia H



# Zadanie 4 ---------------------------------------------------------------

proba <- c(10, 26, 56, 64, 30, 14)

# H: proba z rozkladu normalnego
# K: !H


# lewy kraniec przedziału 
w1 <- 19:24
# środek przedziału
w2 <- 19:24 + 0.5
# prawy kraniec przedziału
w3 <- 20:25

# estymujemy mu i sigma 

# estymator mu - średnia

# średnia 

mu <- sum(w2 * proba) / sum(proba)

# 22.1

# estymator sigma - odchylenie

sigma <- sqrt(sum(proba * (w2 - mu) ** 2) / (sum(proba) - 1))

# 1.235977

pr <- pnorm(w3, mu, sigma) - pnorm(w1, mu, sigma)
pr[1] <- pnorm(w3[1], mu, sigma)
pr[length(pr)] <- 1-pnorm(w1[length(pr)], mu, sigma)

chisq.test(proba, p=pr) 

# p-value = 0.9295 > alpha -> nie ma podstaw do odrzucenia H



# Zadanie 5 ---------------------------------------------------------------

prop.test(c(61, 34, 38, 35), c(206, 164, 98, 102))

# p-value = 0.0104 < alpha -> odrzucamy H


# Zadanie 6 ---------------------------------------------------------------

#test Kolmogorowa-Smirnova

dane <- c(2.5, 1.8, 6.0, 0.5, 8.75, 1.2, 3.75)
ks.test(dane, 'pexp', 1/4)
