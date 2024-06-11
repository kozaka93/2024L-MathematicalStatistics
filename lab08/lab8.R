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

# b) 


# Zadanie 4 ---------------------------------------------------------------

proba <- c(10, 26, 56, 64, 30, 14)



# Zadanie 5 ---------------------------------------------------------------



# Zadanie 6 ---------------------------------------------------------------

