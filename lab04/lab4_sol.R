###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 4            ###
###########################################



# Cel: podstawy wnioskowania statystycznego; ------------------------------
# estymacja punktowa i przedziałowa ---------------------------------------


# Zadanie 2 ---------------------------------------------------------------

# a)

N <- 500
Y <- rcauchy(N)

med <- c()
sre <- c()
for(i in 1:N) {
  med[i] <- median(Y[1:i])
  sre[i] <- mean(Y[1:i])
}

plot(1:N, sre, type = "l", col = 1)
lines(1:N, med, type = "l", col = 2)
abline(h = 0, col = 3)
legend("topleft", c("mean","median", "a = 0"), lty = 1, col = 1:3, text.col = 1)

# b)

N <- 500
Y <- rcauchy(N)

st <- c()
r <- c()
for(i in 2:N){
  st[i-1] <- sd(Y[1:i])
  r[i-1] <- IQR(Y[1:i])/2
}

plot(2:N, st, type = "l", log = 'y', ylim = c(0.5, max(st)+1), col = 1)
lines(2:N, r, lty = 1, col = 2)
abline(h = 1, col = 3)
legend("topleft", c("sd","SQR", "b = 1"), lty = 1, col = 1:3, text.col = 1)


# Zadanie 3 ---------------------------------------------------------------

# a)

N <- 500
Y <- rnorm(N)

srednia <- cumsum(Y)/1:N
plot(1:N, srednia, type = "l")

med <- c()

for(i in 1:N){
  med[i] <- median(Y[1:i])
}

lines(1:N, med, lty = 1, col = 2)
abline(h = 0, col = 3)
legend("bottomright", c("mean", "median", "mu = 0"), lty = 1, col = 1:3)

# b)

N <- 500
Y <- rnorm(N)

st <- c()
r <- c()

for(i in 2:N){
  st[i-1] <- sd(Y[1:i])
  r[i-1] <- IQR(Y[1:i])/1.35
}

plot(2:N, st, type = "l", log = 'y', ylim = c(0.5, max(st)+1))
lines(2:N, r, lty = 1, col = 2)
abline(h = 1, col = 3)
legend("topleft", c("sd", "IQR/1.35", "sigma = 1"))


# Zadanie 4 ---------------------------------------------------------------

n <- 10000
theta <- 1

theta_M <- c()
theta_NW <- c()

for (i in 1:n){
  proba <- runif(20, 0, theta)
  theta_M[i] <- 2*mean(proba)
  theta_NW[i] <- max(proba)
}

b_M <- mean(theta_M)-theta
b_NW <- mean(theta_NW)-theta
mse_M <- sum((theta_M-theta)^2)/n
# lub: var(theta_M)+b_M^2
mse_NW <- sum((theta_NW-theta)^2)/n
# lub: var(theta_NW)+b_NW^2


# Zadanie 5 ---------------------------------------------------------------

N <- 10000
n <- 10
mi <- 9
sigma <- 3
alfa <- 0.05
q <- qt(1 - alfa / 2, n - 1)
k <- 0
for(i in 1:N){
  x <- rnorm(n, mi, sigma)
  m <- mean(x)
  s <- sd(x)
  if( mi >= m - q * s / sqrt(n) & mi <= m + q * s / sqrt(n) ) k <- k + 1
}
paste0(k/N*100, "%")


# Zadanie 6 ---------------------------------------------------------------

n <- 50
m <- 28.4
sigma <- 4.75
alfa <- 0.05
q <- qnorm(1 - alfa / 2)
c(m - q * sigma / sqrt(n), m + q * sigma / sqrt(n))

