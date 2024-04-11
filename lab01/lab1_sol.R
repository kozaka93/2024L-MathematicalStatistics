###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 1            ###
###########################################


# Cel: wybrane rozkłady prawdopodobieństwa --------------------------------

# nrom - rozkład normalny
# chisq - rozkład chi-kwadrat
# gamma - rozkład gamma
# exp - rozkład wykładniczy
# f - rozkład F-Snedecora

# d - gęstość f(x) lub rozkład prawdopodobieństwa P(X=x)
# p - dystrybuanta F(x) = P(X<=x)
# q - funkcja kwantylowa F^{1}(p)
# r - generowanie liczb pseudolosowych


# Zadanie 1 ---------------------------------------------------------------

# a) N(0, 1), N(1, 1), N(2, 1)

par(mfrow = c(1, 3))

# gęstość

#?curve
# curve can plot also an expression in the variable x

curve(dnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(dnorm(x, 1, 1), xlim = c(-3, 3), col = 2, add = TRUE)
curve(dnorm(x, 2, 1), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)","N(1,1)", "N(2,1)"), lty=1, col=1:3, text.col=1)

title("Gęstości")


# dystrybuanta

curve(pnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(pnorm(x, 1, 1), xlim = c(-3, 3), col = 2, add = TRUE)
curve(pnorm(x, 2, 1), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)","N(1,1)", "N(2,1)"), lty=1, col=1:3, text.col=1)

title("Dystrybuanta")


# funkcja przeżycia 

curve(1 - pnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(1 - pnorm(x, 1, 1), xlim = c(-3, 3), col = 2, add = TRUE)
curve(1 - pnorm(x, 2, 1), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)","N(1,1)", "N(2,1)"), lty=1, col=1:3, text.col=1)

title("Funkcja przeżycia")


# b) N(0, 1), N(0, 0.5), N(0, 2)

par(mfrow = c(1, 3))

# gęstość

curve(dnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(dnorm(x, 0, 0.5), xlim = c(-3, 3), col = 2, add = TRUE)
curve(dnorm(x, 0, 2), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)","N(0,0.5)", "N(0,2)"), lty=1, col=1:3, text.col=1)

title("Gęstości")


# dystrybuanta

curve(pnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(pnorm(x, 0, 0.5), xlim = c(-3, 3), col = 2, add = TRUE)
curve(pnorm(x, 0, 2), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)","N(0,0.5)", "N(0,2)"), lty=1, col=1:3, text.col=1)

title("Dystrybuanta")


# funkcja przeżycia

curve(1 - pnorm(x, 0, 1), xlim = c(-3, 3), ylim = c(0, 0.5), col = 1)
curve(1 - pnorm(x, 0, 0.5), xlim = c(-3, 3), col = 2, add = TRUE)
curve(1 - pnorm(x, 0, 2), xlim = c(-3, 3), col = 3, add = TRUE)
legend("topleft", c("N(0,1)","N(0,0.5)", "N(0,2)"), lty=1, col=1:3, text.col=1)

title("Funkcja przeżycia")


# Zadanie 2 ---------------------------------------------------------------

pnorm(3, 0, 1) - pnorm(-3, 0, 1)


# Zadanie 3 ---------------------------------------------------------------

# a) 
pnorm(179, 173, 6)

# b)
pnorm(180, 173, 6) - pnorm(167, 173, 6)

# c)
1 - pnorm(181, 173, 6)

# d)
qnorm(0.6, 173, 6)


# Zadanie 4 ---------------------------------------------------------------

# a)
qnorm(0.95, 0, 1)

# b)
qnorm(0.975, 0, 1)

# c)
?qt
qt(0.95, 10)

# d)
qt(0.99, 20)

# e)
?qchisq
qchisq(0.9, 4)

# f)
qchisq(0.95, 10)

# g)
?qf
qf(0.95, 2, 10)

# h)
qf(0.99, 3, 18)


# Zadanie 5 ---------------------------------------------------------------

?dgamma

# a)

par(mfrow = c(1, 1))
curve(dgamma(x, 1, 1), xlim = c(0, 6), ylim = c(0, 1), col = 1)
curve(dgamma(x, 0.5, 1), xlim = c(0, 6), col = 2, add = TRUE)
curve(dgamma(x, 2, 1), xlim = c(0, 6), col = 3, add = TRUE)
curve(dgamma(x, 3, 1), xlim = c(0, 6), col = 4, add = TRUE)
legend("topright", c("Gamma(1,1)","Gamma(0.5,1)", "Gamma(2,1)", "Gamma(3,1)"), lty=1, col=1:4, text.col=1)


# b)

par(mfrow = c(1, 1))
curve(dgamma(x, 2, 1), xlim = c(0, 6), ylim = c(0, 1), col = 1)
curve(dgamma(x, 2, 2), xlim = c(0, 6), col = 2, add = TRUE)
curve(dgamma(x, 2, 3), xlim = c(0, 6), col = 3, add = TRUE)
legend("topright", c("Gamma(2,1)","Gamma(2,2)", "Gamma(2,3)"), lty=1, col=1:3, text.col=1)


# Zadanie 6 ---------------------------------------------------------------

?dchisq

# a)

par(mfrow = c(1, 1))
curve(dchisq(x, 5), xlim = c(0, 80), ylim = c(0, 0.2), col = 1)
curve(dchisq(x, 10), xlim = c(0, 80), col = 2, add = TRUE)
curve(dchisq(x, 40), xlim = c(0, 80), col = 3, add = TRUE)
legend("topleft", c("Chi^2(5)","Chi^2(10)", "Chi^2(40)"), lty=1, col=1:3, text.col=1)

# b)


# Zadanie 7 ---------------------------------------------------------------

par(mfrow = c(1, 1))
curve(dt(x, 1), xlim = c(-5, 5), ylim = c(0, 0.4), col = 1)
curve(dt(x, 5), col = 2, add = TRUE)
curve(dt(x, 30), col = 3, add = TRUE)
legend("topleft", c("t-Student(1)","t-Student(5)", "t-Student(30)"), lty=1, col=1:3, text.col=1)


par(mfrow = c(1, 3))

curve(dt(x, 5), xlim = c(-5, 5), col = 1)
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = 2, add = TRUE)
legend("bottom", c("t-Student(5)", "N(0,1)"), lty = 1, col = 1:2, text.col = 1)

curve(dt(x, 10), xlim = c(-5, 5), col = 1)
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = 2, add = TRUE)
legend("bottom", c("t-Student(10)", "N(0,1)"), lty = 1, col = 1:2, text.col = 1)

curve(dt(x, 40), xlim = c(-5, 5), col = 1)
curve(dnorm(x, 0, 1), xlim = c(-5, 5), col = 2, add = TRUE)
legend("bottom", c("t-Student(40)", "N(0,1)"), lty = 1, col = 1:2, text.col = 1)



# Zadanie 8 ---------------------------------------------------------------

par(mfrow = c(1, 3))

# a)
curve(df(x, 10, 5), ylim = c(0, 1), xlim = c(0, 3), col = 1)
curve(df(x, 10, 10), col = 2, add = TRUE)
curve(df(x, 10, 20), col = 3, add = TRUE)
legend("topright", c("F Snedecor(10,5)","F Snedecor(10,10)", "F Snedecor(10,20)"), lty = 1, col = 1:3, text.col = 1)


# b)
curve(df(x, 5, 2), ylim = c(0, 1), col = 1)
curve(df(x, 3, 2), col = 2, add = TRUE)
curve(df(x, 2, 2), col = 3, add = TRUE)
legend("topright", c("F Snedecor(5,2)","F Snedecor(3,2)", "F Snedecor(2,2)"), lty = 1, col = 1:3, text.col = 1)


# c)
curve(df(x, 2, 1), ylim = c(0, 1), xlim = c(0, 2), col= 1)
curve(df(x, 2, 5), col = 2, add = TRUE)
curve(df(x, 2, 10), col = 3, add = TRUE)
curve(df(x, 2, 20), col = 4, add = TRUE)
curve(dexp(x, 1), col = 5, add = TRUE)
legend("topright", c("F Snedecor(2,1)","F Snedecor(2,5)", "F Snedecor(2,10)", "F Snedecor(2,20)", "Exp(1)"), lty = 1, col = 1:5, text.col = 1)


# Zadanie 9 ---------------------------------------------------------------

par(mfrow = c(1, 1))
curve(dbeta(x, 1, 1), ylim = c(0, 3), xlim = c(0, 1), col = 1)
curve(dbeta(x, 2, 2), col = 2, add = TRUE)
curve(dbeta(x, 2, 5), col = 3, add = TRUE)
curve(dbeta(x, 5, 2), col = 4, add = TRUE)
legend("topright", c("Beta(1,1)","Beta(2,2)", "Beta(2,5)", "Beta(5,2)"), lty = 1, col = 1:5, text.col = 1)


# Zadanie 10 --------------------------------------------------------------

par(mfrow = c(1, 1)) 
x <- 0:15
barplot(dbinom(x, 10, 0.5), ylim = c(0, 0.3), xlim = c(0, 20), col = rgb(0, 0, 0, alpha = 0.5))
barplot(dbinom(x, 10, 0.25), col = rgb(1, 0, 0, alpha = 0.5), add = TRUE)
barplot(dbinom(x, 50, 0.25), col = rgb(0, 1, 0, alpha = 0.5), add = TRUE)

