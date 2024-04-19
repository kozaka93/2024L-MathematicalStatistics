###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 2            ###
###########################################


# Cel: statystyka opisowa: graficzne metody prezentacji danych ------------
# i wyznaczanie podstawowych statystyk próbkowych -------------------------

# ZESTAW ZADAŃ 1 ----------------------------------------------------------

# Zadanie 11 --------------------------------------------------------------

dgeom(0, 0.1)
dgeom(1, 0.1)
dgeom(2, 0.1)
dgeom(3, 0.1)
1 - pgeom(10, 0.1)


# Zadanie 12 --------------------------------------------------------------

phyper(0, 5, 195, 10)


# Zadanie 13 --------------------------------------------------------------

# a)
1 - pexp(1000, 0.0001)
1 - pexp(10000, 0.0001)
1 - pexp(30000, 0.0001)


# b)
qexp(0.1, 0.0001)


# Zadanie 14 --------------------------------------------------------------

# a) Y~exp(4)

# b) EY=1/4 VarY=1/16

# c)
pexp(0.5, 4)

# d)
1 - pexp(1, 4)

# Zadanie 15 --------------------------------------------------------------

# a)

n <- 20000
x <- runif(n)
y <- runif(n)
plot(x, y, pch = ".",
     xlim = c(0, 1), 
     ylim = c(0, 1))
z <- y < x*x
z[1:20]

points(x[z > 0], y[z > 0], col = 3, pch = ".")
curve(x^2, col = 2, add = TRUE, lwd = 3)

mean(z)


# b)

n <- 20000
x <- runif(n)
y <- runif(n)
plot(x, y, pch = '.',
     xlim = c(0, 1),
     ylim = c(0, 1))
z <- x * x < y & y < 1 - x * x
z[1:20]


points(x[z > 0], y[z > 0], col = 3)
curve(x ^ 2, col = 2, add = TRUE, lwd = 3)
curve(1 - x ^ 2, col = 2, add = TRUE, lwd = 3)

mean(z)


# ZESTAW ZADAŃ 2 ----------------------------------------------------------


# Zadanie 1 ---------------------------------------------------------------

kobiety <- c(17364, 56128, 11239, 8170)
stan <- c("panny", "mężatki", "wdowy", "rozwódki")

# a)

pie(kobiety)
pie(kobiety, labels = stan)
pie(kobiety, paste(stan, kobiety, sep = "\n"),
    col = c(2, 3, 4, 6))

x <- c(1, 2, 3, 2, 1)
table(x)

paste(round(prop.table(kobiety) * 100, 2), "%")

pie(kobiety, paste(stan, round(prop.table(kobiety) * 100, 2), "%"))

# b)

barplot(kobiety)
barplot(kobiety, names.arg = stan)


# Zadanie 2 ---------------------------------------------------------------

stacje <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=stacje.csv")

stacje$Answers

pie(table(stacje))
pie(table(stacje), labels = paste0(names(table(stacje)), ",", 
                                   paste0(prop.table(table(stacje)) * 100, "%")))
barplot(table(stacje))


# Zadanie 3 ---------------------------------------------------------------

notowania <- c(23.30, 24.50, 25.30, 25.30, 24.30, 24.80, 
               25.20, 24.50, 24.60, 24.10, 24.30, 26.10, 
               23.10, 25.50, 22.60, 24.60, 24.30, 25.40, 
               25.20, 26.80)

plot(1:20, notowania, type = "l")

# Zadanie 4 ---------------------------------------------------------------

butelki <- read.csv("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=butelki.csv")

butelki$strength

# a) 

cisnienie <- butelki$strength * 0.0068947

### dplyr ###

library(dplyr)
butelki %>% 
  mutate(cisnienie = strength * 0.0068947) -> butelki

###

# b) 

hist(cisnienie)
hist(cisnienie, breaks = 6)
hist(cisnienie, breaks = 14)
hist(cisnienie, breaks = "FD")

### ggplot ###

library(ggplot2)

ggplot(butelki, aes(x = cisnienie)) + 
  geom_histogram() -> gg_hist

###

# c)

h <- hist(cisnienie, breaks = 14)
lines(h$mids, h$counts, col = 3, lwd = 3)


# d)
  
stem(cisnienie)
stem(cisnienie, 2)

# e)

boxplot(cisnienie)
summary(cisnienie)

### ggplot ###

ggplot(butelki, aes(x = cisnienie)) + 
  geom_boxplot() 

###

# f)

var(cisnienie)
sd(cisnienie)
IQR(cisnienie)
diff(range(cisnienie))

install.packages("moments")
library(moments)

skewness(cisnienie)
kurtosis(cisnienie)

# g)

quantile(cisnienie, 0.05)
quantile(cisnienie, 0.1)
quantile(cisnienie, 0.25)
quantile(cisnienie, 0.5)
quantile(cisnienie, 0.75)
quantile(cisnienie, 0.9)
quantile(cisnienie, 0.95)

# h)

mean(cisnienie, trim = 0.1)
mean(cisnienie)
median(cisnienie)

lapply(0:49 / 100, function(x) {
  mean(cisnienie, trim = x)
}) -> trim_means

plot(1:length(trim_means), trim_means)


# Zadanie 5 ---------------------------------------------------------------

czynsz <- c(334, 436, 425, 398, 424, 429, 392, 428, 339, 389,
            352, 405, 392, 403, 344, 400, 424, 443, 378, 387,
            384, 498, 374, 389, 367, 457, 409, 454 ,345, 422)

summary(czynsz)
var(czynsz)
sd(czynsz)
IQR(czynsz)
diff(range(czynsz))

skewness(czynsz)
kurtosis(czynsz)
