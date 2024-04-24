###########################################
###       STATYSTYKA MATEMATYCZNA       ###
###           LABORATORIUM 3            ###
###########################################



# Cel: statystyka opisowa: graficzne metody prezentacji danych ------------
# i wyznaczanie podstawowych statystyk próbkowych -------------------------



# Zadanie 6 ---------------------------------------------------------------

samochody <- read.csv2("http://pages.mini.pw.edu.pl/~grzegorzewskip/www/?download=samochody.csv")

# a)

samochody$zp <- 1 / samochody$mpg * 3.7851 / 1.609 * 100 
mean(samochody$zp)
sum(is.na(samochody$zp))
mean(samochody$zp, na.rm = TRUE)


# b)

stem(samochody$zp)


# c)

hist(samochody$zp)


# d)

zp2 <- na.omit(samochody$zp)
density(zp2, kernel = "epanechnikov")
par(mfrow = c(1, 2))
plot(density(zp2, kernel = "epanechnikov"))
plot(density(zp2, kernel = "epanechnikov", bw = 0.1))

par(mfrow = c(2, 3))
plot(density(zp2, kernel = "gaussian"))
plot(density(zp2, kernel = "epanechnikov"))
plot(density(zp2, kernel = "rectangular"))
plot(density(zp2, kernel = "triangular"))
plot(density(zp2, kernel = "cosine"))


# e)

par(mfrow = c(1, 1))
boxplot(samochody$zp)


# f)

mean(samochody$zp, na.rm = TRUE)
median(samochody$zp, na.rm = TRUE)
var(samochody$zp, na.rm = TRUE)
sd(samochody$zp, na.rm = TRUE)
range(samochody$zp, na.rm = TRUE)
quantile(samochody$zp, na.rm = TRUE, c(0.25, 0.75, 0.9))
IQR(samochody$zp, na.rm = TRUE)
library(moments)
kurtosis(samochody$zp, na.rm = TRUE)
skewness(samochody$zp, na.rm = TRUE)


# g)

quantile(samochody$zp, c(0.05, 0.1, 0.9, 0.95))


# h)

mean(samochody$zp, na.rm = TRUE, trim = 0.05)


# Zadanie 7 ---------------------------------------------------------------

opis <- c()
opis[samochody$zp <= 7] <- 'mało'
opis[samochody$zp > 7 & samochody$zp <= 10] <- 'średnio'
opis[samochody$zp > 10] <- 'dużo'

opis2 <- cut(samochody$zp, c(-Inf, 7, 10, Inf), right = TRUE,
    labels = c('mało', 'średnio', 'dużo')
  )

barplot(table(opis))

### dplyr/ggplot ###

library(dplyr)
library(ggplot2)

samochody %>%
  mutate(opis = case_when(zp <= 7 ~ "mało",
                          zp > 7 & zp <= 10 ~ "średnio",
                          TRUE ~ "dużo")) -> samochody_opis

ggplot(samochody_opis, aes(x = opis)) +
  geom_bar()

###


# Zadanie 8 ---------------------------------------------------------------

prod <- factor(samochody$producent)
levels(prod) <-  c("Ameryka", "Europa", "Japonia")

tapply(samochody$zp, prod, mean, na.rm = TRUE)
tapply(samochody$zp, prod, var, na.rm = TRUE)
tapply(samochody$zp, prod, sd, na.rm = TRUE)

par(mfrow = c(1, 3))
tapply(samochody$zp, prod, boxplot)

par(mfrow = c(1, 1))
boxplot(zp ~ prod)


### dplyr/ggplot ###

samochody %>% 
  mutate(producent = case_when(producent == 1 ~ "Ameryka",
                               producent == 2 ~ "Europa",
                               TRUE ~ "Japonia")) %>% 
  ggplot(aes(x = factor(producent), y = zp)) + 
  geom_boxplot() +
  labs(x = "Producent",
       y = "ZP")

###


# Zadanie 9 ---------------------------------------------------------------

boxplot(samochody$zp ~ samochody$cylindry)

### ggplot ###

ggplot(samochody, aes(x = factor(cylindry), y = zp)) + 
  geom_boxplot()

###


# Zadanie 10 --------------------------------------------------------------

zp10 <- samochody$zp[samochody$waga < 2500]
mean(zp10)
median(zp10)
sd(zp10)
skewness(zp10)

### dplyr ###

samochody %>% 
  filter(waga < 2500) %>% 
  summarise(srednia = mean(zp),
            mediana = median(zp),
            sd = sd(zp),
            skewness = skewness(zp))

###


# Zadanie 11 --------------------------------------------------------------

moc11 <- samochody$moc[samochody$rok <= 81 & samochody$rok >= 79]

# a)

boxplot(moc11)

# b)

quantile(moc11, 0.95, na.rm = TRUE)

# Zadanie 12 --------------------------------------------------------------

przyspieszenie <- samochody$przysp[samochody$waga > 2500 & samochody$waga < 3000]

# a)

boxplot(przyspieszenie)

# b)

quantile(przyspieszenie, 0.75)


# Zadanie 13 --------------------------------------------------------------

waga <- samochody$waga[samochody$mpg >= 26]

# a)

boxplot(waga)

# b)

quantile(waga, 0.95, na.rm = TRUE)


# Zadanie 14 --------------------------------------------------------------

boxplot(przysp~prod)

### ggplot ###

samochody %>% 
  filter(producent != 2) %>%
  ggplot(aes(x = producent, y = przysp, group = producent)) + 
  geom_boxplot()

###


# ZESTAW ZADAŃ 3 ----------------------------------------------------------

# Zadanie 1 ---------------------------------------------------------------

x1 <- rnorm(20)
x2 <- rnorm(100)
plot(ecdf(x1))
plot(ecdf(x2), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
legend("topleft", c("N(0,1) - 20","N(0,1) - 100", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


x3 <- rnorm(200)
plot(ecdf(x1))
plot(ecdf(x3), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
legend("topleft", c("N(0,1) - 20","N(0,1) - 200", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


x4 <- rnorm(1000)
plot(ecdf(x3))
plot(ecdf(x4), add = TRUE, col = 2)
curve(pnorm(x), add = TRUE, col = 3, lw = 3)
legend("topleft", c("N(0,1) - 220","N(0,1) - 1000", "N(0,1) - teoretyczna"), lty=1, col=1:3, text.col=1)


# Zadanie 2 ---------------------------------------------------------------

# a)

N <- 500
x <- rcauchy(N)
suma <- cumsum(x)
srednia <- suma / 1:N
plot(1:N, srednia, type = "l")

med <- c()
sre <- c()
for(i in 1:N) {
  med[i] <- median(x[1:i])
  sre[i] <- mean(x[1:i])
}
plot(1:N, med, type = "l")
abline(h = 0, col = 3)
