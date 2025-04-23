### Higher Education and Drug Consumption Is there a link between the two? Explore along the years.###

#RScript by Maria Liova (3247034) and Savvina Salvaridou (3230288)

#Setup 
#"File" -> "Import Dataset" -> "From Excel" -> View 4 FINAL Excels

library(readxl)
FINAL_Book1_r2005 <- read_excel("C:/Users/Savvina Salvaridou/Downloads/R Project - Final/All Data/Altered - Final Data/FINAL Book1-r2005.xlsx")
View(FINAL_Book1_r2005)

library(readxl)
FINAL_Book1_r2020 <- read_excel("C:/Users/Savvina Salvaridou/Downloads/R Project - Final/All Data/Altered - Final Data/FINAL Book1-r2020.xlsx")
View(FINAL_Book1_r2020)

library(readxl)
FINAL_Book2_r2005 <- read_excel("C:/Users/Savvina Salvaridou/Downloads/R Project - Final/All Data/Altered - Final Data/FINAL Book2-r2005.xlsx")
View(FINAL_Book2_r2005)

library(readxl)
FINAL_Book2_r2020 <- read_excel("C:/Users/Savvina Salvaridou/Downloads/R Project - Final/All Data/Altered - Final Data/FINAL Book2-r2020.xlsx")
View(FINAL_Book2_r2020)

# Univariate analysis - Centrality 

hist(FINAL_Book1_r2005$`% of people who are highly educated`, freq=FALSE)

hist(FINAL_Book1_r2020$`% of people who are highly educated`, freq=FALSE)

hist(FINAL_Book2_r2005$`per 100 people with the same characteristics`, freq=FALSE)

hist(FINAL_Book2_r2020$`per 100 people`, freq=FALSE)

# Univariate analysis - Variability

mean(FINAL_Book1_r2005$`% of people who are highly educated`)

mean(FINAL_Book1_r2020$`% of people who are highly educated`)

mean(FINAL_Book2_r2005$`per 100 people with the same characteristics`)

mean(FINAL_Book2_r2020$`per 100 people`)

max(FINAL_Book1_r2005$`% of people who are highly educated`)-min(FINAL_Book1_r2005$`% of people who are highly educated`)

max(FINAL_Book1_r2020$`% of people who are highly educated`)-min(FINAL_Book1_r2020$`% of people who are highly educated`)

max(FINAL_Book2_r2005$`per 100 people with the same characteristics`)-min(FINAL_Book2_r2005$`per 100 people with the same characteristics`)

max(FINAL_Book2_r2020$`per 100 people`)-min(FINAL_Book2_r2020$`per 100 people`)

quantile(FINAL_Book1_r2005$`% of people who are highly educated`,prob=c(0.25,0.5,0.75))

quantile(FINAL_Book1_r2020$`% of people who are highly educated`,prob=c(0.25,0.5,0.75))

quantile(FINAL_Book2_r2005$`per 100 people with the same characteristics`,prob=c(0.25,0.5,0.75))

quantile(FINAL_Book2_r2020$`per 100 people`,prob=c(0.25,0.5,0.75))

IQR(FINAL_Book1_r2005$`% of people who are highly educated`)

IQR(FINAL_Book1_r2020$`% of people who are highly educated`)

IQR(FINAL_Book2_r2005$`per 100 people with the same characteristics`)

IQR(FINAL_Book2_r2020$`per 100 people`)

var(FINAL_Book1_r2005$`% of people who are highly educated`)

var(FINAL_Book1_r2020$`% of people who are highly educated`)

var(FINAL_Book2_r2005$`per 100 people with the same characteristics`)

var(FINAL_Book2_r2020$`per 100 people`)

sd(FINAL_Book1_r2005$`% of people who are highly educated`)

sd(FINAL_Book1_r2020$`% of people who are highly educated`)

sd(FINAL_Book2_r2005$`per 100 people with the same characteristics`)

sd(FINAL_Book2_r2020$`per 100 people`)

sd(FINAL_Book1_r2005$`% of people who are highly educated`)/mean(FINAL_Book1_r2005$`% of people who are highly educated`)

sd(FINAL_Book1_r2020$`% of people who are highly educated`)/mean(FINAL_Book1_r2020$`% of people who are highly educated`)

sd(FINAL_Book2_r2005$`per 100 people with the same characteristics`)/mean(FINAL_Book2_r2005$`per 100 people with the same characteristics`)

sd(FINAL_Book2_r2020$`per 100 people`)/mean(FINAL_Book2_r2020$`per 100 people`)

boxplot(FINAL_Book1_r2005$`% of people who are highly educated`)

boxplot(FINAL_Book1_r2020$`% of people who are highly educated`)

boxplot(FINAL_Book2_r2005$`per 100 people with the same characteristics`)

boxplot(FINAL_Book2_r2020$`per 100 people`)

# Bivariate analysis - Association

cor(FINAL_Book1_r2005$`% of people who are highly educated`, FINAL_Book2_r2005$`per 100 people with the same characteristics`)

cor(FINAL_Book1_r2020$`% of people who are highly educated`, FINAL_Book2_r2020$`per 100 people`)

cov(FINAL_Book1_r2005$`% of people who are highly educated`, FINAL_Book2_r2005$`per 100 people with the same characteristics`)

cov(FINAL_Book1_r2020$`% of people who are highly educated`, FINAL_Book2_r2020$`per 100 people`)

plot(FINAL_Book1_r2005$`% of people who are highly educated`, FINAL_Book2_r2005$`per 100 people with the same characteristics`)

plot(FINAL_Book1_r2020$`% of people who are highly educated`, FINAL_Book2_r2020$`per 100 people`)

# Bivariate analysis - Simple linear regression model

beta1_1 = cov(FINAL_Book1_r2005, FINAL_Book2_r2005) / var(FINAL_Book1_r2005)
beta1_1
beta1_2 = cov(FINAL_Book1_r2020, FINAL_Book2_r2020) / var(FINAL_Book1_r2020)
beta1_2

beta0_1 = mean(FINAL_Book2_r2005) - beta1_1 * mean(FINAL_Book1_r2005)
beta0_1
beta0_2 = mean(FINAL_Book2_r2020) - beta1_2 * mean(FINAL_Book1_r2020)
beta0_2

plot(FINAL_Book1_r2005$`% of people who are highly educated`, FINAL_Book2_r2005$`per 100 people with the same characteristics`)
abline(lm(FINAL_Book2_r2005$`per 100 people with the same characteristics`~FINAL_Book1_r2005$`% of people who are highly educated`), col="red", lwd=3)

plot(FINAL_Book1_r2020$`% of people who are highly educated`, FINAL_Book2_r2020$`per 100 people`, col="black")
abline(lm(FINAL_Book2_r2020$`per 100 people`~FINAL_Book1_r2020$`% of people who are highly educated`), col = "red", lwd = 3)
