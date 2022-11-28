# Script Homework_2
# due 12/05/2022
# Iby 11701349
# Pacher 12026049


###################################################
# Exercise 1
###################################################




###################################################
# Exercise 2
###################################################

library(tidyverse)
library(haven)

andy <- read_dta("andy.dta")
andy

## 2a #############################################

# Regression model: x1 = price, x2 = advertising
andy_lm <- lm(sales ~ price + advert, data = andy)
summary(andy_lm)

# advertising^2
andy$advert_sq <- (andy$advert)^2
view(andy)

# Regression model: x1 = price, x2 = advertising, x3 = advertising^2
andy_lm_ads_sq <- lm(sales ~ price + advert + advert_sq, data = andy)
summary(andy_lm_ads_sq)

cor(andy$price, andy$advert)
cor(andy$price, andy$advert_sq)
cor(andy$advert, andy$advert_sq)

# The coefficients for price hardly differ from the two models as advertising and advertising squared hardly effect price.
# The coefficients for advertising differ a lot between the two models as advertising and advertising squared are highly correlated.

## 2b #############################################

sal_ad2_reg <- lm(sales ~ advert_sq, data = andy)
summary(sal_ad2_reg)
andy$sales_res <- sal_ad2_reg$residuals

pri_ad2_reg <- lm(price ~ advert_sq, data = andy)
summary(pri_ad2_reg)
andy$price_res <- pri_ad2_reg$residuals

adv_ad2_reg <- lm(advert ~ advert_sq, data = andy)
summary(adv_ad2_reg)
andy$advert_res <- adv_ad2_reg$residuals

res_reg <- lm(andy$sales_res ~ andy$price_res + andy$advert_res-1, data = andy)
summary(res_reg)

## 2c #############################################

df <- data.frame(rep(1,75), andy$price, andy$advert, andy$advert_sq)
X<- as.matrix(df)
X

## 2d #############################################

y <- c(andy$sales)
beta <- (solve((t(X)%*%X))%*%t(X))%*%y
beta
summary(andy_lm_ads_sq)

## 2e #############################################

u_hat <- c(andy_lm_ads_sq$residuals)
# y=y_hat-u_hat
y_hat <- c(y-u_hat)
# t(y_hat)*u_hat = 0
t(y_hat)%*%u_hat # approximatly equal to 0

## 2f #############################################

# R_sq = 1 - SSR/SST
# SSR = t(u_hat)*u_hat
# SST = t(y)*y - n*(mean(y))^2
# R_sq_ad = 1 - (1-R_sq)(n-1)/(n-k-1)

SSR <- t(u_hat)%*%u_hat
SST<- t(y)%*%y-75*(mean(y))^2
R_sq <- 1-SSR/SST
R_sq_ad <- 1-(1-R_sq)*74/71

R_sq
R_sq_ad
summary(andy_lm_ads_sq)

## 2g #############################################

# VC_M = sigma^2solve((t(X)*X))
sigma_hat_sq <- c(SSR/71)
VC_M <- sigma_hat_sq*(solve(t(X)%*%X))
VC_M
VC_M_diag <- diag(VC_M)
sqrt(VC_M_diag)
summary(andy_lm_ads_sq)

###################################################
# Exercise 3
###################################################

install.packages("wooldridge")
library(wooldridge)

data("wage2")
view(wage2)

## 3a #############################################

wage_log_reg1 <- lm(log(wage) ~ educ + exper + tenure, data = wage2)
summary(wage_log_reg1)

# A 1 unit increase in education is associated with a 7.49% increase in wage when holding other variables constant.
# A 1 unit increase in experience is associated with a 1.53% increase in wage when holding other variables constant.
# A 1 unit increase in tenure is associated with a 1.34% increase in wage when holding other variables constant.

## 3b #############################################


## 3c #############################################


## 3d #############################################
wage2$expersq <- (wage2$exper)^2
wage2$tenuresq <- (wage2$tenure)^2
view(wage2)

## 3e #############################################
wage_log_reg2 <- lm(log(wage) ~ educ + exper + tenure + expersq + tenuresq, data = wage2)
summary(wage_log_reg2)
