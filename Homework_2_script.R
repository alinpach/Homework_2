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
X <- as.matrix(df)
X

## 2d #############################################

y <- c(andy$sales)
beta <- (solve((t(X)%*%X))%*%t(X))%*%y
beta

summary(andy_lm_ads_sq)

## 2e #############################################

u_hat <- c(andy_lm_ads_sq$residuals)
u_hat

# y = y_hat - u_hat
y_hat <- c(y-u_hat)
y_hat

# t(y_hat) * u_hat = 0
t(y_hat)%*%u_hat # approximately equal to 0

## 2f #############################################

# SSR = t(u_hat)*u_hat
SSR <- t(u_hat)%*%u_hat

# SST = t(y)*y - n*(mean(y))^2
# n = number of observations
SST<- t(y)%*%y-75*(mean(y))^2

# R2 = 1 - SSR/SST
R2 <- 1-SSR/SST
R2

# R2_ad = 1 - (SSR/(n-k-1))/(SST/(n-1))
# k = number of variables
R2_ad <- 1-(SSR/(75-3-1))/(SST/(75-1))
R2_ad

summary(andy_lm_ads_sq)

## 2g #############################################

# estimate sigma squared
sigma_hat_sq <- c(SSR/(75-3-1))

# VC = sigma^2 * solve((t(X) * X))
VC <- sigma_hat_sq*(solve(t(X)%*%X))
VC

# diagonal elements
VC_diag <- diag(VC)
sqrt(VC_diag)

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

# H1 = beta_exper - beta_tenure != 0
wage_log_coefficiants <- as.matrix(wage_log_reg1$coefficients)
wage_log_coefficiants

beta_exper <- wage_log_coefficiants[3]
beta_tenure <- wage_log_coefficiants[4]

se_exper <- coef(summary(wage_log_reg1))["exper", "Std. Error"]
se_exper

t <- (beta_exper - beta_tenure)/se_exper
t

t_statistic <- abs(t)
c.0025 <- 1.96
if((t_statistic > c.0025)) {
  print("reject H_0")
  } else
    print("do not reject H_0")


## 3d #############################################


## 3e #############################################
wage2$expersq <- (wage2$exper)^2
wage2$tenuresq <- (wage2$tenure)^2
view(wage2)

wage_log_reg2 <- lm(log(wage) ~ educ + exper + tenure + expersq + tenuresq, data = wage2)
summary(wage_log_reg2)

# H1 = beta4 - beta5 != 0
wage_log_coefficiants_2 <- as.matrix(wage_log_reg2$coefficients)
wage_log_coefficiants_2

beta_4 <- wage_log_coefficiants_2[5]
beta_5 <- wage_log_coefficiants_2[6]

se_expersq <- coef(summary(wage_log_reg2))["expersq", "Std. Error"]
se_expersq

t2 <- (beta_4 - beta_5)/se_expersq
t2

t2_statistic <- abs(t)
c.0025 <- 1.96
if((t2_statistic > c.0025)) {
  print("reject H_0")
} else
  print("do not reject H_0")


