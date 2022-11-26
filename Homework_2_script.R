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
view(andy)

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

cor(andy$price,andy$advert)
cor(andy$price,andy$advert_sq)
cor(andy$advert,andy$advert_sq)

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
# A 1 unit increase in tenure is associated with a 1.34% increase in wage when holding other variables constant

## 3b #############################################


## 3c #############################################


## 3d #############################################
wage2$expersq <- (wage2$exper)^2
wage2$tenuresq <- (wage2$tenure)^2
view(wage2)

## 3e #############################################
wage_log_reg2 <- lm(log(wage) ~ educ + exper + tenure + expersq + tenuresq, data = wage2)
summary(wage_log_reg2)
>>>>>>> da7c922f3d079ec0bf940590609eaad7bb677e6c
