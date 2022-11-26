# Script Homework_2
# due 12/05/2022
# Iby 11701349
# Pacher 12026049


library("tidyverse")

library(haven)
andy <- read_dta("andy.dta")
view(andy)


# 2 a)
# Regression model: x1 = price, x2 = advertising
andy_lm <- lm(sales ~ price + advert, data = andy)
summary(andy_lm)

plot(andy_lm)

# advertising^2
advert_sq <- andy$advert^2
advert_sq
andy$advert

# Regression model: x1 = price, x2 = advertising, x3 = advertising^2
andy_lm_ads_sq <- lm(sales ~ price + advert + advert_sq, data = andy)
summary(andy_lm_ads_sq)

plot(andy_lm_ads_sq)

cor(andy$price,andy$advert)
cor(andy$price,advert_sq)
cor(andy$advert,advert_sq)

# The coefficients for price hardly differ from the two models as advertising and advertising squared hardly effect price.
# The coefficients for advertising differ a lot between the two models as advertising and advertising squared are highly correlated.


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
