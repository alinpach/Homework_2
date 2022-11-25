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
