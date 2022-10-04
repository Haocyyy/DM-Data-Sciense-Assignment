library(readr)
library(dplyr)
library(tidyverse)
library(foreign)

#Load the data
chol1 <- read.spss("data/voorbeeld7_1.sav", to.data.frame = TRUE,
                   locale = locale(encoding="ISO-8859-1"))
head(chol1)

library(ggplot2)
library(hrbrthemes)
library(forcats)
library(stringr)

#Make a scatterplot + regression line (a)
chol1_plot <- ggplot(chol1, aes(x=leeftijd, y=chol))+
  geom_point() +
  geom_smooth(method="lm",
              formula=y ~ x,
              col="red")
print(chol1_plot)

#Fit a linear model (b)
fit1 <- lm(chol~leeftijd, data=chol1)
summary(fit1)

#Fit a linear model (c)
fit2 <- lm(chol~bmi, data=chol1)
summary(fit2)

fit3 <- lm(chol~sekse, data=chol1)
summary(fit3)

fit4 <- lm(chol~alcohol, data=chol1)
summary(fit4)

#Residuals (d)
chol1$Residuals <- fit2$residuals
head(chol1)

#Histogram (d)
chol1_histogram <- ggplot(chol1, aes(x=bmi)) +
  geom_histogram()
print(chol1_histogram)
