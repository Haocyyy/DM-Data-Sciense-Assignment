library(readr)
library(dplyr)
library(tidyverse)
library(foreign)
library(haven)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(lattice)
library(caret)
library(Rcpp)
library(magrittr)

#Load the data
births <- read.csv(file = "data/births.csv")
head(births)

#(a)Recode the variable 'child_birth' to new variable 'home'
births <- births %>%
  rename(home = child_birth) %>%
  mutate(home = factor(
    ifelse(home == "first line child birth, at home", "at_home", "not_at_home")
  ))

#(b)Recode the variable 'parity'  in a new variable 'pari'
births <- births %>%
  rename(pari = parity) %>%
  mutate(pari = factor(ifelse(pari == 1, "primi", "multi")))

#(c)Recode the variable 'etnicity' into a new variable 'etni'
table(births$etnicity)
births <- births %>%
  rename(etni = etnicity) %>%
  mutate(etni = factor(ifelse(etni == "Dutch", "Dutch", "Not_Dutch")))
table(births$etni)

#(d)Make a logistic regression model
births_glm <- glm(home == "at_home" ~ pari + age_cat + etni + urban,
                  data = births)

summary(births_glm)

#(e)Make a decision tree
births_tree <- rpart(home ~ pari + age_cat + etni + urban,
                     method = 'class', data = births)
rpart.plot(births_tree)
#(f) Compare logistic regression moderl and decision tree model, assess which is better

#Split data into training and test sets
set.seed(100)
mydat <- births
trainRowNumbers <- createDataPartition(mydat$home, p=0.8, list=FALSE)
trainData <- mydat[trainRowNumbers,]
testData <- mydat[-trainRowNumbers,]
#Fit the training data to logictic regression model
fit_logreg <- train(home ~ pari + age_cat + etni + urban, data = trainData,
                    method="glm", family="binomial")
#Calculate accuracy of logictic regression model
predicted_logistic <- predict(fit_logreg, testData)
confusionMatrix(reference = testData$home, data = predicted_logistic,
                mode='everything', positive='at_home')


#Fit the training data to decision tree model
fit_rpart <- train(home ~ pari + age_cat + etni + urban, data = trainData,
                   method="rpart")
#Calculate accuracy of decision tree model
predicted_tree <- predict(fit_rpart, testData)
confusionMatrix(reference = testData$home, data = predicted_tree,
                mode='everything', positive='at_home')
