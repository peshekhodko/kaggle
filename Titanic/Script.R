setwd("/Users/NataliyaPeshekhodko/Documents/KaggleCompetion/Titanic/data")

library(rpart)
library(randomForest)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(party)

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors = FALSE)

test$Survived <- 0

all_data = rbind (train, test)
all_data$title = sapply (all_data$Name, FUN=function(x) {strsplit(x, split = '[,.]')[[1]][2] } )


all_data$title [ all_data$title %in% c('Mme', 'Mlle') ] <- 'Mlle'
all_data$title [ all_data$title %in% c('Capt', 'Don', 'Major', 'Sir') ] <- 'Sir'
all_data$title [ all_data$title %in% c('Dona', 'Lady', 'the Countess', 'Jonkheer') ] <- 'Lady'

all_data$FamilySize <- all_data$SibSp + all_data$Parch + 1


all_data$Cab_let = sapply (all_data$Cabin, FUN=function(x) {substr(x, start=1, stop = 1)})


all_data$Fare[which(is.na(all_data$Fare))] = median (all_data$Fare, na.rm = TRUE)
all_data$Embarked[62] = "S"
all_data$Embarked[830] = "S"
all_data$Fare [all_data$Fare == 0] = median (all_data$Fare)


cabin_fit <- rpart (Cab_let ~ Fare + Pclass, data = all_data[!is.na(all_data$Cab_let), ], method = 'anova')
all_data$Cab_let[is.na(all_data$Cab_let)] <- predict(cabin_fit, all_data[is.na(all_data$Cab_let),])

all_data$Embarked <- as.factor(all_data$Embarked)
all_data$Survived = as.factor(all_data$Survived)
all_data$Sex = as.factor(all_data$Sex) 

age_fit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + title + FamilySize,
                data=all_data[!is.na(all_data$Age),], 
                method="anova")
all_data$Age[is.na(all_data$Age)] <- predict(age_fit, all_data[is.na(all_data$Age),])


train <- all_data[1:891,]
test <- all_data[892:1309,]

# Decision tree
fit_tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + FamilySize, data=train, method="class")
prediction_tree <- predict(fit_tree, test, type = "class")

fancyRpartPlot(fit_tree)

# Random Forests
set.seed(400)
fit_rf <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + FamilySize,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)
varImpPlot(fit_rf)
prediction_rf <- predict(fit_rf, test)

# Random forest (statistical test)
fit_rf_st <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + FamilySize,
                 data = train, 
                 controls=cforest_unbiased(ntree=2000, mtry=3))
prediction_rf_st <- predict(fit_rf_st, test, OOB=TRUE, type = "response")
