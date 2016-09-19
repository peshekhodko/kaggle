
library(randomForest)
library(readr)

# Random Forest algorithm
set.seed(0)

numTrain <- 10000
numTrees <- 50

train <- read_csv("train.csv")
test <- read_csv("test.csv")

rows <- sample(1:nrow(train), numTrain)
labels <- as.factor(train[rows,1])
train <- train[rows,-1]

rf <- randomForest(train, labels, ntree=numTrees)
predictions <- data.frame(ImageId=1:nrow(test), predict(fit_rf, test))
colnames (predictions) = c("ImageId", "Label")
write_csv(predictions, "dig_rec_4.csv")

# Neuralnet library

library (neuralnet)

train$label = as.factor(train$label)
frla <- paste(colnames(train)[2:NCOL(train)], collapse = ' + ')
fo <- as.formula(paste('label', '~', frla)) 


ffff <- neuralnet (fo, data=train, hidden = 4, lifesign = "minimal", linear.output = FALSE, threshold = 0.1)
ffff.results = compute (ffff, test)
results = data.frame (Label = ffff.results$net.result)
head (results)


# H2o library

library (h2o)
h2o.init(nthreads = -1)

train$label = as.factor(train$label)

train_h2o = as.h2o(train)
test_h2o = as.h2o(test)

s <- proc.time()

model =
  h2o.deeplearning(x = 2:785,                           # column numbers for predictors
                   y = 1,                               # column number for label
                   training_frame = train_h2o,          # data in H2O format
                   activation = "RectifierWithDropout", # algorithm
                   input_dropout_ratio = 0.2,           # % of inputs dropout
                   hidden_dropout_ratios = c(0.5,0.5),  # % for nodes dropout
                   balance_classes = TRUE, 
                   hidden = c(100,100),                 # two layers of 100 nodes
                   momentum_stable = 0.99,
                   nesterov_accelerated_gradient = T,   # use it for speed
                   epochs = 15)                         # no. of epochs

h2o.confusionMatrix(model)

s - proc.time()

h2o_y_test <- h2o.predict(model, test_h2o)
df_y_test = as.data.frame(h2o_y_test)
df_y_test = data.frame(ImageId = seq(1,length(df_y_test$predict)), Label = df_y_test$predict)
write.csv(df_y_test, file = "dig_rec_8.csv", row.names=F)

h2o.shutdown(prompt = F)