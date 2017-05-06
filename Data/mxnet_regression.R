library(caret)
library(mxnet)

#Reading the cleaned data
train <- read.csv("transformed train.csv")
test <- read.csv("transformed test.csv")
test1 <- data.matrix(test[,-2])

#Dividing training data into train and validation
ind <- createDataPartition(train$SalePrice, p = 0.6, list = F)
train.x <- data.matrix(train[ind, -2])
train.y <- train[ind, 2]
valid.x <- data.matrix(train[-ind, -2])
valid.y <- train[-ind,2]


#Creating the network with 3 hidden layers with 300 neuorns each
data <- mx.symbol.Variable("data")
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=300)
act1 <- mx.symbol.Activation(fc1, name="sigm1", act_type="sigmoid")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=300)
act2 <- mx.symbol.Activation(fc2, name="sigm2", act_type="sigmoid")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=300)
act3 <- mx.symbol.Activation(fc3, name="sigm3", act_type="sigmoid")
fc4 <- mx.symbol.FullyConnected(act3, name="fc4", num_hidden=1)
linear_reg_output <- mx.symbol.LinearRegressionOutput(fc4)


mx.set.seed(123)
model <- mx.model.FeedForward.create(symbol = linear_reg_output,
                                     X = train.x, 
                                     y = train.y, 
                                     ctx = mx.cpu(),
                                     num.round = 200,
                                     array.batch.size = 50,
                                     learning.rate = 1e-6,
                                     eval.data = list(data = valid.x, label = valid.y),
                                     #momentum = 0.05,
                                     #optimizer = "adam", 
                                     #optimizer = "rmsprop",
                                     eval.metric = mx.metric.rmse)

preds <- predict(model, valid.x)
preds
sqrt(mean((preds-valid.y)^2))

#Used when optimizer is 'adam'
preds_adam <- predict(model, valid.x)
preds_adam
sqrt(mean((preds_adam-valid.y)^2))

#Used when optimizer is 'rmsprop'
preds_rmsprop <- predict(model, valid.x)
preds_rmsprop
sqrt(mean((preds_rmsprop-valid.y)^2))

#predicting the test set
preds <- predict(model,test1)
preds1<- t(preds)

submission <- read.csv("sample_submission.csv")
submission$SalePrice <- preds1
write.csv(submission,file = "mxnet_cnn1.csv",row.names = F)



##############################################
model1 <- mx.mlp(data = train.x, 
                 label = train.y, 
                 hidden_node = 1, 
                 out_node = 1, 
                 num.round = 200,
                 learning.rate = 0.09,
                 momentum = 0.9,
                 eval.metric = mx.metric.rmse,
                 out_activation = "rmse")

preds <- predict(model1, valid.x)
