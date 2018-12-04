library(keras)
source("simulation.R")

output.path <- "output"
dir.create(file.path("/MMCI/MS/ExpRegulation/work/nonLinearCor/", output.path), showWarnings = FALSE)

### Functions ###

buildNN <- function(activation= c("relu", "tanh", "selu", "sigmoid", "linear"), hidden.nodes= c(5, 5)){
  model <- keras_model_sequential()

  model %>% layer_dense(units = hidden.nodes[1], activation = activation, input_shape = c(7))
  for(i in seq(2, length(hidden.nodes))){
    model %>% layer_dense(units = hidden.nodes[i], activation = activation)
  }

  model %>% layer_dense(units = 1)

  model %>% compile(loss= "mean_squared_error", optimizer = optimizer_adam(), metrics= "mse")
  #model %>% compile(loss= "mean_squared_error", optimizer = optimizer_rmsprop(), metrics= "mse")

  return(model)
}
#################
#################

train.cnt <- 800
shuffle.idx <- sample(nrow(X))[seq(train.cnt)]
train_X <- X[shuffle.idx, ]
test_X <- X[-shuffle.idx, ]
train_Y <- X1[shuffle.idx]
test_Y <- X1[-shuffle.idx]

model.relu <- buildNN(activation= "relu", hidden.nodes= c(20, 10, 10, 5))
model.lin <- buildNN(activation= "linear", hidden.nodes= c(20, 10, 10, 5))
model.sigm <- buildNN(activation= "sigmoid", hidden.nodes= c(20, 10, 10, 5))

history.relu <- model.relu %>% fit(train_X, train_Y, epochs = 200, batch_size = 256, validation_split = 0.2)
history.lin <- model.lin %>% fit(train_X, train_Y, epochs = 200, batch_size = 256, validation_split = 0.2)
history.sigm <- model.sigm %>% fit(train_X, train_Y, epochs = 800, batch_size = 256, validation_split = 0.2)


model.relu %>% evaluate(test_X, test_Y)
model.lin %>% evaluate(test_X, test_Y)
model.sigm %>% evaluate(test_X, test_Y)

pdf(paste(output.path, "/NN_relu.pdf", sep= ""))
summary(model.relu)
plot(history.relu)
dev.off()

pdf(paste(output.path, "/NN_linear.pdf", sep= ""))
summary(model.lin)
plot(history.lin)
dev.off()

pdf(paste(output.path, "/NN_sigm.pdf", sep= ""))
summary(model.sigm)
plot(history.sigm)
dev.off()
