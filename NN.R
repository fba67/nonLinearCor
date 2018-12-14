library(keras)
## for colMins, colMaxs
library(rpgm)
## for entropy based stat test
##library(np)
## hilbert schmidt independence test
library(dHSIC)
library(ggplot2)
library(ggthemes)
source("simulation.R")
source("../DPM/R/nldata.R")

output.path <- "output"
dir.create(file.path("/MMCI/MS/ExpRegulation/work/nonLinearCor/", output.path), showWarnings = FALSE)

### Functions ###

buildNN <- function(input_shape, activation= c("relu", "tanh", "selu", "sigmoid", "linear"), hidden.nodes= c(5, 5)){
  model <- keras_model_sequential()

  model %>% layer_dense(units = hidden.nodes[1], activation = activation, input_shape = input_shape)
  for(i in seq(2, length(hidden.nodes))){
    model %>% layer_dense(units = hidden.nodes[i], activation = activation)
  }

  model %>% layer_dense(units = 1)

  model %>% compile(loss= "mean_squared_error", optimizer = optimizer_adam(), metrics= "mse")
  #model %>% compile(loss= "mean_squared_error", optimizer = optimizer_rmsprop(), metrics= "mse")

  return(model)
}


normalizeMinMax <- function(train_X, train_Y1, train_Y2, test_X, test_Y1, test_Y2)
{
    colMin <- colMins(train_X)
    colMax <- colMaxs(train_X)
    train_X <- scale(train_X, center=colMin, scale=(colMax - colMin))
    test_X <- scale(test_X, center=colMin, scale=(colMax - colMin))
    train_Y1 <- scale(train_Y1, center=min(train_Y1), scale=(max(train_Y1) - min(train_Y1)))
    test_Y1 <- scale(test_Y1, center=min(train_Y1), scale=(max(train_Y1) - min(train_Y1)))
    train_Y2 <- scale(train_Y2, center=min(train_Y2), scale=(max(train_Y2) - min(train_Y2)))
    test_Y2 <- scale(test_Y2, center=min(train_Y2), scale=(max(train_Y2) - min(train_Y2)))
    return(list(train_X, train_Y1, train_Y2, test_X, test_Y1, test_Y2))
}

## Compute statistical test based on entropy to see if variables are the same
#computeResEntTest <- function(res1, res2)
#{
#    return(npunitest(res1, res2, boot.num=100))
#}

computeKernelTest <- function(res1, res2)
{
	return(dhsic.test(res1, res2, alpha=0.01))
}

print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
) 

#################
#################

dataplus <- nldata(nsamp=1000,sigma=0.2)

## data
data <- dataplus$data

## gold standard
gs <- dataplus$gs

res <- list()
#sapply(1:(ncol(data)-1), function(id1) {
#    sapply((id1+1):ncol(data), function(id2) {
for (id1 in 1:(ncol(data)-1))
{
	for (id2 in (id1+1):ncol(data))
	{
        ## split data into test and training, and feature and responses
        train.cnt <- 1000
        shuffle.idx <- sample(nrow(data))[seq(train.cnt)]
        train_X <- data[shuffle.idx, -c(id1,id2)]
        train_Y1 <- data[shuffle.idx, id1]
        train_Y2 <- data[shuffle.idx, id2]
        test_X <- data[-shuffle.idx, -c(id1,id2)]
        test_Y1 <- data[-shuffle.idx, id1]
        test_Y2 <- data[-shuffle.idx, id2]

        ## Normalize data
        #train_X <- scale(train_X, center= T, scale= T)
        #test_X <- scale(test_X, center= colMeans(train_X), scale= apply(train_X, 2, FUN= sd))
        #
        #train_Y <- scale(train_Y, center= T, scale= T)
        #test_Y <- scale(test_Y, center= mean(train_Y), scale= sd(train_Y))
        #normalized_data <- normalizeMinMax(train_X, train_Y1, train_Y2, test_X, test_Y1, test_Y2)
        normalized_data <- normalizeMinMax(train_X, train_Y1, train_Y2, test_X, train_Y1, train_Y2)
        train_X <- normalized_data[[1]]
        train_Y1 <- normalized_data[[2]]
        train_Y2 <- normalized_data[[3]]
        test_X <- normalized_data[[4]]
        test_Y1 <- normalized_data[[5]]
        test_Y2 <- normalized_data[[6]]
        #################
        
        print(head(train_X))
        print(head(test_X))


        #model.relu <- buildNN(activation= "relu", hidden.nodes= c(10, 5))
        #model.lin <- buildNN(activation= "linear", hidden.nodes= c(10, 5))
        model.sigm1 <- buildNN(dim(train_X)[2], activation= "relu", hidden.nodes= c(20, 15, 10))
        model.sigm2 <- buildNN(dim(train_X)[2], activation= "relu", hidden.nodes= c(20, 15, 10))

        #history.relu <- model.relu %>% fit(train_X, train_Y, epochs = 200, batch_size = 256, validation_split = 0.2)
        #history.lin <- model.lin %>% fit(train_X, train_Y, epochs = 200, batch_size = 256, validation_split = 0.2)
        # The patience parameter is the amount of epochs to check for improvement.
		early_stop <- callback_early_stopping(monitor = "val_loss", patience = 100)
        history.sigm1 <- model.sigm1 %>% fit(train_X, train_Y1, epochs = 800, batch_size = 32, validation_split = 0.2, callbacks = list(early_stop, print_dot_callback))
        history.sigm2 <- model.sigm2 %>% fit(train_X, train_Y2, epochs = 800, batch_size = 32, validation_split = 0.2, callbacks = list(early_stop, print_dot_callback))


        #model.relu %>% evaluate(test_X, test_Y)
        #model.lin %>% evaluate(test_X, test_Y)
        #model.sigm1 %>% evaluate(test_X, test_Y1)
        #model.sigm2 %>% evaluate(test_X, test_Y2)

        #pdf(paste(output.path, "/NN_relu.pdf", sep= ""))
        #summary(model.relu)
        #plot(history.relu)
        #dev.off()

        #pdf(paste(output.path, "/NN_linear.pdf", sep= ""))
        #summary(model.lin)
        #plot(history.lin)
        #dev.off()

        ## compute residuals
        pred1 <- model.sigm1 %>% predict(train_X)
        residual1 <- train_Y1 - pred1
        pred2 <- model.sigm2 %>% predict(train_X)
        residual2 <- train_Y2 - pred2
        statT <- computeKernelTest(residual1, residual2)
        print(statT$p.value)
        res[[length(res)+1]] <- list(id1=id1, id2=id2, HSIT=statT)



        pdf(paste(output.path, "/NN_sigm",colnames(data)[id1],"_comb",colnames(data)[id1],"+",colnames(data)[id2],".pdf", sep= ""))
        summary(model.sigm1)
        print(plot(history.sigm1))
        print(ggplot(data.frame(id1 = train_Y1, id2 = train_Y2), aes(x=id1, y=id2)) + geom_point() + coord_equal() + theme_tufte())
        print(ggplot(data.frame(res1 = residual1, res2 = residual2), aes(x=res1, y=res2)) + geom_point() + coord_equal() + theme_tufte())
        print(ggplot(data.frame(id1 = train_Y1, pred = pred1), aes(x=id1, y=pred)) + geom_point() + coord_equal() + theme_tufte())
        dev.off()
        pdf(paste(output.path, "/NN_sigm",colnames(data)[id2],"_comb",colnames(data)[id1],"+",colnames(data)[id2], ".pdf", sep= ""))
        summary(model.sigm2)
        print(plot(history.sigm2))
        print(ggplot(data.frame(id1 = train_Y1, id2 = train_Y2), aes(x=id1, y=id2)) + geom_point() + coord_equal() + theme_tufte())
        print(ggplot(data.frame(res1 = residual1, res2 = residual2), aes(x=res1, y=res2)) + geom_point() + coord_equal() + theme_tufte())
        print(ggplot(data.frame(id2 = train_Y2, pred = pred2), aes(x=id2, y=pred)) + geom_point() + coord_equal() + theme_tufte())
        dev.off()
        
	}
}


save(res, file= "res.RData")

#        }
#    )
#    }
#)

sig.thresh <- 0.05
sigs <- NULL;
for(i in seq(length(res))){
  sigs <- c(sigs, res[[i]]$HSIT$p.value <= sig.thresh)
}

sig.idx <- which(sigs == T)

pdf("signif_data.pdf")
for(i in seq(length(sig.idx))){
  a <- data[, res[[sig.idx[i]]]$id1];
  b <- data[, res[[sig.idx[i]]]$id2];
  plot(a, b, pch= 20, main= paste(res[[sig.idx[i]]]$id1, res[[sig.idx[i]]]$id2, sep= "..."))
}
dev.off()
