#install.packages("rpart") 
#install.packages("class")
#install.packages("e1071")
#install.packages("adabag")
#install.packages("neuralnet")
library(rpart) #decision tree
library(class) #k-nn classifier
library(e1071) #naive bayes classifier
library(adabag) #bagging and boosting with rpart
library(neuralnet) #for neural networks

# read the dataset and split it into train and test sets #
auto.df <- read.table("autompg1.csv", header=TRUE, sep=",")
auto.df$origin <- factor(auto.df$origin)


ind <- sample(1:2, nrow(auto.df), r = TRUE, pr = c(0.7, 0.3)) #approx 70% of all values will be 1 and the rest will be 0
auto.train <- auto.df[ind==1,] #select approx 70% of the rows and create the training set
auto.test <- auto.df[ind==2,] #select remaining approx 30% of the rows and create the test set


# creating formulas #
names(auto.df)
auto.formula <- origin ~ mpg + cylinders + displacement + horsepower + weight + acceleration + modelyear #specify the predictor and target variables
auto.formula <- origin ~. #practically the same with above line. includes all variables


# create a tree #
auto.tree <- rpart(auto.formula, auto.train, method="class") #train a decision tree
print(auto.tree) #see the rules of the tree
plot(auto.tree, margin = 0.2) #visualize the tree
text(auto.tree,  use.n=TRUE, all=TRUE, cex=0.5) #add labels to the tree7


auto.train.pred <- predict(auto.tree, auto.train, type="class") #predicted values for the training set
table(auto.train.pred, auto.train$origin) #the confusion matrix
auto.tree.train.accuracy <- sum(auto.train.pred == auto.train$origin)/nrow(auto.train) #overall accuracy
auto.tree.train.accuracy

auto.test.pred <- predict(auto.tree, auto.test, type="class") #predicted values for the test set
table(auto.test.pred, auto.test$origin) #the confusion matrix
auto.tree.test.accuracy <- sum(auto.test.pred == auto.test$origin)/nrow(auto.test) #overall accuracy
auto.tree.test.accuracy

# create another tree #
auto.tree2 <- rpart(auto.formula, auto.train, method="class", minsplit= 3, minbucket = 1, cp = 0.0001) #train a decision tree (which is likely to overfit due to the parameter settings)
auto.train.pred <- predict(auto.tree2, auto.train, type="class") #predicted values for the training set
table(auto.train.pred, auto.train$origin) #the confusion matrix
auto.tree2.train.accuracy <- sum(auto.train.pred == auto.train$origin)/nrow(auto.train) #overall accuracy
auto.tree2.train.accuracy

auto.test.pred <- predict(auto.tree2, auto.test, type="class") #predicted values for the test set
table(auto.test.pred, auto.test$origin) #the confusion matrix
auto.tree2.test.accuracy <- sum(auto.test.pred == auto.test$origin)/nrow(auto.test) #overall accuracy
auto.tree2.test.accuracy


#create a new tree by pruning auto.tree2
auto.tree3<- prune(auto.tree2, cp=0.02)

auto.train.pred <- predict(auto.tree3, auto.train, type="class") #predicted values for the training set
auto.tree3.train.accuracy <- sum(auto.train.pred == auto.train$origin)/nrow(auto.train) #overall accuracy
auto.tree3.train.accuracy

auto.test.pred <- predict(auto.tree3, auto.test, type="class") #predicted values for the test set
auto.tree3.test.accuracy <- sum(auto.test.pred == auto.test$origin)/nrow(auto.test) #overall accuracy
auto.tree3.test.accuracy

# create a k-nn classifier model #
auto.knn <- knn(train=auto.train, test=auto.test, cl = auto.train$origin) #predicted values for the test set
table(auto.knn, auto.test$origin) 
knn.accuracy <- sum(auto.knn == auto.test$origin) / nrow(auto.test) #overall accuracy
knn.accuracy

# create a naive bayes classifier #
auto.bayes <- naiveBayes(auto.formula, auto.train) #train the model
auto.bayes
auto.bayes.pred <- predict(auto.bayes, auto.test) #predictions for the test set
table(auto.bayes.pred, auto.test$origin) #the confusion matrix
nb.accuracy <- sum(auto.bayes.pred == auto.test$origin) / nrow(auto.test) #overall accuracy for the test set
nb.accuracy


## 06.11.2018 boosting, bagging, & classification with neural nets ##
# data was read before. training and test sets were created before. the formula was created before. packages are installed at the top.
# creating formulas #

# simple tree for comparison with bagging, boosting #
auto.rpart <- rpart(auto.formula, auto.train) #see the relevant section above for details on rpart
auto.rpart.pred <- predict(auto.rpart, auto.test, type="class")
rpart.accuracy <- sum(auto.rpart.pred == auto.test$origin) / nrow(auto.test)
rpart.accuracy #compare this with accuracy scores of other methods

# bagging with rpart #
auto.bag <- bagging(auto.formula, auto.train, mfinal=10) #create 10 trees
names(auto.bag)
auto.bag$trees #list of trees
auto.bag$votes #vote counts for each class. each rows sums up to 10.
auto.bag$prob #normalized votes
auto.bag$class #predicted class based on majority vote
auto.bag.pred <- predict(auto.bag, auto.test) #predictions for test set
bag.accuracy <- sum(auto.bag.pred$class == auto.test$origin) / nrow(auto.test) #overall accuracy for the test set
bag.accuracy #compare this with accuracy scores of other methods

# boosting with rpart #
auto.boost <- boosting(auto.formula, auto.train, mfinal = 10) #see the comments for bagging. interpretations are very similar
names(auto.boost)
auto.boost$trees
auto.boost$votes
auto.boost$prob
auto.boost$class
auto.boost.pred <- predict(auto.boost, auto.test)
boost.accuracy <- sum(auto.boost.pred$class == auto.test$origin) / nrow(auto.test) #overall accuracy for the test set
boost.accuracy #compare this with accuracy scores of other methods


# NEURAL NETWORKS #
auto.train$isUSA <- ifelse(auto.train$origin == 1, 1, 0) #differently from the previous models, we only want to predict whether produced in US or not. so, a new variable is created both for training and test sets.
auto.test$isUSA <- ifelse(auto.test$origin == 1, 1, 0)

names(auto.train)
auto.formula.nn <- isUSA ~ mpg + cylinders + displacement + horsepower + weight #we only want to use these predictors and not all

# range transformation to [0,1] for training and test sets. this is not a efficient way. a function would be desired
max <- NULL
min <- NULL
for(i in 1:7){
  max[i] <- max(auto.train[,i])
  min[i] <- min(auto.train[,i])
}

auto.trainRT <- cbind(auto.train) #RT: range transformed. a name we give.
for(i in 1:7){
  auto.trainRT[i] <- (auto.train[i] - min[i])/(max[i] - min[i])
}

max <- NULL
min <- NULL
for(i in 1:7){
  max[i] <- max(auto.test[i])
  min[i] <- min(auto.test[,i])
}

auto.testRT <- cbind(auto.test)
for(i in 1:7){
  auto.testRT[i] <- (auto.test[i] - min[i])/(max[i] - min[i])
}


auto.nn22 <- neuralnet(auto.formula.nn, auto.trainRT, hidden =c(2,2)) #build a neural network with 2 hidden layers and 2 nodes in each hidden layer
plot(auto.nn22) #visualize the neural networks
nn22.results <- compute(auto.nn22, auto.testRT[,1:5])$net.result #neural network predictions (continuous values)
nn22.pred <- ifelse(nn22.results>0.5, 1, 0) #cut at 0.5 and predict larger values as US, smaller values as not US. Values other than 0.5 can also be used.
nn22.pred #our class predictions
nn22.accuracy <- sum(nn22.pred == auto.test$isUSA) / nrow(auto.test) #overall accuracy for the test set
nn22.accuracy  #compare with the other neural network's accuracy

nn22.resultsT <- compute(auto.nn22, auto.trainRT[,1:5])$net.result #this section: accuracy calculation for the training data
nn22.predT <- ifelse(nn22.resultsT>0.5, 1, 0)
nn22.accuracyT <-sum(nn22.predT == auto.train$isUSA) / nrow(auto.train)
nn22.accuracyT #compare with the other neural network's accuracy

auto.nn242 <- neuralnet(auto.formula.nn, auto.trainRT, hidden =c(2,4,2)) #neural network with 3 hidden layers. 2,4, and 2 nodes at each hidden layer respectively.
plot(auto.nn242) #visualize the neural network
nn242.results <- compute(auto.nn242, auto.testRT[,1:5])$net.result
nn242.pred <- ifelse(nn242.results>0.5, 1, 0)
nn242.pred
nn242.accuracy <- sum(nn242.pred == auto.test$isUSA) / nrow(auto.test) #overall accuracy for the test set
nn242.accuracy #compare with the other neural network's accuracy

nn242.resultsT <- compute(auto.nn242, auto.trainRT[,1:5])$net.result #this section: accuracy calculation for the training data
nn242.predT <- ifelse(nn242.resultsT>0.5, 1, 0)
nn242.accuracyT <-sum(nn242.predT == auto.train$isUSA) / nrow(auto.train)
nn242.accuracyT #compare with the other neural network's accuracy

# we have 2 neural networks. compare their accuracy on training and tests and see if there is overfitting. if you identify an overfitting, what could be a cause of overfitting? #
