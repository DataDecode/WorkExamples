library(rpart) # classification and regression trees (CART)
library(randomForest) # random forests of CARTs
library(pROC) # evaluation function from before

#testing for type
f_type<- as.formula('type ~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar + chlorides + 
               free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol + quality')

type_tree <- rpart(f_type, method="class", data=wine_training)

#plotting 
plot(type_tree, uniform=TRUE, 
       +      main="Classification Tree for Survival on the Titanic")

text(type_tree, use.n=TRUE, all=TRUE, cex=.7)

#predicting
# This is the same data the model saw when it was training
type_training_pred <- predict(type_tree, newdata=wine_training, type="class")

# this is our held-out data
type_test_pred <- predict(type_tree, newdata=wine_test, type="class")

type_training_pred <- list(class_predictions=training_pred, 
                            eval=evaluate(actual = titanic_training$Survived, 
                                          predicted = training_pred))

#PRUNE TREE?
#What Level do you use for pruning a tree?

#testing for quality
f<- as.formula('quality ~ fixed.acidity + volatile.acidity + cirtric.acid + residual.sugar + chlorides + 
               free.sulfur.dioxide + total.sulfur.dioxide + density + pH + sulphates + alcohol + type')