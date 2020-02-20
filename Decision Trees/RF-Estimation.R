library(pacman)

# This loads and installs the packages you need at once
pacman::p_load(tm,SnowballC,foreign,plyr,
               twitteR,slam,foreign,wordcloud,LiblineaR,e1071,
               caret,rpart, quanteda, rpart.plot)


############ Training and testing ########################################
source("http://www.openintro.org/stat/data/cdc.R")

# Truncate the cdc dataset to 1000 observations
cdc.trunc = cdc[1:1000,]

# Take training and test data from Problem Set 2 to predict smoker status using random forests

set.seed(33)

index = sample(nrow(cdc.trunc), 0.8* nrow(cdc.trunc))# Generates the randomized indices.
# Create the training data
smoker_train = cdc.trunc[index,] # Training data
smoker_test = cdc.trunc[-index,] # Test data

# Did it work?
dim(smoker_train)
dim(smoker_test)

library(ranger)

############ Random Forest with Ranger ########################################

# Training the algorithm

rf_fit<-ranger(factor(smoke100) ~ ., data=smoker_train, 
                                 importance='impurity',
                                 write.forest=TRUE,
                                 probability=TRUE)


################################################################################################
################################################################################################
####### Draw the trees #########################################################################
################################################################################################
################################################################################################
################################################################################################

trees=rpart(factor(smoke100)~., smoker_train)
rpart.plot(trees)


################################################################################################
################################################################################################
####### Performance######## ####################################################################
################################################################################################
################################################################################################
################################################################################################


# (2) take the trained model "rf_fit" and predict smoking status (smoke100) on the 
# test data

rf_probs<-predict(rf_fit,data.frame(smoker_test))

# Classifying the people in the test data into smoker or non-smoker based on
# the predicted probabilities from rf_probs

rf_class<-ifelse(rf_probs$predictions[,2] > 0.5, 1,0)

predicted_class = factor(rf_class)
true_class = factor(smoker_test$smoke100)

cmat = confusionMatrix(predicted_class,true_class, positive = "1")
cmat

# Precision, recall and F1
precision = cmat$byClass[5]
recall = cmat$byClass[6]
F1 = cmat$byClass[7]

################################################################################################
################################################################################################
####### Variable Importance ####################################################################
################################################################################################
################################################################################################
################################################################################################

# Let's extract the variable importance

varimp = rf_fit$variable.importance

# We can create a variable importance plot
# but it's a bit tricky

# Extract the importance scores
words<-names(varimp)
importance<-as.vector(varimp)

# Create a data frame with both
importance.data = data.frame(words,importance)

# Now we need to reorder the data frame in descending order
# and only choose the top few words, let's say 20

importance.data = importance.data[order(-importance.data$importance),]

# Now we can use ggplot2 to create the plot
# Plot variable importance 
ggplot(importance.data, 
       aes(x=reorder(words,importance), y=importance,fill=importance))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Variable Importance Plot for Predicting Smoking Status")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")

setwd('/Users/jasona/Dropbox/Teaching Spring 2020/PADP 9200 | SP20/Decision Trees')
ggsave("importanceplot.png")














