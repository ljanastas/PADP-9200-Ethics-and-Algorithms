library(pacman)

# This loads and installs the packages you need at once
pacman::p_load(tm,SnowballC,foreign,plyr,
               twitteR,slam,foreign,wordcloud,LiblineaR,e1071,
               caret,rpart, quanteda, rpart.plot)


############ Training and testing ########################################
source("http://www.openintro.org/stat/data/cdc.R")
# Divide into train and test with a 70/30 split
#set.seed(33)

#train=sample(1:dim(dfm_mat)[1],   # Creating an index with each of the observation numbers and randomly shuffling them using a 70/30 split
#             dim(dfm_mat)[1]*0.7)
#trainX = dfm_mat[train,] # Defines training data
#testX = dfm_mat[-train,] # Defines test data
#trainY = viraltweets[train]
#testY = viraltweets[-train]

#traindata<-data.frame(trainY,trainX)
#testdata<-data.frame(testY,testX)

# Take training and test data from Problem Set 2 to predict smoker status using random forests

library(ranger)

############ Random Forest with Ranger ########################################

rf_fit<-ranger(factor(trainY)~., data=traindata, 
                                 importance='impurity',
                                 write.forest=TRUE,
                                 probability=TRUE)


################################################################################################
################################################################################################
####### Draw the trees #########################################################################
################################################################################################
################################################################################################
################################################################################################

trees=rpart(factor(trainY)~., traindata)
rpart.plot(trees)


################################################################################################
################################################################################################
####### Performance######## ####################################################################
################################################################################################
################################################################################################
################################################################################################


# With ranger we have to generate the predicted probabilities and classify the tweets ourselves
rf_probs<-predict(rf_fit,data.frame(testdata))

rf_class<-ifelse(rf_probs$predictions[,2] > 0.5, 1,0)

predicted_class = factor(rf_class)
true_class = factor(testdata$testY)

cmat = confusionMatrix(predicted_class,true_class, positive = "1")
cmat


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
  ggtitle("Word Importance Plot")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
















