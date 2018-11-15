
bcdata <- read.table("G:/bcdata.csv",sep=",",header=TRUE,na.strings="?")
#
bcdata
names(bcdata)
# 
df <- bcdata[-1]
#The benign and malignant are the names of the cancer
df$class<- factor(df$class,levels=c(2,4),labels=c("benign","malignant"))
#The intake of 70 percent of values randomly
set.seed(1234)
#70 percent of data will be taken for train for developing a model, 
train<- sample(NROW(df),0.7*NROW(df))
#The trained data set will be placed in the object df.train
df.train <- df[train,]
#Remaining 30 percentage of the data will be used for testing and that will be placed in the df.validate object
df.validate <- df[-train,]
#Creating a confusion matrix to identify the accuracy we can use the table function
table(df.train$class)
table(df.validate$class)
#Logistic regression - Model

# generalised linear model is being used for trained data set
fit.logit<- glm(class~.,data=df.train,family=binomial())
#result analysis of the model to get displayed
summary(fit.logit)
#Testing the model using test data
prob <- predict(fit.logit,df.validate,type="response")
#we are setting the probability value 
logit.pred <- factor(prob >.5,levels=c(FALSE,TRUE),label=c("bengin","malignant"))
# Using confusion matrix we can set the accracy for test data also
logit.pref <- table(df.validate$class,logit.pred,dnn=c("Actual","Predicted"))
# Displaying the result
logit.pref


#Decision trees - visualizing the result

#rpart is a package required to generate a decision tree
library(rpart)
set.seed(1234)
# The splitting criteria will be decided by r part in the decision tree
# As an argument the model developed by us should be given
dtree <- rpart(class~ .,data=df.train,method="class",parms=list(split="information"))

dtree$cptable
plotcp(dtree)
dtree.pruned <- prune(dtree,cp=.0125)
install.packages("rpart.plot")
library(rpart.plot)

prp(dtree.pruned,type=2,extra=104,fallen.leaves=TRUE,main="Decision Tree")





