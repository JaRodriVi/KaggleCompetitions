# Libraries
#install.packages('dplyr')
library(dplyr)


# Read train and test of Titanic.
train <- read.csv('train.csv')
test <- read.csv('test.csv')
test$Survived <- NA
full <- rbind(train, test)

# First we have to explore our Data
str(train)
str(train$Name)
length(train$Name)
str(train$Pclass)
str(train$Sex)
str(train$Embarked)

lapply(train, function(x) length(unique(x)))

# We can see that Name is a Factor and that makes no sense
# Other data  that we can observe is that the variable of Pclass is and int
# and it is actually a Factor and Embarked has 4 Factor, one of them it is "",
# the empty string maybe we have to clean it

columns <- colnames(train)
lapply(train, function(x) sum(is.na(x)))
lapply(train, function(x) sum("" == x))

# We can see that Cabin Embarked and Age have NA or empty String that we maybe 
# have to delete, our biggest problem is Cabin and Age, but Cabin have a lot
# of empty String.
# We will do a fist model without Cabin and we can see what score we obtain.


