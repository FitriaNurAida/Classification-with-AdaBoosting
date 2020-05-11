# Import Data #
movie <- read.csv("E:/My Dictionary/Using R/Data/Movie_classification.csv")
View(movie)
str(movie)
movie$Start_Tech_Oscar <- as.factor(movie$Start_Tech_Oscar)

# Data Preprocessing #
summary(movie) #there are missing values in variable Time_taken
movie$Time_taken[is.na(movie$Time_taken)] <- mean(movie$Time_taken,na.rm = TRUE) #imputasi with mean because it is numerical variable

# Test-Train Split
install.packages('caTools')
library(caTools)
set.seed(0)
split <- sample.split(movie,SplitRatio = 0.8)
train <- subset(movie,split == TRUE)
test <- subset(movie,split == FALSE)

############################### MODELING #################################
install.packages("adabag")
library(adabag)
set.seed(0)

adaboost <- boosting(Start_Tech_Oscar~., data=train, boost=TRUE)
tplot <- adaboost$trees[[1]]; tplot
plot <- plot(tplot)
text(tplot, pretty=100)

pred <- predict(adaboost, test)
View(pred)
cm <- table(pred$class, test$Start_Tech_Oscar)
cm
Accuracy <- (cm[1,1]+cm[2,2])/sum(cm)
Accuracy
