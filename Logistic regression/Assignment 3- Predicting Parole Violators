#read dataset
parole <- read.csv("parole.csv")

#check dataset
summary(parole)
#check how many parole violated term
table(parole$violator)
str(parole)

#convert state and crime to factor
parole$state <- as.factor(parole$state)
parole$crime <- as.factor(parole$crime)
summary(parole)
table(parole$state)

#splitting data into a training and test set
set.seed(144)
library(caTools)
split = sample.split(parole$violator, SplitRatio = 0.7)
train = subset(parole, split == TRUE)
test = subset(parole, split == FALSE)
str(split)

#building a model
model1 <- glm(violator ~ ., family = "binomial", data = train)
summary(model1)

#parolee who is male, of white race, 
#aged 50 years at prison release, from the state of Maryland, 
#served 3 months, had a maximum sentence of 12 months,
#did not commit multiple offenses, and committed a larceny

new_data = c(male=1, race=1, age=50, state=1, time.served=3, max.sentence=12, multiple.offenses=0, crime=2, violator=0)
x = rbind(test, new_data)
new_data = x[nrow(x),]
predict(model1, newdata=new_data)

#odds individual is violator
exp(-1.70063)

#probability individual is violator
0.1826/(1+0.1826)

#predict
testpredict <- predict(model1, newdata = test, type = "response")
summary(testpredict)
table(test$violator, testpredict>=0.5)
#sensitivity
12/(12+11)
#specifity
167/(167+12)
#accuracy
(167+12)/202
#simple model that predicts that every parolee is a non-violator
table(test$violator)  
179/202
#false negative error rate 
11/(11+12)
#false positive error rate
12/(167+12)
#calculated auc
install.packages("ROCR")
library(ROCR)
ROCRpredTest <-  prediction(testpredict, test$violator)
auc <-  as.numeric(performance(ROCRpredTest, "auc")@y.values)
