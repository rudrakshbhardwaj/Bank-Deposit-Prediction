dataset<-read.csv("bank.csv",sep = ";")
sum(is.na(dataset))
str(dataset)

#DATA PREPARATION
jobnames<-unique(dataset$job)
dataset$job<-factor(dataset$job,
                    levels = jobnames,
                    labels = c(1:12))
dataset$job<-as.numeric(dataset$job)
class(dataset$job)

marital<-unique(dataset$marital)
dataset$marital<-factor(dataset$marital,
                        levels = marital,
                        label=c(1:4))
dataset$marital<-as.numeric(dataset$marital)

education<-unique(dataset$education)
dataset$education<-factor(dataset$education,
                        levels = education,
                        label=c(1:8))
dataset$education<-as.numeric(dataset$education)

default<-unique(dataset$default)
dataset$default<-factor(dataset$default,
                        levels = default,
                        label=c(1:3))
dataset$default<-as.numeric(dataset$default)

housing<-unique(dataset$housing)
dataset$housing<-factor(dataset$housing,
                        levels = housing,
                        label=c(1:3))
dataset$housing<-as.numeric(dataset$housing)

loan<-unique(dataset$loan)
dataset$loan<-factor(dataset$loan,
                        levels = loan,
                        label=c(1:3))
dataset$loan<-as.numeric(dataset$loan)

contact<-unique(dataset$contact)
dataset$contact<-factor(dataset$contact,
                     levels = contact,
                     label=c(1:2))
dataset$contact<-as.numeric(dataset$contact)

day_of_week<-unique(dataset$day_of_week)
dataset$day_of_week<-factor(dataset$day_of_week,
                     levels = day_of_week,
                     label=c(1:5))
dataset$day_of_week<-as.numeric(dataset$day_of_week)

poutcome<-unique(dataset$poutcome)
dataset$poutcome<-factor(dataset$poutcome,
                     levels = poutcome,
                     label=c(1:3))
dataset$poutcome<-as.numeric(dataset$poutcome)

month<-unique(dataset$month)
dataset$month<-factor(dataset$month,
                         levels = month,
                         label=c(1:10))
dataset$month<-as.numeric(dataset$month)

dataset$y<-factor(dataset$y,
                  levels = c("no","yes"),
                  labels = c(0,1)
                  )
dataset$y<-as.numeric(dataset$y)

library(corrplot)
cot<-cor(dataset)
corrplot(cot)

dataset$y<-as.factor(dataset$y)
class(dataset$y)


library(caTools)
set.seed(123)
split = sample.split(dataset$y, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


training_set[,1:20] = scale(training_set[,1:20])
test_set[,1:20] = scale(test_set[,1:20])

nrow(training_set)

library(class)


  knn.176<-knn(train=training_set[,1:21],test=test_set[,1:21],cl=training_set[,21],
               k=176)
  cm = table(test_set[, 21], knn.176)
cm
sum(diag(cm))/nrow(test_set)

knn.175<-knn(train=training_set[,1:21],test=test_set[,1:21],cl=training_set[,21],
             k=175)
cm = table(test_set[, 21], knn.175)
cm
sum(diag(cm))/nrow(test_set)

i=1
k.optm=1
for (i in 1:176){
  knn.mod <- knn(train=training_set[,1:21], test=test_set[,1:21], cl=training_set[,21], k=i)
   k.optm[i] <- 100 * sum(test_set[,21] == knn.mod)/NROW(test_set[,21])
   k=i
 
}

plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")

knn.3<-knn(train=training_set[,1:21],test=test_set[,1:21],cl=training_set[,21],
             k=3)
cm = table(test_set[, 21], knn.3)
cm
sum(diag(cm))/nrow(test_set)
