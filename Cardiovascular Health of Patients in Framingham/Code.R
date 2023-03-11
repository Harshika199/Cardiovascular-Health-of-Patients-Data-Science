############# - - - READING DATA - - - #############
frame <- read.csv("framingham.csv")
head(frame)

############# - - - CLEANING - - - #############
#Removing all NAN values
dim(frame)
frame <- na.omit(frame)
dim(frame) #Removes 4240 - 3658 rows

############# - - - Mitigate classification imbalance - - - #############
table(frame$TenYearCHD) #We see that there is a 5.5:1.0 imbalance
prop.table(table(frame$TenYearCHD)) #84% of the data is class 0

# Checking columns where all values are the same
summary(frame)
#Theres none, so we're all good

############# - - - PREPROCESSING - - - #############
frame$TenYearCHD<-factor(frame$TenYearCHD) #We tell R that the dependent variable is a factor/category
frame$male<-factor(frame$male)
frame$currentSmoker<-factor(frame$currentSmoker)
frame$BPMeds<-factor(frame$BPMeds)
frame$prevalentStroke<-factor(frame$prevalentStroke)
frame$prevalentHyp<-factor(frame$prevalentHyp)
frame$diabetes<-factor(frame$diabetes)
#The other variables are continous

############# - - - SPLITTING - - - #############
#70% Train, 30% Test
ind<-sample(1:nrow(frame), 0.3 * nrow(frame))
train_data<-frame[-ind,]
test_data<-frame[ind,]
#### HOW DO WE fix the length equal?
table(train_data$TenYearCHD)
prop.table(table(train_data$TenYearCHD))


#We do both (over AND under) at same time
n_new <- nrow(train_data)
fraction_heartdisease_new <- 0.50
sampling_result <- ovun.sample(TenYearCHD ~ .,
                               data = train_data,
                               method = "both",
                               N = n_new,
                               p = fraction_heartdisease_new,
                               seed = 666)
train_data <- sampling_result$data #### NEW TRAIN DAAT 
table(train_data$TenYearCHD)
prop.table(table(train_data$TenYearCHD))

############# - - - CLASSIFICATION TREE - - - #############

#Computing tree

ctout <- ctree(TenYearCHD ~ .,control = ctree_control(mincriterion =0.999),data=train_data) 
#ctout <- ctree(TenYearCHD ~ .,cluster = 2, data=train_data) 

#Using tree on test-data
ctpred <- predict(ctout, test_data) 


#Evaluating performance on test data (the important one)
mean(test_data$TenYearCHD == ctpred) #Accuracy, "xx% correct". On average the class-tree har xx% correct
table(ctpred, test_data$TenYearCHD)

plot(ctout, gp = gpar(fontsize = 6),     # font size changed to 6
     inner_panel=node_inner,
     ip_args=list(
       abbreviate = FALSE, 
       id = FALSE)
)

############# - - - RANDOM FORREST - - - #############
#A Way of aggreagating a lot of difference classification trees 
# Therefore cannot plot, but can show predictions

cfout <- cforest(TenYearCHD ~ .,ntree=20, data=train_data) # ntree specifies the number of trees used in the forest
cfpred <- predict(cfout, newdata=test_data)
mean(cfpred == test_data$TenYearCHD) # do this command several times to see the variation in predictions
table(cfpred, test_data$TenYearCHD)

#Support Vector Machine#
library(e1071)
library(ggplot2)
install.packages("WeightSVM")
library(WeightSVM)
# Read in training data (70% of entire data set)
frame<-read.csv("framingham.csv")
class(frame$education)
library(corrplot)
frame<-na.omit(frame)
#frame<-as.data.frame(lapply(frame, as.numeric))
#matrix<-as.matrix(frame)

#Look at the correlation matrix for all variables in this data set
cor_matrix<-cor(frame, method="pearson") 
#Draw the correlation graph
corrplot.mixed(corr=cor_matrix, tl.pos="lt", tl.srt=20, 
               addCoef.col = "black")

# turn categorical columns into factors 
frame$male=as.factor(frame$male) 
frame$currentSmoker=as.factor(frame$currentSmoker) 
frame$BPMeds=as.factor(frame$BPMeds) 
frame$prevalentStroke=as.factor(frame$prevalentStroke) 
frame$prevalentHyp=as.factor(frame$prevalentHyp) 
frame$diabetes=as.factor(frame$diabetes) 
frame$TenYearCHD=as.factor(frame$TenYearCHD) 

head(frame)

#plot(frame$sysBP,frame$glucose,pch=16,col=frame$TenYearCHD)
ggplot(frame,aes(x=sysBP,y=glucose,col=TenYearCHD))+geom_point()
ggplot(frame,aes(x=age,y=sysBP,col=TenYearCHD))+geom_point()
#557 of them have heart disease
sum(frame$TenYearCHD == 1)  


#create training and testing data sets
split.at <- round(0.8*length(frame$male)) # store the number in split.at
ind<-sample(1:nrow(frame), split.at) # from the frame, sample "split.at" amount of data
train_data<-frame[ind,]
test_data<-frame[-ind,]

# Fit SVM margin with RBF Kernel with all the variables  the SVM will automatically scale the data
#result<-svm(TenYearCHD~., kernel="radial", gamma=0.5, cost=20, data=train_data)
result<-svm(TenYearCHD~., kernel="radial", data=train_data)
print(result)

# Compute fitted values on training data
pred_train<-predict(result, data=train_data)
# Evaluate model performance on training data
table(pred_train, train_data$TenYearCHD)
accuracy_train = mean(pred_train==train_data$TenYearCHD)

# Compute fitted values on testing data
pred_test<-predict(result, newdata=test_data)
# Evaluate model performance on training data
table(pred_test, test_data$TenYearCHD)
accuracy_test = mean(pred_test==test_data$TenYearCHD)

#overfitting problems occurs, so decrease some variables
# Fit SVM margin with RBF Kernel with all the variables  the SVM will automatically scale the data
#cancel currentSmoker(keep cigsperDay), diaBP & prevalentHyp (keep sysBP), diabetes(keep glucose), 
result2<-svm(TenYearCHD~male+age+education+cigsPerDay+BPMeds 
             +prevalentStroke+totChol+sysBP+BMI+heartRate+glucose, kernel="linear", data=train_data)
print(result2)

# Compute fitted values on training data
pred_train<-predict(result2, data=train_data)
# Evaluate model performance on training data
table(pred_train, train_data$TenYearCHD)
accuracy_train = mean(pred_train==train_data$TenYearCHD)

# Compute fitted values on testing data
pred_test<-predict(result2, newdata=test_data)
# Evaluate model performance on training data
table(pred_test, test_data$TenYearCHD)
accuracy_test = mean(pred_test==test_data$TenYearCHD)

#try weighted svm
frame_withw <- frame
frame_withw$weights <- with(
  frame_withw, ifelse(TenYearCHD==0, 0.17, 1))

result<-wsvm(TenYearCHD~., kernel="radial", data=train_data,weight=frame_withw[ind,]$weights)
print(result)
# Compute fitted values on training data
pred_train<-predict(result, data=train_data)
# Evaluate model performane on training data
table(pred_train, train_data$TenYearCHD)
accuracy_train = mean(pred_train==train_data$TenYearCHD)

# Compute fitted values on testing data
pred_test<-predict(result, newdata=test_data)
# Evaluate model performance on training data
table(pred_test, test_data$TenYearCHD)
accuracy_test = mean(pred_test==test_data$TenYearCHD)

#Neural Network#

install.packages("readxl")
install.packages("neuralnet")
install.packages("dplyr")
install.packages("ROSE")
library (ROSE)
library(neuralnet)
library(readxl)
library(dplyr)
df <- read.csv("framingham.csv")
dim(df)
# remove empty rows
df<- na.omit(df)
dim(df)

#data$char[data$char == "b"] <- "XXX" 
v1 = c(0.0, 0.0, 0.0, 1.0)
v2 = c(0.0, 0.0, 1.0, 0.0)
v3 = c(0.0, 1.0, 0.0, 0.0)
v4 = c(-1.0, -1.0, -1.0, -1.0)

df$male[df$male == "1"] <- -1
df$male[df$male == "0"] <- 1

df$currentSmoker[df$currentSmoker == "1"] <- -1
df$currentSmoker[df$currentSmoker == "0"] <- 1

df$BPMeds[df$BPMeds == "1"] <- -1
df$BPMeds[df$BPMeds == "0"] <- 1

df$prevalentStroke[df$prevalentStroke == "1"] <- -1
df$prevalentStroke[df$prevalentStroke == "0"] <- 1

df$prevalentHyp[df$prevalentHyp == "1"] <- -1
df$prevalentHyp[df$prevalentHyp == "0"] <- 1

df$diabetes[df$diabetes == "1"] <- -1
df$diabetes[df$diabetes == "0"] <- 1

df$education <- ifelse((df$education==1), v1, df$education)
df$education <- ifelse((df$education==2), v2, df$education)
df$education <- ifelse((df$education==3), v3, df$education)
df$education <- ifelse((df$education==4), v4, df$education)


df$age <- ((df$age - mean(df$age))/ sd(df$age))
df$cigsPerDay <-((df$cigsPerDay - mean(df$cigsPerDay))/ sd(df$cigsPerDay))
df$totChol <- ((df$totChol - mean(df$totChol))/ sd(df$totChol))
df$sysBP <- ((df$sysBP - mean(df$sysBP))/ sd(df$sysBP))
df$diaBP <- ((df$diaBP - mean(df$diaBP))/ sd(df$diaBP))
df$BMI <- ((df$BMI - mean(df$BMI))/ sd(df$BMI))
df$heartRate<- ((df$heartRate - mean(df$heartRate))/ sd(df$heartRate))
df$glucose <- ((df$glucose - mean(df$glucose))/ sd(df$glucose))

ind<-sample(1:nrow(df), 900)
train_data<-df[-ind,]
test_data<-df[ind,]

n_new <- nrow(train_data)
fraction_heartdisease_new <- 0.50
sampling_result <- ovun.sample(TenYearCHD ~ .,
                               data = train_data,
                               method = "both”,
                               N = n_new,
                               p = fraction_heartdisease_new,
                               seed = 2018)

train_data <- sampling_result$data 
table(train_data$TenYearCHD)
prop.table(table(train_data$TenYearCHD))

Net.Est<-neuralnet(TenYearCHD~male+age+education+currentSmoker+cigsPerDay+BPMeds+prevalentStroke+prevalentHyp+diabetes+totChol+sysBP+diaBP+BMI+heartRate+glucose, hidden=c(2,2), data=train_data,  threshold = 0.01, rep = 1, startweights = NULL,learningrate.limit = NULL, learningrate.factor = list(minus = 0.5,plus = 1.2), learningrate = NULL, lifesign = "none", lifesign.step = 1000, algorithm = "rprop+", err.fct = "sse",act.fct = "logistic", linear.output = FALSE, exclude = NULL, constant.weights = NULL, likelihood = FALSE)


plot(Net.Est)
Net.Est$result.matrix

predictions<-predict(Net.Est, train_data)
unscalepred<- round(predictions*(max(train_data$TenYearCHD)-min(train_data$TenYearCHD))+min(train_data$TenYearCHD),0)

mean(abs(unscalepred-train_data$TenYearCHD))

cor(train_data$TenYearCHD, unscalepred)
table(round(train_data$TenYearCHD,0),unscalepred)

predictions<-predict(Net.Est, test_data)
unscalepred<- round(predictions*(max(test_data$TenYearCHD)-min(test_data$TenYearCHD))+min(test_data$TenYearCHD),0)

mean(abs(unscalepred-test_data$TenYearCHD))

cor(test_data$TenYearCHD, unscalepred)
table(round(test_data$TenYearCHD,0),unscalepred)

