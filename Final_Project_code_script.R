#some more code in PDF...

#loading data:
install.packages("tidyverse")
library (tidyverse)
#data <- read_csv("C:\\Users\\Israel\\Desktop\\ML & application to biological data analysis\\Final project\\archive\\timesData - UsedWithLabels.csv") 
data <- read_csv("\timesData - UsedWithLabels.csv") 

#filtering:
data = data %>% filter(year == 2016)
data[data==""]<-NA 
data[data=="-"]<-NA
data<-data[complete.cases(data),]

#graphs:
#1.
hist(data$teaching) # plot histograma type
#2.
hist(data$research)
#3.
ggplot(data = data, mapping = aes( x=student_staff_ratio,y = teaching))   +geom_point(mapping = aes(x=student_staff_ratio, y = teaching))+geom_point(mapping  = aes(colour = research))
#4.
ggplot(data = data, mapping = aes(x = income, y=research))+geom_smooth(method="lm")+geom_point(mapping = aes(x = income, y=research))+geom_point(mapping  = aes(colour = teaching))
#5.
ggplot(data = data, mapping = aes( x=research,y = teaching)) + geom_smooth(method="lm")  +geom_point(mapping = aes(x=research, y = teaching))
#6.
ggplot(data = data, mapping = aes(x = income, y=research))+ geom_smooth(method="lm")   +geom_point(mapping = aes(x = income, y=research))+geom_point(mapping  = aes(colour = female_male_ratio))
#7.
ggplot(data = data, mapping = aes(y = research, x=world_rank))+ geom_smooth(method="lm")   +geom_point(mapping = aes(y = research, x=world_rank))+geom_point(mapping  = aes(colour = female_male_ratio))
#8.
best_uni =  filter(data,world_rank >=9) ggplot(data = new_data2) + geom_bar(mapping = aes(x = country))+ theme(text = element_text(size=10),  axis.text.x = element_text(angle=90, hjust=1))


#R algorithms codes: (many script where placed together here)
#2. SVM:

library (tidyverse)
data <- read_csv("C:\\Users\\Israel\\Desktop\\ML & application to biological data analysis\\Final project\\archive\\timesData - UsedWithLabels.csv") 

data = data %>% filter(year == 2016)
data[data==""]<-NA
data[data=="-"]<-NA
data<-data[complete.cases(data),]

#changin country & university name to numeric representation:
data$country <- as.factor(data$country) #convert the string from character to factor
data$university_name <- as.factor(data$university_name) #convert the string from character to factor
#data = data[,-2] #remove country #if factor above doesnt work somewhy
#data = data[,-2] #REMOVE UNI' NAME
  #do twice,rmv female male ration as its a chr col, 2T to remove year as well
data = data[,-12] 
data = data[,-12]
#else do only once:
#data = data[,-13] #REMOVE year

#shuffling the data by rows:
data = data[sample(1:nrow(data)),]

#Doing scaling:
#scaleddf <- as.data.frame(sapply(data, function(i) if(is.numeric(i)) scale(i) else i)) #the algo will do it as well, so meybe not needed
#data.frame(round(scales::rescale(-scaleddf$world_rank, to = c(1, 100)))) #rescale 1 column
#need to check rescaling all column altogether
#data <- data.frame(round(scales::rescale(-scaleddf$count, to = c(1, 100))))
#rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
#data <- rescale(data)
#data
#install.packages("kernlab")
#install.packages("ISLR")
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots

#make any negative num to 0
data[data < 0] <- 0

#splitting to train and test
data_train <- data[118:702, ]
data_test <- data[1:117, ]

#Building the actual model:
data_classifier <- ksvm(world_rank ~ ., data = data_train, kernel = "vanilladot")
#data_classifier <- ksvm(world_rank ~ ., data = data_train, kernel = "rbfdot")

data_classifier

data_predictions <- floor(predict(data_classifier, data_test)+0.5) #added floor
#data_predictions <- ceiling(predict(data_classifier, data_test)-0.5) #added ceiling
#data_predictions <- trunc(predict(data_classifier, data_test)) #added ceiling

#make any negative num to 0
data_predictions[data_predictions < 0] <- 0
head(data_predictions)
table(data_predictions, data_test$world_rank)
agreement <- data_predictions == data_test$world_rank
table(agreement)
#in percentage
prop.table(table(agreement))
#Avg 60% success rate. Lets move on to improving the model performance:
#There are many kernal functions for SVM, but a standard Gaussian Radial basis function (RBF) kernal is a good place to start.
#We'll use the ksvm() function here:

WR_classifier_rbf <- ksvm(world_rank ~ ., data = data_train, kernel = "rbfdot") #NOT STREIGHT LINE
#WR_classifier_rbf <- ksvm(world_rank ~ ., data = data_train, kernel = "vanilladot") #STREIGHT LINE
#Create a prediction for this new model:
#WR_predictions_rbf <- floor(predict(WR_classifier_rbf, data_test)+0.5)
WR_predictions_rbf <- ceiling(predict(WR_classifier_rbf, data_test)-0.5)

#And compare it to the previous model:
agreement_rbf <- WR_predictions_rbf == data_test$world_rank
table(agreement_rbf)
prop.table(table(agreement_rbf))

WR_classifier_rbf <- ksvm(world_rank ~ ., data = data_train, kernel = "radial") 
#Create a prediction for this new model:

#WR_predictions_rbf <- floor(predict(WR_classifier_rbf, data_test)+0.5)
WR_predictions_rbf <- ceiling(predict(WR_classifier_rbf, data_test)-0.5)


#And compare it to the previous model:
agreement_rbf <- WR_predictions_rbf == data_test$world_rank
table(agreement_rbf)
prop.table(table(agreement_rbf))

#T-sne:
#in R:
#install.packages("Rtsne")
library(ggplot2)
library(Rtsne)
tsne_out <- Rtsne(data)
tsne_plot <- data.frame(x = tsne_out$Y[,1], y = tsne_out$Y[,2], col = data$world_rank)
ggplot(tsne_plot) + geom_point(aes(x=x, y=y, color=col))

Random Forest:
### Importing libraries ###
library (tidyverse)
library(ggplot2)
library(tidymodels) 
library(modeldata)  
library(vip)    
library(yardstick)

### Loading data ###
data <- read.csv(file = "./timesData - UsedWithLabels-fixed FMRatio.csv")
###Preprocessing the data###
#Removing NA's
data[data==""]<-NA
data[data=="-"]<-NA
data<-data[complete.cases(data),]
#Filtering for year 2016
data = data%>%filter(year == 2016)
	
data %>% 
  count(world_rank) %>% 
  mutate(prop = n/sum(n))

#Shuffling the data by rows:
data = data[sample(1:nrow(data)),]


###Splitting the data###

#Selecting to remove the columns: university_name, country,female_male_ratio and year:
data_split <- initial_split(data %>% select(-university_name, -country,-female_male_ratio,-year), strata = world_rank)

data_train <- training(data_split)
data_test  <- testing(data_split)

#Training set proportions by world_rank
data_train %>% 
  count(world_rank) %>% 
  mutate(prop = n/sum(n))

#Test set proportions by world_rank
data_test %>% 
  count(world_rank) %>% 
  mutate(prop = n/sum(n))

### Random Forest Modelling ###

rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

#set.seed(234)
rf_fit <- 
  rf_mod %>% 
  fit(factor(world_rank) ~ . , data = data_train)
rf_fit


### Estimating performance ###
rf_training_pred <- 
  predict(rf_fit, data_train) %>% 
  bind_cols(predict(rf_fit, data_train, type = "prob")) %>% 
  # Add the true outcome data back in
  bind_cols(data_train %>% 
              select(world_rank))

rf_training_pred %>%                # training set predictions
  roc_auc(truth = factor(world_rank), c(.pred_0,.pred_1,.pred_2,.pred_3,.pred_4,
                                .pred_5,.pred_6,.pred_7,.pred_8,.pred_9,.pred_10))


rf_training_pred %>%                # training set predictions
  accuracy(truth = factor(world_rank), .pred_class)


## Lets see how the model performs on the test data: 

rf_testing_pred <- 
  predict(rf_fit, data_test) %>% 
  bind_cols(predict(rf_fit, data_test, type = "prob")) %>% 
  bind_cols(data_test %>% select(world_rank))

rf_testing_pred %>%                   # test set predictions
  roc_auc(truth = factor(world_rank), c(.pred_0,.pred_1,.pred_2,.pred_3,.pred_4,
                                        .pred_5,.pred_6,.pred_7,.pred_8,.pred_9,.pred_10))

rf_testing_pred %>%                   # test set predictions
  accuracy(truth = factor(world_rank), .pred_class)

# 5.K-means:
### Importing libraries ###
library(dplyr)
library(stats)
library(ggpubr)
library(factoextra)
### Data Loading ###
data <- read.csv(file = "./timesData - UsedWithLabels-fixed FMRatio.csv")

### Filtering NA's ###
data[data==""]<-NA
data[data=="-"]<-NA
data<-data[complete.cases(data),]

### Filtering to year 2016 ###
data = data%>%filter(year == 2016)

### Creating dataframe from the data with only numeric columns ###
world_ranks_z <- as.data.frame(lapply(data[,c("world_rank","teaching","research","citations","num_students")], scale)) 
### Clustering the data to 5 clusters ###
worldrank_clusters <- kmeans(scale(data[,c("world_rank","teaching","research","citations","num_students")]), 5, nstart = 1)


### Plotting the clusters ###

fviz_cluster(worldrank_clusters, data = world_ranks_z,
             palette = c("yellow","blue","red","purple","orange"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
