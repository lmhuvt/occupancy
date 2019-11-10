# Section 1: Introduction

# Original data is retrieved from the UCI Machine Learning Repository at 
# https://archive.ics.uci.edu/ml/machine-learning-databases/00357/occupancy_data.zip 
# There are 3 datasets in the repository. One is training set and other 
# two are test sets. My goal is to detect occupancy status of room based on attributes
# such as date, temperature, humidity, CO2, light, and humidityratio. Occupancy variable 
# has 0 and 1 values that represents not occupied and occupied respectively. In this 
# analysis, I will use training data set for building model and test on the first test
# set, I will use the second test set(test2) as my validaiton data set in the results.
# Top two models will be recommanded at the end of the report. All data sets along with
# a R script, a Rmd file and a pdf report are in 
# githut:  https://github.com/lmhuvt/occupancy.

# Section 2: methods/analysis

# load library, if not installed, install them.
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", 
                                       repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", 
                                            repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", 
                                            repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", 
                                     repos = "http://cran.us.r-project.org")
library(lubridate)
library(ggplot2)
library(tidyverse)
library(caret)
library(randomForest)
library(rpart)
library(data.table)
library(purrr)
# load data through relative path 
# all project related files are in github: https://github.com/lmhuvt/occupancy
data_training <- read.table("./datatraining.txt",header=TRUE,sep=",")
data_testing <- read.table("./datatest.txt",header=TRUE,sep=",")
data_testing2 <- read.table("./datatest2.txt",header=TRUE,sep=",")

str(data_training) # review training data set
# 8143 obs and 7 varibles in training data set
str(data_testing) # review test data set
# 2665 obs and 7 varibles in test data set
str(data_testing2) # review test2 data set
# 9752 obs and 7 varibles in test2 data set

# Summary of data review:
# Over all, from data review, all data set are data frames, 
# there are 6 predictors and 1 outcome
# 6 predictors: Date is factor, Temperature, Humidity, Light, 
# CO2, HumidityRatio are numbers
# 1 outcome "Occupancy" is a binary value, 1 was occupied and 0 was not occupied

# Take a look Occupancy distribution in the data sets
prop.table(table(data_training$Occupancy))
prop.table(table(data_testing$Occupancy))
prop.table(table(data_testing2$Occupancy))
# table shows probability of occupancy "0"(unoccupied) is higher than 
# occupancy "1"(occupied)

# Check missing(NA) data 
sum(is.na(data_training)) # no missing data
sum(is.na(data_testing)) # no missing data
sum(is.na(data_testing2)) # no missing data

# take a look of distinct predictors
n_predictor <- data.frame(n_Humidity = n_distinct(data_training$Humidity),
                          n_HumidityRatio = n_distinct(data_training$HumidityRatio),
                          n_Temperature = n_distinct(data_training$Temperature),
                          n_Light = n_distinct(data_training$Light),
                          n_CO2 = n_distinct(data_training$CO2))
n_predictor

# Visulization predictors effect

# Humidity effect
t1 <- data_training %>% 
  group_by(Humidity)%>% 
  mutate(prob=mean(Occupancy == "1"))%>%
  select(Humidity,prob)
t1 %>% ggplot(aes(x=Humidity, y=prob))+geom_line()+geom_smooth()+
  labs(x="Humidity", y="Occupancy Probability", 
       caption = "source data: Occupancy_data UCI")+
  ggtitle("Humidity vs. Occupancy Probability")
# It is hard to interpret Humidity plot because there are too many points, 
# added line and soomth fuction to see correlation between Humidity and 
# occupancy probability.

# calculate correlation
correlation_Humidity1 <- data_training %>% select(Humidity, Occupancy)%>% 
  summarize(c_Humidity1= cor(Humidity, Occupancy, method = "spearman"))%>%
  pull(c_Humidity1)
correlation_Humidity1
correlation_Humidity2 <-data_training %>% select(Humidity, Occupancy)%>% 
  summarize(c_Humidity2= cor(Humidity, Occupancy, method = "pearson"))%>%
  pull(c_Humidity2)
correlation_Humidity2
correlation_results <- tibble(predictor = "Humidity",
                              spearman = correlation_Humidity1,
                              pearson = correlation_Humidity2)
# Calculation confirmed correlation between Humidity and Occupancy.

# Temperature effect
t2 <- data_training %>% 
  group_by(Temperature)%>% 
  mutate(prob=mean(Occupancy == "1"))%>%
  select(Temperature,prob)
t2 %>% ggplot(aes(x=Temperature, y=prob))+geom_line()+geom_smooth()+
  labs(x="Temperature", y="Occupancy Probability", 
       caption = "source data: Occupancy_data UCI")+
  ggtitle("Temperature vs. Occupancy Probability")
#  plot showed correlation between Temperature and occupancy probability.

# calculate correlation
correlation_Temperature1 <- data_training %>% select(Temperature, Occupancy)%>% 
  summarize(c_Temperature1= cor(Temperature, Occupancy, method = "spearman"))%>%
  pull(c_Temperature1)
correlation_Temperature1
Correlation_Temperature2 <- data_training %>% select(Temperature, Occupancy)%>% 
  summarize(c_Temperature2= cor(Temperature, Occupancy, method = "pearson"))%>%
  pull(c_Temperature2)
Correlation_Temperature2
correlation_results <- bind_rows(correlation_results,
                                 tibble(predictor ="Temperature",
                                        spearman = correlation_Temperature1,
                                        pearson = Correlation_Temperature2))
# Calculation confirmed correlation between Temperature and Occupancy.

# Light effect
t3 <- data_training %>% 
  group_by(Light)%>% 
  mutate(prob=mean(Occupancy == "1"))%>%
  select(Light,prob)
t3 %>% ggplot(aes(x=Light, y=prob))+geom_line()+geom_smooth()+
  labs(x="Light", y="Occupancy Probability", 
       caption = "source data: Occupancy_data UCI")+
  ggtitle("Light vs. Occupancy Probability")
#It is hard to interpret Light plot because there are too many points, 
# added line and soomth fuction to see correlation between Light and 
# occupancy probability.

# calculate correlation
correlation_Light1 <- data_training %>% select(Light, Occupancy)%>% 
  summarize(c_Light1= cor(Light, Occupancy, method = "spearman"))%>%
  pull(c_Light1)
correlation_Light1
correlation_Light2 <- data_training %>% select(Light, Occupancy)%>% 
  summarize(c_Light2= cor(Light, Occupancy, method = "pearson"))%>%
  pull(c_Light2)
correlation_Light2
correlation_results <- bind_rows(correlation_results,
                                 tibble(predictor ="Light",
                                        spearman = correlation_Light1,
                                        pearson = correlation_Light2))
# Calculation confirmed strong correlation between Light and Occupancy.

# CO2 effect
t4 <- data_training %>% 
  group_by(CO2)%>% 
  mutate(prob=mean(Occupancy == "1"))%>%
  select(CO2,prob)
t4 %>% ggplot(aes(x=CO2, y=prob))+geom_line()+geom_smooth()+
  labs(x="CO2", y="Occupancy Probability", 
       caption = "source data: Occupancy_data UCI")+
  ggtitle("CO2 vs. Occupancy Probability")
# It is hard to interpret CO2 plot because there are too many points,
# added line and soomth fuction to see correlation between CO2 and 
# occupancy probability.

# calculate correlation
correlation_CO2_1 <- data_training %>% select(CO2, Occupancy)%>% 
  summarize(c_CO2_1= cor(CO2, Occupancy, method = "spearman"))%>%
  pull(c_CO2_1)
correlation_CO2_1
correlation_CO2_2 <- data_training %>% select(CO2, Occupancy)%>% 
  summarize(c_CO2_2= cor(CO2, Occupancy, method = "pearson"))%>%
  pull(c_CO2_2)
correlation_CO2_2
correlation_results <- bind_rows(correlation_results,
                                 tibble(predictor ="CO2",
                                        spearman = correlation_CO2_1,
                                        pearson = correlation_CO2_2))
# calculation confirmed correlation between CO2 and Occupancy.

# HumidityRatio effect
t5 <- data_training %>% 
  group_by(HumidityRatio)%>% 
  mutate(prob=mean(Occupancy == "1"))%>%
  select(HumidityRatio,prob)
t5 %>% ggplot(aes(x=HumidityRatio, y=prob))+geom_line()+geom_smooth()+
  labs(x="HumidityRatio", y="Occupancy Probability", 
       caption = "source data: Occupancy_data UCI")+
  ggtitle("HumidityRatio vs. Occupancy Probability")
# It is hard to interpret HumidityRatio plot because there are too many points,
# added line and soomth fuction to see correlation between Humidityratio and 
# occupancy probability.

# calcualte correlation
correlation_HR1 <- data_training %>% select(HumidityRatio, Occupancy)%>% 
  summarize(c_HR1= cor(HumidityRatio, Occupancy, method = "spearman"))%>%
  pull(c_HR1)
correlation_HR1
correlation_HR2 <- data_training %>% select(HumidityRatio, Occupancy)%>% 
  summarize(c_HR2= cor(HumidityRatio, Occupancy, method = "pearson"))%>%
  pull(c_HR2)
correlation_HR2
correlation_results <- bind_rows(correlation_results,
                                 tibble(predictor ="HumidityRatio",
                                        spearman = correlation_HR1,
                                        pearson = correlation_HR2))
# calculation confirmed correlation between HumidityRatio and Occupancy.

# Summary: 
options(pillar.sigfig = 4) # keep 4 significant figures in table
correlation_results
# 5 plots shows correlation between numeric predictors and outcome(Occupancy)
# from strongest to weakest correlation:
# Light > CO2 > Temperature > HumidityRatio > Humidity
rm(correlation_results) # remove table

# Date effect
# Date is not a numeric factor, it needs further data cleaning and processng. 
# In order to look into date effect, I will convert outcome occupancy to 
# factor for data process.
data_training$Occupancy <- as.factor(data_training$Occupancy)
data_testing$Occupancy <- as.factor(data_testing$Occupancy)
data_testing2$Occupancy <- as.factor(data_testing2$Occupancy)
# make copies of all data set without changing the original data sets
data_training_m <- copy(data_training)
data_testing_m <- copy(data_testing)
data_testing2_m <- copy(data_testing2)
# take a look of date/time effect, timestamp need to covert to POSIXct format
data_training_m$date <- as.POSIXct(data_training_m$date,tz="UTC") 
data_testing_m$date <- as.POSIXct(data_testing_m$date,tz="UTC") 
data_testing2_m$date <- as.POSIXct(data_testing2_m$date,tz="UTC") 

# I need to convert date into a format which can be esay to process
# x is POSIXct format timestamp
weekend_weekday <- function(x) {
  val <- weekdays(x)
  if (val == "Saturday" | val == "Sunday") {
    val2 = "Weekend"
  }
  else {
    val2= "Weekday"
  }
  return(val2)
}
# for ploting purpose, 0 repersents weekend, 1 repersents weekday
# following function is to convert character weekday/weekend into numeric
Relevel_weekend <- function(y) {
  if (y == "Weekend") {
    val2 = 0
  }
  else {
    val2= 1
  }
  return(val2)
}
# add weekday/weekend column into copy data set
data_training_m$WeekStatus <-unlist(lapply(data_training_m$date,
                                           weekend_weekday))
data_testing_m$WeekStatus <-unlist(lapply(data_testing_m$date,
                                          weekend_weekday))
data_testing2_m$WeekStatus <-unlist(lapply(data_testing2_m$date,
                                           weekend_weekday))
# add WeekStatus2 column into copy data set, use "1" repersent weekday
# use "0" repersent weekend
data_training_m$WeekStatus2 <- unlist(lapply(data_training_m$WeekStatus,
                                             Relevel_weekend))
data_testing_m$WeekStatus2 <- unlist(lapply(data_testing_m$WeekStatus,
                                             Relevel_weekend))
data_testing2_m$WeekStatus2 <- unlist(lapply(data_testing2_m$WeekStatus,
                                             Relevel_weekend))
# take a look of new data sets
str(data_training_m)
str(data_testing_m)
str(data_testing2_m)

# Visulization of date effect after date convertion
plot_date <- data_training_m %>% 
  ggplot(aes(x= WeekStatus, y = as.numeric(Occupancy)))+
  geom_count(aes(alpha=0.25,color= ..n.., size = ..n..))+
  labs(x="WeekStatus", y="Occupancy", caption = "source data: Occupancy_data UCI")+
  ggtitle("WeekStatus vs. Occupancy")
plot_date
# plot_date shows correlation between date and Occupancy.
# Caculate occupancy probability and plot correlation, the probability of 
# occupancy might be easier to see the correlation.
data_date <- data_training_m %>% 
  group_by(WeekStatus2)%>% 
  mutate(prob=mean(Occupancy == "1"))%>%
  select(WeekStatus2,prob)

# plot correlation between data and occupancy probability
data_date %>% ggplot(aes(x=WeekStatus2, y=prob))+
  geom_count(aes(alpha=0.8,color= ..n.., size = ..n..))+
  geom_count(aes(alpha=0.8,color= ..n.., size = ..n..))+
  labs(x="WeekStatus2", y="Occupancy Prob", 
       caption = "source data: Occupancy_data UCI")+
  ggtitle("WeekStatus vs. Occupancy Probability")
# date and probability plots both showed correlation between date and 
# occupancy, and the probability plot is better to show the correlation.

# build model based on training data set and test model in test data set
# test2 data will be used as validation data set in results section. 
# date is factor, and the rest of predictors are numeric.
# remove original date column out of data sets to simplify model training
data_training_1 <- subset(data_training_m, 
                          select = c("Temperature", "Humidity", "Light",
                                     "CO2", "HumidityRatio",
                                     "Occupancy"))
data_testing_1 <- subset(data_testing_m, 
                         select = c("Temperature", "Humidity", "Light",
                                    "CO2", "HumidityRatio",
                                    "Occupancy"))
data_testing2_1 <- subset(data_testing2_m, 
                          select = c("Temperature", "Humidity", "Light",
                                     "CO2", "HumidityRatio",
                                     "Occupancy"))
# check all data sets
str(data_training_1)
str(data_testing_1)
str(data_testing2_1)
# new data sets show 6 varibles including 1 outcome and 5 predictors
# I will use traing data to build models, and test model in test data set, 
# test2 data set will be used as validation data set in the results section.

# qda, knn, rpart, rf model will be used on traing and test data set.

# qda model is a logistic regression model, I am going to try it first.
# Temperature
set.seed(1, sample.kind = "Rounding")
train_qda_Temperature <- train(Occupancy~ Temperature, 
                               method ="qda", data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_qda_Temperature_train <-confusionMatrix(predict(train_qda_Temperature,
                                                            data_training_1),
                                                    data_training_1$Occupancy
                                                    )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_qda_Temperature_test <-confusionMatrix(predict(train_qda_Temperature,
                                                        data_testing_1),
                                                data_testing_1$Occupancy
                                                )$overall["Accuracy"]
# make sure accuracy reulsts have 7 significant figures
options(pillar.sigfig = 7)

# accuracy_results table contains accuracy of different models with predictors
accuracy_results <- tibble(method = "qda", 
                           predictor = "Temperature", 
                           Accuracy_Train = accuracy_qda_Temperature_train,
                           Accuracy_Test = accuracy_qda_Temperature_test)
# Humidity
set.seed(1, sample.kind = "Rounding")
train_qda_Humidity <- train(Occupancy~Humidity, 
                            method ="qda", data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_qda_Humidity_train <-confusionMatrix(predict(train_qda_Humidity,
                                                         data_training_1),
                                               data_training_1$Occupancy
                                              )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_qda_Humidity_test <-confusionMatrix(predict(train_qda_Humidity,
                                                        data_testing_1),
                                              data_testing_1$Occupancy
                                             )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method = "qda", 
                                     predictor = "Humidity", 
                                     Accuracy_Train = accuracy_qda_Humidity_train,
                                     Accuracy_Test = accuracy_qda_Humidity_test))

# Light
set.seed(1, sample.kind = "Rounding")
train_qda_Light <- train(Occupancy~Light, 
                         method ="qda", data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_qda_Light_train <-confusionMatrix(predict(train_qda_Light,
                                                   data_training_1),
                                           data_training_1$Occupancy
                                           )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_qda_Light_test <-confusionMatrix(predict(train_qda_Light,
                                                  data_testing_1),
                                          data_testing_1$Occupancy
                                          )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method = "qda", 
                                     predictor = "Light", 
                                     Accuracy_Train = accuracy_qda_Light_train,
                                     Accuracy_Test = accuracy_qda_Light_test))

# CO2
set.seed(1, sample.kind = "Rounding")
train_qda_CO2 <- train(Occupancy~CO2, 
                       method ="qda", data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_qda_CO2_train <- confusionMatrix(predict(train_qda_CO2,
                                                  data_training_1),
                                          data_training_1$Occupancy
                                          )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_qda_CO2_test <- confusionMatrix(predict(train_qda_CO2,
                                                 data_testing_1),
                                         data_testing_1$Occupancy 
                                         )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results,
                              tibble(method = "qda", 
                                     predictor = "CO2", 
                                     Accuracy_Train = accuracy_qda_CO2_train,
                                     Accuracy_Test =accuracy_qda_CO2_test))

# HumidityRatio
set.seed(1, sample.kind = "Rounding")
train_qda_HumidityRatio <- train(Occupancy~HumidityRatio, 
                                 method ="qda", data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_qda_HumidityRatio_train <-confusionMatrix(predict(train_qda_HumidityRatio,
                                                           data_training_1),
                                                   data_training_1$Occupancy
                                                   )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_qda_HumidityRatio_test <-confusionMatrix(predict(train_qda_HumidityRatio,
                                                          data_testing_1),
                                                  data_testing_1$Occupancy
                                                  )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method = "qda", 
                                     predictor = "HumidityRatio", 
                                     Accuracy_Train = accuracy_qda_HumidityRatio_train,
                                     Accuracy_Test = accuracy_qda_HumidityRatio_test))
# all
set.seed(1, sample.kind = "Rounding")
train_qda <- train(Occupancy~., 
                   method ="qda", data = data_training_1)
varImp(train_qda)
set.seed(1, sample.kind = "Rounding")
accuracy_qda_train <- confusionMatrix(predict(train_qda,data_training_1),
                                      data_training_1$Occupancy
                                      )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_qda_test <-confusionMatrix(predict(train_qda,data_testing_1),
                                    data_testing_1$Occupancy
                                    )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method = "qda", 
                                     predictor = "All", 
                                     Accuracy_Train = accuracy_qda_train,
                                     Accuracy_Test = accuracy_qda_test))
accuracy_results
# The accuracy_results table shows qda model used all 5 predictors has hihgest
# accuracy in training set and first test set. The qda model with 5 predictors 
# will be used on validation data set in results section.

# remove table before next model
rm(accuracy_results)

# CART model
# Classification and Regression Trees (CART)

# Temperature 
set.seed(1, sample.kind = "Rounding")
train_rpart_Temperature <- train(Occupancy~Temperature, method = "rpart", 
                                 data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_Temperature_train <- confusionMatrix(predict(train_rpart_Temperature,
                                                            data_training_1),
                                                    data_training_1$Occupancy
                                                    )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_Temperature_test <- confusionMatrix(predict(train_rpart_Temperature,
                                                           data_testing_1),
                                                   data_testing_1$Occupancy
                                                   )$overall["Accuracy"]
accuracy_results <- tibble(method = "rpart", 
                           predictor = "Temperature", 
                           Accuracy_Train = accuracy_rpart_Temperature_train,
                           Accuracy_Test =accuracy_rpart_Temperature_test)

# Humidity
set.seed(1, sample.kind = "Rounding")
train_rpart_Humidity <- train(Occupancy~Humidity, method = "rpart", 
                                 data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_Humidity_train <- confusionMatrix(predict(train_rpart_Humidity,
                                                         data_training_1),
                                                 data_training_1$Occupancy
                                                 )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_Humidity_test <- confusionMatrix(predict(train_rpart_Humidity,
                                                        data_testing_1),
                                                data_testing_1$Occupancy
                                                )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results, tibble(method = "rpart", 
                           predictor = "Humidity", 
                           Accuracy_Train = accuracy_rpart_Humidity_train,
                           Accuracy_Test =accuracy_rpart_Humidity_test))

# Light
set.seed(1, sample.kind = "Rounding")
train_rpart_Light <- train(Occupancy~Light, method = "rpart", 
                              data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_Light_train <- confusionMatrix(predict(train_rpart_Light,
                                                      data_training_1),
                                              data_training_1$Occupancy
                                              )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_Light_test <- confusionMatrix(predict(train_rpart_Light,
                                                     data_testing_1),
                                             data_testing_1$Occupancy
                                             )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method = "rpart", 
                                     predictor = "Light", 
                                     Accuracy_Train = accuracy_rpart_Light_train,
                                     Accuracy_Test =accuracy_rpart_Light_test))

# CO2
set.seed(1, sample.kind = "Rounding")
train_rpart_CO2 <- train(Occupancy~CO2, method = "rpart", 
                           data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_CO2_train <- confusionMatrix(predict(train_rpart_CO2,
                                                    data_training_1),
                                            data_training_1$Occupancy
                                            )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_CO2_test <- confusionMatrix(predict(train_rpart_CO2,
                                                   data_testing_1),
                                           data_testing_1$Occupancy
                                           )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method = "rpart", 
                                     predictor = "CO2", 
                                     Accuracy_Train = accuracy_rpart_CO2_train,
                                     Accuracy_Test =accuracy_rpart_CO2_test))

#HumidityRatio
set.seed(1, sample.kind = "Rounding")
train_rpart_HumidityRatio <- train(Occupancy~HumidityRatio, method = "rpart", 
                         data = data_training_1)
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_HumidityRatio_train <- confusionMatrix(predict(train_rpart_HumidityRatio,
                                                              data_training_1),
                                                      data_training_1$Occupancy
                                                      )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_HumidityRatio_test <- confusionMatrix(predict(train_rpart_HumidityRatio,
                                                             data_testing_1),
                                                     data_testing_1$Occupancy
                                                     )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method = "rpart", 
                              predictor = "HumidityRatio", 
                              Accuracy_Train = accuracy_rpart_HumidityRatio_train,
                              Accuracy_Test =accuracy_rpart_HumidityRatio_test))

# all
set.seed(1, sample.kind = "Rounding")
train_rpart <- train(Occupancy~., 
                     method ="rpart", data = data_training_1)
varImp(train_rpart)
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_train <- confusionMatrix(predict(train_rpart, 
                                                data_training_1),
                                        data_training_1$Occupancy
                                        )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_test <- confusionMatrix(predict(train_rpart, 
                                               data_testing_1),
                                       data_testing_1$Occupancy
                                       )$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results,
                              tibble(method = "rpart",
                                     predictor = "All",
                                     Accuracy_Train = accuracy_rpart_train,
                                     Accuracy_Test =accuracy_rpart_test))
accuracy_results
# accuracy_results table shows all 5 predictors rpart model has accuracy 99%
# in training but accuracy in test was 96%. Light predictor only rpart model
# has accuracy 99% in training  and 98% in test data set. Using all 5 predictors might be 
# overtaining the rpart model, I will try rpart model with only Light predictor
# at the validation data set in results section.

rm(accuracy_results)# remove accuracy table before next mode

# knn
# select best k with all predictors
# all predictors are numeric, knn might the best because I am dealing 
# with the distance
set.seed(1, sample.kind = "Rounding")
train_knn <- train(Occupancy ~ ., method = "knn", 
                   data = data_training_1,
                   tuneGrid = data.frame(k = seq(3, 51, 2)))

train_knn # take a look of different k and accuracy
varImp(train_knn) # importance of different predictors
# plot to see best k
ggplot(train_knn, highlight = TRUE)+
  labs(x="k in knn", y="Accuracy", 
       caption = "source data: Occupancy_data UCI")+
  ggtitle("k in knn vs. Accuracy")
# select the best k
train_knn$bestTune
set.seed(1, sample.kind = "Rounding")
accuracy_knn_train <- confusionMatrix(predict(train_knn, 
                                              data_training_1, type = "raw"),
                data_training_1$Occupancy)$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_knn_test <- confusionMatrix(predict(train_knn, 
                                             data_testing_1, type = "raw"),
                data_testing_1$Occupancy)$overall["Accuracy"]
accuracy_results <- tibble(method = "knn",
                           predictor = "All",
                           Accuracy_Train = accuracy_knn_train,
                           Accuracy_Test =accuracy_knn_test)
accuracy_results
# try 10-flod cross validation to see any further accuracy improvement
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn_cv <- train(Occupancy ~ ., method = "knn", 
                      data = data_training_1,
                      tuneGrid = data.frame(k = seq(3, 51, 2)),
                      trControl = control)
train_knn_cv # take a look of different k and accuracy
varImp(train_knn_cv) # importance of different predictors
ggplot(train_knn_cv, highlight = TRUE)+
  labs(x="k in knn_cv(10-fold)", y="Accuracy", 
       caption = "source data: Occupancy_data UCI")+
  ggtitle("k in knn_cv vs. Accuracy")
# plot showes very similar accuracy from 0.9883342 to 0.9898081.
train_knn_cv$bestTune
set.seed(1, sample.kind = "Rounding")
accuracy_knn_cv_train <- confusionMatrix(predict(train_knn_cv, 
                                                 data_training_1, type = "raw"),
                                        data_training_1$Occupancy)$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_knn_cv_test <- confusionMatrix(predict(train_knn_cv, 
                                                data_testing_1, type = "raw"),
                                     data_testing_1$Occupancy)$overall["Accuracy"]
accuracy_results <- bind_rows(accuracy_results, 
                              tibble(method = "knn_cv",
                              predictor = "All",
                              Accuracy_Train = accuracy_knn_cv_train,
                              Accuracy_Test =accuracy_knn_cv_test))
accuracy_results
# Summary of knn model, added 10-fold validation has lower accuracy in test data set.
# And the 10-fold knn_cv model has k=3 which might be overtraining the model. 
# I will keep knn without 10-fold cross validaiton at validation data set in results
# section. I also learned Knn model use more computer time than qda and rpart model.

rm(accuracy_results) # remove accuracy table before next model training and testing

# Radom forest
# for classification and regression. It can also be used in unsupervised mode for 
# assessing proximities among data points.I will use all predictors at this model
set.seed(1, sample.kind = "Rounding")
train_rf <- train(Occupancy~., method = "rf", data = data_training_1)
train_rf
# extracts variable importance 
varImp(train_rf)
set.seed(1, sample.kind = "Rounding")
accuracy_rf_train <- confusionMatrix(predict(train_rf, data_training_1),
                                     data_training_1$Occupancy
                                     )$overall["Accuracy"]
set.seed(1, sample.kind = "Rounding")
accuracy_rf_test <- confusionMatrix(predict(train_rf, data_testing_1),
                                    data_testing_1$Occupancy
                                    )$overall["Accuracy"]
accuracy_results <- tibble(method = "rf",
                           predictor = "All",
                           Accuracy_Train = accuracy_rf_train,
                           Accuracy_Test =accuracy_rf_test)
accuracy_results
#  Summary rf model: rf model has 100% accuracy in trainind data set, but 97% 
# in testing data set. I will keep it for the validation and check accuracy in 
# the results section for now.
rm(accuracy_results) # remove all accuray talbe before results section

# Summary of methods/analysis section:

accuracy_results <- tibble(method = c("qda","rpart","knn", "rf"),
                           predictors = c("All","Light", "All","All"),
                           Accuracy_Train = c(accuracy_qda_train,
                                              accuracy_rpart_Light_train,
                                              accuracy_knn_train,
                                              accuracy_rf_train),
                           Accuracy_Test = c(accuracy_qda_test,
                                             accuracy_rpart_Light_test,
                                             accuracy_knn_test,
                                             accuracy_rf_test))
accuracy_results 
# Most single predictor model has lower accuracy compare to using all 4 predictors model.
# Except light predictor in rpart model. I will use all these 4 model in results section.

# Summary of modeling: Most single predictor model has lower accuracy compare to using 
# all 5 predictors in qda model. Light predictor in rpart model has higher accuracy than 
# other predictors. knn and rf model showed high accuracy in training data set. 
# Overall, these 4 models showed high accuracy, I will use 4 these model in results 
# section for validation.

# Section 3: Results
# From the data analysis, I learned that qda, rpart, knn, and random forest with all predictors
# gave me high accuracy model in the test set. I am going to apply them on the validaiton set,
# AKA test2 data set and pick two final models

# qda with 5 predictors
set.seed(1, sample.kind = "Rounding")
accuracy_qda_test2 <- confusionMatrix(predict(train_qda, data_testing2_1),
                                      data_testing2_1$Occupancy
                                      )$overall["Accuracy"]
final_accuracy_validation <- tibble(
  method = "qda",
  predictors = "Temperature+Humidity+Light+CO2+HumidityRatio",
  Accuracy_validattion =accuracy_qda_test2)

# rpart with only Light predictor
set.seed(1, sample.kind = "Rounding")
accuracy_rpart_Light_test2 <- confusionMatrix(predict(train_rpart_Light, data_testing2_1),
                                              data_testing2_1$Occupancy
                                              )$overall["Accuracy"]
final_accuracy_validation <- bind_rows(
  final_accuracy_validation, 
  tibble(method = "rpart",
         predictors = "Light",
         Accuracy_validattion =accuracy_rpart_Light_test2))

# knn with 5 predictors
set.seed(1, sample.kind = "Rounding")
accuracy_knn_test2 <- confusionMatrix(predict(train_knn, data_testing2_1),
                                              data_testing2_1$Occupancy
                                              )$overall["Accuracy"]
final_accuracy_validation <- bind_rows(
  final_accuracy_validation, 
  tibble(method = "knn",
         predictors = "Temperature+Humidity+Light+CO2+HumidityRatio",
         Accuracy_validattion =accuracy_knn_test2))
# rf with 5 predictors
set.seed(1, sample.kind = "Rounding")
accuracy_rf_test2 <- confusionMatrix(predict(train_rf, data_testing2_1),
                                     data_testing2_1$Occupancy
                                     )$overall["Accuracy"]
final_accuracy_validation <- bind_rows(
  final_accuracy_validation, 
  tibble(method = "rf",
         predictors = "Temperature+Humidity+Light+CO2+HumidityRatio",
         Accuracy_validattion =accuracy_rf_test2))

final_accuracy_validation

# Section 4: Conclusion
# Although I didn't use date in the models, the final validation showed accuracy from
# 97% to 99%. Predictor Light has greatest effect on occupancy prediction in rpart model. 
# Based on my results, I would like to recommand two models: qda and rpart models. Both them have 
# accuracy 99% in validaiton data set. From the results, some models has very high accuracy
# in train data set, but the test and validation dataset accuracy is lower, there might be
# some overtraining in the models. So I think my further analysis will try to
# avoid overtraining models and imroving models accuracy. I would also like to
# investigating more date effect on the models. Another approach I think it will be good to 
# try is combine all three data sets togehter and randomly set training, test, and 
# validation data set to test models.  