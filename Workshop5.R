# install.packages("MASS")
# install.packages("ISLR")

library(tidymodels)
library(MASS)
library(ISLR)

set.seed(123)


# 1 Specify the model: type, mode and engine
lm_spec<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")

lm_spec

data(Boston)

# 2 Take model specification from step 1 and apply to the data. Use fit() and put formula y~x
lm_fit<-lm_spec%>%
  fit(data=Boston, medv~lstat)  #medv is the response variable and lstat is the predictor

lm_fit

lm_fit%>%pluck("fit")%>%
  summary()

tidy(lm_fit)

# 3 Use the fitted model from step 2 to predict new y in new data (can be testing data or a new data set)
predict(lm_fit, new_data = Boston)

# examining new predicted values 
final_model<-augment(lm_fit, new_data = Boston)

############################################################################################################
rm(list=ls())

data(Boston)

#1
model_spec<-linear_reg()%>%
  set_mode("regression")%>%  #regression or classification but regression is most always used
  set_engine("lm") #must be specified within double quotes

#2
model_fit<-model_spec%>%
  fit(data = Boston, medv~age+crim+rm)

#3
#model_predicted<-predict(model_fit, new_data = Boston)

model_predicted_augment<-augment(model_fit, new_data = Boston)

###############################################################################################################
rm(list=ls())

data(Boston)

#1
model_lm_spec<-linear_reg()%>%
  set_mode("regression")%>%
  set_engine("lm")


#using different spec models e.g. Random Forest model from Parsnip package
model_rf_spec<-rand_forest()%>%
  set_mode("regression")%>%
  set_engine("ranger")

#install.packages("ranger")
library(ranger)


#2
model_lm_fit<-model_lm_spec%>%
  fit(data = Boston, medv~.)

#3
model_lm_augm<-augment(model_lm_fit, new_data = Boston)

# Iris Dataset
data(iris)
