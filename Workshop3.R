library(tidyverse)
library(tidymodels)
library(skimr)
library(janitor)


muffin_cupcake_data_original<-read_csv("https://raw.githubusercontent.com/adashofdata/muffin-cupcake/master/recipes_muffins_cupcakes.csv")

muffin_cupcake_data_original%>%skim()

# Clean variable names
muffin_cupcake<-muffin_cupcake_data_original%>%
  clean_names()

#count the types - cupcake is 10 and muffin = 10. This is called balanced dataset.
muffin_cupcake%>%count(type)


# Splitting the cleaned dataset into training vs test datasets - internal structure
muffin_cupcake_split<-initial_split(muffin_cupcake)

# Shows how the dataset is split into. Split into 15 variables and 5 variables.
# <Analysis/Assess/Total>
# <15/5/20>
muffin_cupcake_split

# Save training and testing datasets separately
muffin_cupcake_training<-training(muffin_cupcake_split)

muffin_cupcake_testing<-testing(muffin_cupcake_split)

# Create a recipe
muffin_recipe<-recipe(type~flour+milk+sugar+egg, data = muffin_cupcake_training)

muffin_recipe 

#role #variables
#outcome 1
#predictor 3

#add egg and predictor will be 4


# Writing recipe steps
muffin_recipe_steps<-muffin_recipe%>%
  step_meanimpute(all_numeric())%>%
  step_center(all_numeric())%>%
  step_scale(all_numeric())

muffin_recipe_steps


# Preparing the recipe
prepped_recipe<-prep(muffin_recipe_steps, training = muffin_cupcake_training)
prepped_recipe

# Bake
muffin_train_preprocessed<-bake(prepped_recipe, muffin_cupcake_training)

muffin_testing_preprocessed<-bake(prepped_recipe, muffin_cupcake_testing)
