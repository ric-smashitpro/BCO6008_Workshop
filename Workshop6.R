library(tidymodels)
data(ames)

ames$Sale_Price%>%head()

library(skimr)
skim(ames)


ames<-ames%>%mutate(Sale_Price = log10(Sale_Price))
set.seed(123)
ames_split<-initial_split(ames, prop = 0.8, strata = Sale_Price)
