# Assignment: ASSIGNMENT 4.2
# Name: Han, Phil
# Date: 2021-07-04

install.packages("ggplot2")
install.packages("qqplotr")
install.packages("plyr")
install.packages("dplyr")
install.packages('readxl')
install.packages("tidyverse")

library(ggplot2)
library(qqplotr)
library(purrr)
library(plyr)
library(dplyr)
library(readxl)
require(plyr)



theme_set(theme_minimal())

setwd("~/Documents/GitHub/dsc520")
## Load the `data/asc14.csv` to
scores_df <- read.csv("data/scores.csv")
scores_df

# 4.2.1 Question 1 -- Answer: Observation units are scores

# 4.2.1 Question 2 -- Answer: Section is categorical and Score is quantitative.

# 4.2.1 Question 3
reg_score<-filter(scores_df, Section=="Regular")
reg_score

scores_df
sport_score<-filter(scores_df, Section=="Sports")
sport_score

## 3. Create a histogram of the `Regular` variable using `geom_histogram()`
## 4. Add a normal curve to the Histogram
## Use 10 bins
scores_df
reg_score<-filter(scores_df, Section=="Regular")
reg_score

ggplot(reg_score, aes(Score)) + geom_histogram(aes(y=stat(density)), bins=7, fill="sky blue", col="grey") + 
  ggtitle("Title: Regular Section vs Number of Students") + xlab("Regular Section") + 
  ylab("Count of Students (density)")+geom_density(col="red")


## 3. Create a histogram of the `Sports` variable using `geom_histogram()`
## 4. Add a normal curve to the Histogram
## Use 10 bins

scores_df
sport_score<-filter(scores_df, Section=="Sports")
sport_score

# 4.2.1. Question #4.1 A:Regular section seems to have scored more points given the fact
#        the distribution curve looks more normal
#        #4.2 A: No, some students in Sports section scored more (upwards of over 400 pts).
#               Those in the Regular section show a normal curve slightly skewed to the left 
#               whereas in the Sports section show a normal curve slightly skewed to the right.

#        #4.3. One variable that was not mentioned was an overlapping sample population in
#              regular section where the applicants also played or participated in sports.

ggplot(sport_score, aes(Score)) + geom_histogram(aes(y=stat(density)), bins=7, fill="sky blue", col="grey") + 
  ggtitle("Title: Sports Section vs Number of Students") + xlab("Sports Section") + 
  ylab("Count of Students (density)")+geom_density(col="red")

# load housing data in an Excel dataset 
# 4.2

setwd("~/Documents/GitHub/dsc520")
housing_df <- read_excel("data/week-7-housing.xlsx")
view(housing_df)
housing_df
dfdata = data.frame(housing_df)
dfdata

#Selecting a variable Sale Price
#4.2 Q1. Applying apply to sum a column for Sale Price
SalePrice<-select(housing_df, 'Sale Price')
SalePrice

apply(SalePrice, 2, sum)  # apply function on a variable Sale Price
apply(housing_df, 2, mean, na.rm = TRUE)

housing_df
dfdata = data.frame(housing_df)
dfdata
apply(dfdata, 2, mean, na.rm = TRUE)
apply(dfdata, 2, mean) 

#4.2.2. Q2 Applying aggregate function to calculate mean of Sale Price by year_built
housing_df
dfdata = data.frame(housing_df)
dfdata

aggregate(Sale.Price ~ year_built, dfdata, mean) # aggregate function on Mean of Sale Price by Built Year 

# 4.2.2. Q3. Applying Pull Function to pull a column Sale.Price
new_dt<-pull(dfdata, Sale.Price)
new_dt
rename (new_dt, Sale.Price = SalePrice)


# 4.2.2 Q3. Select Function to extract year_built
new_dt1<-select(dfdata, 'year_built')
new_dt1

# 4.2.2.Q3. Bind the new columns together
bind_cols(new_dt, new_dt1)

# checking the distribution of data

#new_dt2<-bind_cols(new_dt, new_dt1)
#new_dt2
#rename (new_dt2, Count, Sale.Price, Year_Built)

# 4.2.2. Q4. Check the distribution of the data
housing_df
dfdata = data.frame(housing_df)
dfdata

aggregate(Sale.Price ~ year_built, dfdata, mean) # aggregate function on Mean of Sale Price by Built Year 
dfdata1<-aggregate(Sale.Price ~ year_built, dfdata, mean)

# 4.2 Question 4 
ggplot(dfdata1, aes(Sale.Price)) + geom_histogram(aes(y=stat(density)), bins=10, fill="sky blue", col="grey") + 
  ggtitle("Title: Avg. Sale Price vs Year Built") + xlab("Avg. Sale Price") + 
  ylab("Year Built (density)")+geom_density(col="red")

# 4.2 Question 5
# Answer: Yes, in the years 1918, 1927 & 1929, average sale price went over $1M to $1.2M.  
# And in 1935 and 1938, average sale price went over $1.6M.

# 4.2 Create 2 new variables
dfdata %>%
  mutate(Sale.Price = Sale.Price/1000)

# 1st New Variable is Sale Price in thousands
New_SalePrice <-  mutate(Sale.Price = Sale.Price/1000)

# 2nd New Variable is Mean Sale Price
aggregate(Sale.Price ~ year_built, dfdata, mean) 
Mean_SalePrice <-aggregate(Sale.Price ~ year_built, dfdata, mean) 
Mean_SalePrice
# 3rd New Variable is Mean Sale Price
aggregate(Sale.Price ~ year_built, dfdata, median) 
Median_SalePrice <-aggregate(Sale.Price ~ year_built, dfdata, median) 
Mean_SalePrice
