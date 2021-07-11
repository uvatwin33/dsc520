# Assignment: ASSIGNMENT 5.2
# Name: Han, Phil
# Date: 2021-07-11

library(plyr)
library(dplyr)


sport<- c("Hockey", "Baseball", "Football")
league<- c("NHL", "MLB", "NFL")
trophy<- c("Stanley Cup", "Commissioner's Trophy", "Vince Lombardi Trophy")

cbind(Sport = sport, Association = league, Prize = trophy)

rbind(Sport = sport, Association = league, Prize = trophy)

#trophies1<- cbind(sport, league, trophy)

library(purrr)
library(readxl)

setwd("~/Documents/GitHub/dsc520")
housing_df <- read_excel("data/week-7-housing.xlsx")
view(housing_df)
housing_df
dfdata = data.frame(housing_df)
dfdata

# 5.2. Q1.1 Using the dplyr package, transforming data by select 
select(dfdata, Sale.Price, Sale.Date, building_grade, bedrooms, year_built)
dfdata1<-select(dfdata, Sale.Price, Sale.Date, building_grade, bedrooms, year_built)

# Using the dplyr package, transforming data by filter with bedrooms with 4
filter(dfdata1, bedrooms==4)

# Using the dplyr package, transforming data by select and filter with bedrooms with 4
dfdata %>%
  filter(bedrooms ==4) %>%
  select(Sale.Price, Sale.Date, building_grade, bedrooms, year_built)

# Using the dplyr package, transforming data by Mutate Sale Price divided by 1,000
dfdata1 %>%
  mutate(Sale.PriceK = Sale.Price/1000)

# Using the dplyr package, transforming data by Split-apply-combine data analysis 
# and the summarize() function

dfdata1 %>%
  group_by(Sale.Price) %>%
  summarize(n())

# Using the dplyr package, transforming data by Arrange function
dfdata1 
  arrange(dfdata1, Sale.Price, year_built)
 
# 5.2 Q1.2 Using the pluck function to pull a column from the dataset and convert it into a list   
dfdata1 
  pluck(dfdata1, 1)
  
List_SalePrice <- pluck(dfdata1, 1)
List_SalePrice

# Using the keep() function from the Purrr package
keep(List_SalePrice, function(x) x>1000000)  

# Using the discard() function from the Purrr package to drop NA from the list and return the list
dfdata
  pluck(dfdata, 5)
List_SaleWarn <- pluck(dfdata, 5)
List_SaleWarn
discard(List_SaleWarn, is.na)

# Using the compact() function from the Purrr package to drop 0 years from the list
dfdata
pluck(dfdata, 19)
List_YrRenovated <- pluck(dfdata, 19)
compact(List_YrRenovated)

# 5.2 Q1.3 Use the cbind and rbind function on your dataset
library(dplyr)
dfdata1
dfdata2<-slice(dfdata1, 1:10)
dfdata2
bedrooms<-pluck(dfdata2, 4)
bedrooms
year_built<-pluck(dfdata2, 5)

building_grade<-pluck(dfdata2, 3)

cbind(bedrooms, year_built, building_grade)

rbind(bedrooms, year_built, building_grade)

# 5.2 Q1.4 Split a string, then concatenate the results back together.
dfdata
address<-pluck(dfdata, 7)
# spliting a string from the column address 
string1<-address
string1
strsplit(string1, split="  ")

# concatenate the split strings back together
string2<-strsplit(string1, split="--")
string2
paste(string2, sep =" ")
