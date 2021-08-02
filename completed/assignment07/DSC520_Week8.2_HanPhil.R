# Assignment: ASSIGNMENT 8.2
# Name: Han, Phil
# Date: 2021-07-31

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




## Set the working directory to the root of your DSC 520 directory
setwd("~/Documents/GitHub/dsc520")

## Load the `data/housing_df` to
housing_df <- read_excel("data/week-7-housing.xlsx")
view(housing_df)
housing_df

housed_df = data.frame(housing_df)

## 8.2.1 
# I mutated the sale price data by dividing by a thousand.  Later on, I selected four or so
# variables using plyr tools and combined a column--standardized residual-- with a transformed data frame

# mutate housing sale price in thousands
house_df %>%
  mutate(Sale.Price = Sale.Price/1000)
house_df

house_lm<-lm(Sale.Price ~ sq_ft_lot, data=house_df)
house_lm

summary(house_lm)

new_house_df<- data.frame(sale_price = predict(house_lm, house_df), square_ft = house_df$sq_ft_lot)
new_house_df

ggplot(data = house_df, aes(y = Sale.Price, x = sq_ft_lot)) +
  geom_point(color='blue') +
  geom_line(color='red', data = new_house_df, aes(y = sale_price, x=square_ft)) 
  #geom_smooth(method="lm")


## 8.2.2  Create two variables and more variables
## I selected square foot lot (sq_ft_lot), bedroom, & square foot total livingroom
## since they all present some type of space within or surrounding the houses.

house_df = data.frame(housing_df)
house_df

#dfdata %>%
#mutate(Sale.Price = Sale.Price/1000)
housing_df
housed_df = data.frame(housing_df)

# mutate housing sale price in thousands
house_df <-housed_df %>%
  mutate(Sale.Price = Sale.Price/1000)

house_df

multi_house_df<- data.frame(sale_price = predict(multihouse_lm, house_df), sq_ft_lot = house_df$sq_ft_lot, 
                            bd_room=house_df$bedrooms, bath_full=house_df$bath_full_count, 
                            sq_ft_LR = house_df$square_feet_total_living)
multi_house_df

multihouse_lm<-lm(sale_price ~ sq_ft_lot + bd_room + bath_full + sq_ft_LR, data=multi_house_df)
multihouse_lm


ggplot(data = house_df, aes(y = Sale.Price, x = sq_ft_lot)) +
  geom_point(color='blue') +
  geom_line(color='red', data = multi_house_df, aes(y = sale_price, x = sq_ft_lot)) 

## 8.2.3 The R2 and Adjusted R2
# Both the R2 and Adjusted R2 generally explain the goodness of a fit in a regression model.
house_df
house_lm_std<-lm(scale(Sale.Price)~scale(sq_ft_lot), data=house_df)
summary(house_lm_std)

new_house1_df<- data.frame(sale_price = predict(house_lm_std, house_df), square_ft = house_df$sq_ft_lot)
new_house1_df

ggplot(data = house_df, aes(y = Sale.Price, x = sq_ft_lot)) +
  geom_point(color='blue') +
  #geom_line(color='red', data = new_house1_df, aes(y = sale_price, x=square_ft)) 
  geom_smooth(method="lm", color='red')

house_df

multihouse_lm_std<-lm(scale(Sale.Price) ~ scale(sq_ft_lot) + scale(bedrooms) + scale(bath_full_count) + scale(square_feet_total_living), data=house_df, na.omit())
multihouse_lm_std

## 8.2.4 standardized betas for each parameter of the multiple regression model
summary(multihouse_lm_std)

multi_house_df<- data.frame(sale_price = predict(multihouse_lm, house_df), square_ft = house_df$sq_ft_lot, 
                            bd_room=house_df$bedrooms, bath_full=house_df$bath_full_count, 
                            sq_LR_total = house_df$square_feet_total_living)
multi_house_df

ggplot(data = house_df, aes(y = Sale.Price, x = sq_ft_lot)) +
  geom_point(color='blue') +
  geom_line(color='red', data = multi_house_df, aes(y = sale_price, x=square_ft))

## 8.2.5 Confidence Intervals
house_df

multihouse_lm<-lm(Sale.Price ~ sq_ft_lot + bedrooms + bath_full_count + square_feet_total_living, data=house_df)
multihouse_lm

multi_house_df<- data.frame(sale_price = predict(multihouse_lm, house_df), sq_ft_lot = house_df$sq_ft_lot, 
                            bd_room=house_df$bedrooms, bath_full=house_df$bath_full_count, 
                            sq_ft_LR = house_df$square_feet_total_living)
multi_house_df

#multihouse_lm<-lm(sale_price ~ sq_ft_lot + bd_room + bath_full + sq_ft_LR, data=multi_house_df)
#multihouse_lm

#multi_house_df

predict(multihouse_lm, multi_house_df, interval="confidence")


## 8.2.6 When I changed the x-axis with a variable square foot total living room, the regression
#       model improved a lot while the fitness of this multi regression model looked much better.

ggplot(data = multi_house_elm_df, aes(y = sale_price, x = sq_LR_total)) +
  geom_point(color='blue') +
  #geom_line(color='red', data = multi_house_df, aes(y = sale_price, x=sq_LR_total))
  geom_smooth(method="lm", color='red')

#multi_house_elm1_df <- data.frame()


##8.2.7  Identify and removing Outliers
Q<- quantile(multi_house_df$sale_price, probs=c(.25, .75), na.rm = FALSE)
Q1<- quantile(multi_house_df$square_ft, probs=c(.25, .75), na.rm = FALSE)
Q1


iqr<- IQR(multi_house_df$sale_price)
iqr

up<- Q[2] + 1.5*iqr # Upper Range
low<- Q[1] - 1.5*iqr # Lower Range

up
low

multi_house_elm_df <- subset(multi_house_df, multi_house_df$sale_price > (Q[1] - 1.5*iqr) & multi_house_df$sale_price < (Q[2]+1.5*iqr))
multi_house_elm_df1<- subset(multi_house_elm_df, multi_house_elm_df$square_ft > (Q[1] - 1.5*iqr) & multi_house_elm_df$square_ft < (Q[2]+1.5*iqr))
multi_house_elm_df


##8.2.8 Calculate the standardized residuals
standard_res <- rstandard(multihouse_lm_std)

standard_res

final_data <- cbind(multi_house_elm, standard_res)
final_data[order(-standard_res),]

filtered_final_data_less_2<-filter(final_data, standard_res <= -2)
filtered_final_data_less_2

filtered_final_data_more_2<-filter(final_data, standard_res >= 2)
filtered_final_data_more_2

##8.2.9  Sum up some of the large residuals
  #with(filtered_final_data_more_2, sum(sale_price[standard_res > 5]))

  #with(filtered_final_data_less_2, sum(sale_price[standard_res <= -2]))
sum(as.numeric(filtered_final_data_less_2$standard_res), na.rm = TRUE)

sum(as.numeric(filtered_final_data_more_2$standard_res), na.rm = TRUE)

## 8.2.10 Variables wiht large residuals

summary(multihouse_lm)
resid(multihouse_lm_std)

 #sq_ft_lot and sq_livingroom_total variables have large residuals.

## 8.2.11  Calculate the leverage, cooks distance, and covariance

# calculate leverage for each observation
leverage_house <- as.data.frame(hatvalues(multihouse_lm))
leverage_house  

# calculate cooks distance
cooks.distance(multihouse_lm)

# calculate the covariance ratios between sale price and sq livingroom total
cov(multi_house_df$sale_price, multi_house_df$sq_LR_total)

## 8.2.12.
summary(multihouse_lm)

table(multi_house_df$sale_price, multi_house_df$sq_ft_lot)

# chi test of independence

c_test <- chisq.test(table(multi_house_df$sale_price, multi_house_df$sq_ft_lot))
c_test

# X^2 test statistic and the p-value
c_test$statistic

c_test$p.value

# running the diagnostic plots
install.packages('ggfortify')
library(ggfortify)

autoplot(multihouse_lm)
# In looking at Homegeneity of variance in the Scale-Location plot,
# many of the values for the residual poinsts do not seem to be fitted at all.  
# They are rather concentrated in the lower left-hand corner of the graph.
# Thus, the condition is not met.

## 8.2.13  Calculate to assess the assumption of no multicollinearity.

install.packages('caret')
install.packages('car')

library(caret)
library(car)

vif_test<-vif(multihouse_lm)

car::vif(multihouse_lm)

# the variance inflation factor (or VIF)  that exceeds 5 or 10 indicates a 
# problematic amount of collinearity.  It doesn't appear that the variables
# used --sq_ft_lot, bedrooms, bath_full_count, and sq_ft_total_living exceed
# more than 2. Thus, no multicollinearity seems to exist.

## 8.2.14.
plot(sale_price ~ sq_ft_lot, data = multi_house_df)
# Again, the relationship between sale price and square foot lot is not even remotely
# close to being linear.  

multi_house_df
plot(sale_price ~ sq_ft_LR, data = multi_house_df)

hist(multi_house_df$sale_price)
# Based on the histogram, the distribution is skewed to the right where its mean is
# generally to the right side of the median.  This result seems to be caused by
# smaller size of the houses with relatively cheaper prices were preferred and bought.  
# in other words, there is more or less homogenous types of groups in the population.

## 8.2.15 
summary(multihouse_lm)
# Both multiple R-squared (0.2122) and Adjusted R-squared (0.21119) are relatively
# low as shown by the not good fittedness.  The residual plot from Q11 above shows
# that the bias exits, meaning the variances are bigger.  Also, homogenous groups that exist in the
# population probably caused the bias since the predicted values are not small enough.
# In other words, the plot shows that data or values are not scattered along the fitted line.
# Thus, we cannot conclusively state that the regression model is unbiased.
