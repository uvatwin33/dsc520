# Assignment: ASSIGNMENT 3.2
# Name: Han, Phil
# Date: 2021-06-27

install.packages("qqplotr")

library(ggplot2)
library(qqplotr)
theme_set(theme_minimal())


## Load the `data/asc14.csv` to
acs14_df <- read.csv("data/acs-14-1yr-s0201.csv")
acs14_df

## 2. output for the following functions: str();nrow(); ncol()
str(acs14_df)
nrow(acs14_df)
ncol(acs14_df)

## 3. Create a histogram of the `HSDegree` variable using `geom_histogram()`
## 4. Add a normal curve to the Histogram
## Use 10 bins
ggplot(acs14_df, aes(HSDegree)) + geom_histogram(aes(y=stat(density)), bins=10, fill="sky blue", col="grey") + 
  ggtitle("Title: HS Degree vs Population") + xlab("HSDegree(% of Population)") + 
  ylab("Population (density)")+geom_density(col="red")

## 5. Create a Probability Plot of the HSDegree variable
ggplot(acs14_df, aes(sample = HSDegree)) + geom_qq(aes(sample = HSDegree), col="green")


## normalize the data using stat.desc() function
install.packages("pastecs")
library(pastecs)
stat.desc(acs14_df, basic=TRUE, desc=TRUE, norm=FALSE, p=0.95)


