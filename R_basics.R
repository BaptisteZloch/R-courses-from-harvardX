library(tidyverse)
library(dslabs)
library(dplyr)
data(murders)

class(murders) #get the type
str(murders) #get the structure
head(murders) #show the 6 first rows
murders #show the list
names(murders) #show the name of the columns

#murders$population #access to the population columns it preserves the orders of the rows in our data it's a vector
pop <- murders$population #numeric vector
length(pop) #number of item into the vector

class(murders$state) #character vector

z <- 3 == 2 #logical vector : true or false
z #print z
class(z) #logical vector

class(murders$region) #factor categorical data 
levels(murders$region) #print the categories
nlevels(murders$region) #print the number of categories in the factor
seq(1, 10) #generate the sequence spaced by 1 seq(1,10, step between)

x <- c(1, "canada", 3) #= character vector
x <- 1:5 #1 to 5 numeric vector 
y <- as.character(x) #convert the vector x into a character vector
y #print y
as.numeric(y) #convert character vector into numeric vector

#names(temp) <- city #associate 2 vectors, they can have differents types
#temp[1:3]
#temp[c(1,2,3)]#both do the same thing : access to the three first item in the vector

#rank(x) gives you the ranks of x from lowest to highest, rank(-x) gives you the ranks from highest to lowest.

x <- c(TRUE, FALSE, FALSE, TRUE, FALSE) #create de logical vector
which(x) #return the TRUE index 
index <- which(murders$state == "Massachusetts") #return the true index
index <- murders$state == "Massachusetts" #do the same thing 

index <- match(c("New york", "Florida", "Texas"), murders$states) #return the index that match with the c vector

c("Boston", "Dakota", "Washington") %in% murders$state #return false false true because only washington is a state

murders <- mutate(murders, rate = total / population * 100000) #add a new column into murders
filter(murders, rate <= 0.71) #return the states which have the condition verified

newtable <- select(murders, state, region, rate) #create new data frame with new columns as SQL do

murders %>% select(state, region, rate) %>% filter(rate <= 0.71) #this line do the operation select and filter knowing the dataframe is murders
grade <- data.frame(names = c("john", "Lily", "JEAN"), exam_1 = c(95, 80, 75), exam_2 = c(67, 80, 34))
#create a data frame with 3 columns named names, exam_1, exam_2 containing the vector c
class(grade$names) #return factor because all text are considered as factor
grade <- data.frame(names = c("john", "Lily", "JEAN"), exam_1 = c(95, 80, 75), exam_2 = c(67, 80, 34), stringsAsFactors = false) #correct that

population_in_millions <- murders$population / 10 ^ 6
total_gun_merders <- murders$total
plot(population_in_millions, total_gun_merders) #x et y 

#histograms
hist(murders$rate) #distribution 

#boxplot
boxplot(rate ~ region, data = murders) #rate~region = group by region and rate in y / data = dataframe

murder_rate <- murders$total / murders$population * 100000
ind <- which.min(murder_rate)
if (murder_rate[ind] < 0.5) {
  print(murders$state[ind])
} else {
  print("No state has murder rate < 0.5")
}

#ifelse(boolean expression, if yes, else)
data(na_example)
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example), 0, na_example) # replace the missing value by 0 the others don't change

z <- c(TRUE, TRUE, FALSE)
any(z) #return true because there is True into z
all(z) #return false because all item aren't true

avg <- function(x) {
  #variables are local inside the function
  s <- sum(x)
  l <- length(x)
  s / n
}

identical(mean(x), avg(x)) #return true because they give the same result

for (i in 1:5) {
  print(i)
}

prop.table(table(heights$sex)) #proportion