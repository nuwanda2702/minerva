### Multilateral Development Institution Data 
foo <- read.csv("https://tinyurl.com/yb4phxx8") # read in the data 

# column names 
names(foo) 
# dimensions of the data set 
dim(foo) 
# quick look at the data structure 
head(foo) 
# take note of the columns representing calendar dates 
date.columns <- c(11, 12, 14, 15, 16, 17, 18, 25) 


#goes through every column i which has a date (specified as date.columns above) and finds row numbers of empty elements
for(i in date.columns) # loops through the "date.columns" 
{ 
  # Find missing values 
  which_values_are_missing <- which(as.character(foo[, i]) == "") 
  # Replace them by NAs 
  foo[which_values_are_missing, i] <- NA 
  # Turn values into dates 
  foo[, i] <- as.Date(as.character(foo[, i])) 
} 

#Try this yourself

foo[3,12]  # [1] "1968-03-13" 
foo[4,12] # [1] "1968-07-03" 
foo[3,12]-foo[4,12]  # Time difference of -112 days 

##Only consider projects with non-missing Circulation Date >= 2009-01-01
#Removing projects with missing Circulation Date
missing_date <- which(is.na(foo$CirculationDate))
new_foo <- foo[-missing_date,] 
head(new_foo) 
#Selecting Circulation Date >= 2009-01-01 from new_foo
foo_final <- new_foo[new_foo$CirculationDate >= "2009-01-01", ]
foo_final
str(foo_final)

### ASSIGNMENT ###
##Question 1
#1a
#Calculate the difference in two dates and its descriptive stats
difference_1a <- foo_final$OriginalCompletionDate - foo_final$ApprovalDate 
mean_1a <- mean(as.numeric(difference_1a),na.rm = TRUE)
#removing the NA data
sum(is.na(difference_1a))
mean_1a
mean_1a/30

#1b
#Calculate the difference in the delay and its descriptive stats
delay <- foo_final$RevisedCompletionDate - foo_final$OriginalCompletionDate
#removing the NA data
sum(is.na(delay))
mean(delay, na.rm = TRUE)
median(delay, na.rm = TRUE)
quantile(delay,na.rm = TRUE)

#1c
#original_duration is calculated in 1a and stored in difference_1a
#calculate the descriptive stats for original duration
mean(difference_1a, na.rm = TRUE)
median(difference_1a, na.rm = TRUE)
quantile(difference_1a,na.rm = TRUE)
#actual duration is calculated as follows:
#calculate the descriptive stats for the actual duration
actual_duration <- foo_final$RevisedCompletionDate - foo_final$ApprovalDate 
sum(is.na(actual_duration))
mean(actual_duration, na.rm = TRUE)
median(actual_duration, na.rm = TRUE)
quantile(actual_duration,na.rm = TRUE)

##Question 2
#Calculate the percentage of each rating level within the subset of conditioned RevisedCompletionDate, leaving out the NA entries
percentage_rating_0 <- 100* sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating==0,na.rm=TRUE)/sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating != "NA",na.rm=TRUE)
percentage_rating_0
percentage_rating_1 <- 100* sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating==1,na.rm=TRUE)/sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating != "NA",na.rm=TRUE)
percentage_rating_1
percentage_rating_2 <- 100* sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating==2,na.rm=TRUE)/sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating != "NA",na.rm=TRUE)
percentage_rating_2
percentage_rating_3 <- 100* sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating==3,na.rm=TRUE)/sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating != "NA",na.rm=TRUE)
percentage_rating_3

##Question 3
#Calculate the percentage of each rating level within the subset of conditioned RevisedCompletionDate, and PATA type
pata_percentage_rating_0 <- 100* sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating==0 & foo_final$Type =="PATA",na.rm=TRUE)/sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating != "NA" &foo_final$Type =="PATA",na.rm=TRUE)
pata_percentage_rating_0
pata_percentage_rating_1 <- 100* sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating==1&foo_final$Type =="PATA",na.rm=TRUE)/sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating != "NA"&foo_final$Type =="PATA",na.rm=TRUE)
pata_percentage_rating_1
pata_percentage_rating_2 <- 100* sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating==2&foo_final$Type =="PATA",na.rm=TRUE)/sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating != "NA"&foo_final$Type =="PATA",na.rm=TRUE)
pata_percentage_rating_2
pata_percentage_rating_3 <- 100* sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating==3&foo_final$Type =="PATA",na.rm=TRUE)/sum(foo_final$RevisedCompletionDate>="2010-01-01" & foo_final$Rating != "NA"&foo_final$Type =="PATA",na.rm=TRUE)
pata_percentage_rating_3

##Question 4
n <- 10 
top_10 <- foo_final[foo_final$RevisedAmount >quantile(foo_final$RevisedAmount,probs =1-n/100),] #subseting the top 10%
m <- 90
bottom_10 <- foo_final[foo_final$RevisedAmount < quantile(foo_final$RevisedAmount,probs =1-m/100),] #subseting the bottom 10%
#comparison of the rating
#top10
100*sum(top_10$Rating==0,na.rm=TRUE)/115 
100*sum(top_10$Rating==1,na.rm=TRUE)/115  
100*sum(top_10$Rating==2,na.rm=TRUE)/115 
100*sum(top_10$Rating==3,na.rm=TRUE)/115 
#bottom10
100*sum(bottom_10$Rating==0,na.rm=TRUE)/100
100*sum(bottom_10$Rating==1,na.rm=TRUE)/100  
100*sum(bottom_10$Rating==2,na.rm=TRUE)/100 
100*sum(bottom_10$Rating==3,na.rm=TRUE)/100 