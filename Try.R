rm(list = ls())
install.packages("tidyverse")
install.packages("naniar")
library(tidyverse)
library(naniar)
library(dplyr)


# Put data files outside of the git folder in order to avoid pushing too large files to repository
path_to_data <- 'D:/..../payment_dates_final.csv'
data_collection <- read.csv(path_to_data)

#check format of data
str(data_collection)

# Convert columns to correct data type 
data_collection <- data_collection %>%  mutate(product_type = as.factor(product_type))
data_collection <- data_collection %>%  mutate(business_discount = as.factor(business_discount))
data_collection <- data_collection %>%  mutate(gender = as.factor(gender))
data_collection <- data_collection %>%  mutate(marital_status = as.factor(marital_status))
data_collection <- data_collection %>%  mutate(clients_phone = as.factor(clients_phone))
data_collection <- data_collection %>%  mutate(client_mobile = as.factor(client_mobile))
data_collection <- data_collection %>%  mutate(client_email = as.factor(client_email))
data_collection <- data_collection %>%  mutate(living_area = as.factor(living_area))
data_collection <- data_collection %>%  mutate(different_contact_area = as.factor(different_contact_area))
data_collection <- data_collection %>%  mutate(kc_flag = as.factor(kc_flag))
data_collection <- data_collection %>%  mutate(cf_val = as.factor(cf_val))
data_collection <- data_collection %>%  mutate(kzmz_flag = as.factor(kzmz_flag))
data_collection <- data_collection %>%  mutate(due_amount = as.numeric(due_amount))
data_collection <- data_collection %>%  mutate(payed_ammount = as.numeric(payed_ammount))
#date format for date,if we dont do that, we would have the date in charakter type
data_collection <- data_collection %>%  mutate(due_date = as.Date(due_date),format="%Y-%m-%d")
data_collection <- data_collection %>%  mutate(payment_date = as.Date(payment_date),format="%Y-%m-%d")

#check if the data are in the right format 
str(data_collection)

##add column for delay in days
data <- data_collection
delay <- difftime(data$payment_date,data$due_date, tz, units = "days")
data$delay <- delay
data <- data %>%  mutate(delay = as.numeric(delay))

#statistic of data all
summary(data)
#number of rows
nrow <- nrow(data)
#number of columns
ncol <- ncol(data)

####### Generate test and train data ######## 
#fix random generator
set.seed(2020)
n_train <- round(0.08*nrow)
index_train <- sample(1:nrow,n_train)

DTrain <- data[index_train, ]
DTest <- data[-index_train, ]

#summary to find if data have NAs
summary(DTrain)
summary(DTest)
# Detailed summary of data 
install.packages("skimr")
library(skimr)
Dtrainskim <- skim(DTrain)
Dtestskrim <- skim(DTest)
##see all NAs for all dataset
skim(DTrain)
skim(DTest)

##create a new data set without NAs
DTrain_new <- na.omit(DTrain)
DTest_new <- na.omit(DTest)
#number of column and row and summary in data train w-o NAs
dim(DTrain_new)  #number of columns and rows for clean data Train
summary(DTrain_new)

#number of column and row and summary in data test w-o NAs
dim(DTest_new)   #number of columns and rows for clean data Test
summary(DTest_new)


##sumarry for each atrributy
headofTable <- c("Num. of Children","Num. Other Product","Year of Birth","Due amount","payed ammount","delay") 
EX <- c(mean(DTrain_new$number_of_children),mean(DTrain_new$number_other_product),mean(DTrain_new$birth_year),mean(DTrain_new$due_amount), mean(DTrain_new$payed_ammount),mean(DTrain_new$delay))
VarX <- c(var(DTrain_new$number_of_children),var(DTrain_new$number_other_product),var(DTrain_new$birth_year),var(DTrain_new$due_amount), var(DTrain_new$payed_ammount), var(DTrain_new$delay))
Median <- c(median(DTrain_new$number_of_children),median(DTrain_new$number_other_product),median(DTrain_new$birth_year),median(DTrain_new$due_amount), median(DTrain_new$payed_ammount), median(DTrain_new$delay))
Q1 <- c(quantile(DTrain_new$number_of_children,probs = 1/4),quantile(DTrain_new$number_other_product,probs = 1/4),quantile(DTrain_new$birth_year,probs = 1/4),quantile(DTrain_new$due_amount,probs = 1/4), quantile(DTrain_new$payed_ammount,probs = 1/4),quantile(DTrain_new$delay,probs = 1/4))
Q3 <- c(quantile(DTrain_new$number_of_children,probs = c(3/4)),quantile(DTrain_new$number_other_product,probs = c(3/4)),quantile(DTrain_new$birth_year,probs = c(3/4)),quantile(DTrain_new$due_amount,probs = c(3/4)), quantile(DTrain_new$payed_ammount,probs = c(3/4)),quantile(DTrain_new$delay,probs = 3/4))
Min <- c(min(DTrain_new$number_of_children),min(DTrain_new$number_other_product),min(DTrain_new$birth_year),min(DTrain_new$due_amount), min(DTrain_new$payed_ammount), min(DTrain_new$delay))
Max <- c(max(DTrain_new$number_of_children),max(DTrain_new$number_other_product),max(DTrain_new$birth_year),max(DTrain_new$due_amount), max(DTrain_new$payed_ammount), max(DTrain_new$delay))
  
summaryDTrain <- distinct(data.frame(headofTable,EX,VarX,Median,Q1,Q3,Min,Max, check.rows = FALSE, check.names = FALSE))

############## exploring Data ######################## 

#### Train data statistic ####
#statistic addiction delay on gender 
meanG_D<- DTrain_new %>% 
    group_by(gender) %>% 
    summarise(mean = mean(delay))

medG_D <- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(med = median(delay))
   
maxG_D<- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(max = max(delay))

minG_D<- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(min = min(delay))

Q1G_D<- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(Q1 = quantile(delay, probs = 1/4))

Q3G_D<- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(Q3 = quantile(delay, probs = 3/4))

data_GD <- data.frame(meanG_D,medG_D[,2],minG_D[,2],maxG_D[,2],Q1G_D[,2], Q3G_D[,2],check.names = FALSE)    

# statistic addiction payed ammount to gender
meanG_PA <-DTrain_new %>% 
  group_by(gender) %>% 
  summarise(mean = mean(payed_ammount))

medG_PA <- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(med = median(payed_ammount))

maxG_PA<- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(max = max(payed_ammount))

minG_PA<- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(min = min(payed_ammount))

Q1G_PA<- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(Q1 = quantile(payed_ammount, probs = 1/4))

Q3G_PA<- DTrain_new %>% 
  group_by(gender) %>% 
  summarise(Q3 = quantile(payed_ammount, probs = 3/4))

data_GPA <- data.frame(meanG_PA,medG_PA[,2],minG_PA[,2],maxG_PA[,2],Q1G_PA[,2], Q3G_PA[,2],check.names = FALSE)
 ##### test data statictic #####
##statistic addiction delay on gender
TmeanG_D<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(mean = mean(delay))

TmedG_D <- DTest_new %>% 
  group_by(gender) %>% 
  summarise(med = median(delay))

TmaxG_D<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(max = max(delay))

TminG_D<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(min = min(delay))

TQ1G_D<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(Q1 = quantile(delay, probs = 1/4))

TQ3G_D<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(Q3 = quantile(delay, probs = 3/4))

Tdata_GD <- data.frame(TmeanG_D,TmedG_D[,2],TminG_D[,2],TmaxG_D[,2],TQ1G_D[,2], TQ3G_D[,2],check.names = FALSE)    

# statistic addiction payed ammount to gender
TmeanG_PA <-DTest_new %>% 
  group_by(gender) %>% 
  summarise(mean = mean(payed_ammount))

TmedG_PA <- DTest_new %>% 
  group_by(gender) %>% 
  summarise(med = median(payed_ammount))

TmaxG_PA<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(max = max(payed_ammount))

TminG_PA<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(min = min(payed_ammount))

TQ1G_PA<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(Q1 = quantile(payed_ammount, probs = 1/4))

TQ3G_PA<- DTest_new %>% 
  group_by(gender) %>% 
  summarise(Q3 = quantile(payed_ammount, probs = 3/4))

Tdata_GPA <- data.frame(TmeanG_PA,TmedG_PA[,2],TminG_PA[,2],TmaxG_PA[,2],TQ1G_PA[,2], TQ3G_PA[,2],check.names = FALSE)

# addiction payed ammount to gender, product type and busines discount 
DTest_new %>% 
  group_by(gender,product_type, business_discount) %>%
  summarise(payedAmmount = mean(payed_ammount)) %>%
  spread(gender, payedAmmount)

DTest_new %>% 
  group_by(gender,number_of_children) %>%
  summarise(delay = mean(delay)) %>%
  spread(gender, delay)
