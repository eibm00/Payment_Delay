
### Dependencies ---------------------------------------------------------------
rm(list = ls()) #clear

if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(naniar)) install.packages("naniar")
if(!require(naniar)) install.packages("dplyr") 
if(!require(styler)) install.packages("styler") 
if(!require(skimr)) install.packages("skimr")

library(tidyverse)
library(naniar)
library(dplyr)
library(styler)
library(skimr)

### Load the initial data ------------------------------------------------------

# Put data files outside of the git folder in order to avoid pushing too large
#   files to repository
# path_to_data <- 'D:/..../payment_dates_final.csv'
# path_to_data <- "D:/Dokumenty/ŠKOLA/4IT439 Data-X – aplikované analytické datové modely v reálných úlohách/Semestrálka/payment_dates_final.csv"
path_to_data <- "..\\payment_dates_final.csv"
data_collection <- read.csv(path_to_data)

### Data understanding ---------------------------------------------------------

# Data description -------------------------------------------------------------

# Data volume (number of rows and columns)
nrow <- nrow(data_collection)
ncol <- ncol(data_collection)

# Convert columns to the correct data type
data_collection <- data_collection %>%
  mutate(due_date = as.Date(due_date, format = "%Y-%m-%d"))
data_collection <- data_collection %>%
  mutate(payment_date = as.Date(payment_date, format = "%Y-%m-%d"))
data_collection <- data_collection %>%
  mutate(product_type = as.factor(product_type))
data_collection <- data_collection %>%
  mutate(business_discount = as.factor(business_discount))
data_collection <- data_collection %>%
  mutate(gender = as.factor(gender))
data_collection <- data_collection %>%
  mutate(marital_status = as.factor(marital_status))
data_collection <- data_collection %>%
  mutate(clients_phone = as.factor(clients_phone))
data_collection <- data_collection %>%
  mutate(client_mobile = as.factor(client_mobile))
data_collection <- data_collection %>%
  mutate(client_email = as.factor(client_email))
data_collection <- data_collection %>%
  mutate(total_earnings = factor(total_earnings, labels = c(
    "level1", "level2", "level3", "level4",
    "level5", "level6", "level7", "level8",
    "level9", "level10", "not_declared"
  )))
data_collection <- data_collection %>%
  mutate(living_area = as.factor(living_area))
data_collection <- data_collection %>%
  mutate(different_contact_area = as.factor(different_contact_area))
data_collection <- data_collection %>%
  mutate(kc_flag = as.factor(kc_flag))
# this shouldn´t be factor, content of the data does not correspond to data description
#data_collection <- data_collection %>%
# mutate(cf_val = as.factor(cf_val))
#data_collection <- data_collection %>%
# mutate(cf_val = as.numeric(cf_val))
data_collection <- data_collection %>%
  mutate(kzmz_flag = as.factor(kzmz_flag))
data_collection <- data_collection %>%
  mutate(due_amount = as.numeric(due_amount))
data_collection <- data_collection %>%
  mutate(payed_amount = as.numeric(payed_ammount))
data_collection <- data_collection %>%
  mutate(contract_status = as.factor(contract_status))


# Remove column "payed_ammount" which was replaced by column "payed_amount"
data_collection <- subset(data_collection, select = -payed_ammount)

# Display the internal structure of the data
str(data_collection)

# Summary statistics of the data
# Check attribute value ranges, coverage, NAs occurance
summary <- summary(data_collection)
print(summary)
detailed_statistics <- skim(data_collection)
print(detailed_statistics)

# Analyze attribute correlations

# Data exploration--------------------------------------------------------------

# Analyze properties of interesting attributes in detail include graphs and plots


# Verify data quality ----------------------------------------------------------

# Are there missing values in the data? If so, how are they represented, where
# do they occur, and how common are they?
variables_miss <- miss_var_summary(data_collection)
print(variables_miss)
# different_contract_area missing 20%, cf_val living_area kc_flag missing 19,9%
# 1173 payment date missing, not yet paid
gg_miss_var(data_collection)

# more characteristics missing at the same time
data_collection %>%
  gg_miss_var(facet = total_earnings)

# what with NAs in payment order
# to do payment order id!!

# Check for plausibility of values
for (i in c(1:ncol)){
  if(is.factor(data_collection[,i])){
    print(colnames(data_collection[i]))
    print(prop.table(table(data_collection[,i])))
    cat(sep="\n\n")
  }
}

# Check interesting coverage
# most products are type 1
ggplot(data = data_collection, aes(x = product_type)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$product_type)

# most payment orders have discount
ggplot(data = data_collection, aes(x = business_discount)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$business_discount)

# contract status mostly 1, then 5,6,8,7 some 2,3,4...What does it mean??
ggplot(data = data_collection, aes(x = contract_status)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$contract_status)

# marital status mostly 3, some 4,2,6 5 and 1 mostly not...What does it mean?
ggplot(data = data_collection, aes(x = marital_status)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$marital_status)

# mostly 0 children, drops with number 
prop.table(table(data_collection$number_of_children))

#  mostly 1 other product, drops with number
prop.table(table(data_collection$number_other_product))

# almost no email contact
ggplot(data = data_collection, aes(x = client_email)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$client_email)

# total earning level mostly not declared  
ggplot(data = data_collection, aes(x = total_earnings)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$total_earnings)

# mostly not different contact area
ggplot(data = data_collection, aes(x = different_contact_area)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$different_contact_area)

# mostly false KC flag - mostly owns local citizenship
ggplot(data = data_collection, aes(x = kc_flag)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$kc_flag)


# mostly false KZMZ flag  mostly did not fill employer
ggplot(data = data_collection, aes(x = kzmz_flag)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))
table(data_collection$kzmz_flag)










####### Generate test and train data ########
# fix random generator
set.seed(2020)
n_train <- round(0.08 * nrow)
index_train <- sample(1:nrow, n_train)

DTrain <- data[index_train, ]
DTest <- data[-index_train, ]

# summary to find if data have NAs
summary(DTrain)
summary(DTest)
# Detailed summary of data

Dtrainskim <- skim(DTrain)
Dtestskrim <- skim(DTest)
## see all NAs for all dataset
skim(DTrain)
skim(DTest)

## create a new data set without NAs
DTrain_new <- na.omit(DTrain)
DTest_new <- na.omit(DTest)
# number of column and row and summary in data train w-o NAs
dim(DTrain_new) # number of columns and rows for clean data Train
summary(DTrain_new)

# number of column and row and summary in data test w-o NAs
dim(DTest_new) # number of columns and rows for clean data Test
summary(DTest_new)


## summary for each attribute
headofTable <- c(
  "Num. of Children", "Num. Other Product", "Year of Birth",
  "Due amount", "payed amount", "delay"
)
EX <- c(
  mean(DTrain_new$number_of_children),
  mean(DTrain_new$number_other_product), mean(DTrain_new$birth_year),
  mean(DTrain_new$due_amount), mean(DTrain_new$payed_amount),
  mean(DTrain_new$delay)
)
VarX <- c(
  var(DTrain_new$number_of_children),
  var(DTrain_new$number_other_product), var(DTrain_new$birth_year),
  var(DTrain_new$due_amount), var(DTrain_new$payed_amount),
  var(DTrain_new$delay)
)
Median <- c(
  median(DTrain_new$number_of_children),
  median(DTrain_new$number_other_product),
  median(DTrain_new$birth_year), median(DTrain_new$due_amount),
  median(DTrain_new$payed_amount), median(DTrain_new$delay)
)
Q1 <- c(
  quantile(DTrain_new$number_of_children, probs = 1 / 4),
  quantile(DTrain_new$number_other_product, probs = 1 / 4),
  quantile(DTrain_new$birth_year, probs = 1 / 4),
  quantile(DTrain_new$due_amount, probs = 1 / 4),
  quantile(DTrain_new$payed_amount, probs = 1 / 4),
  quantile(DTrain_new$delay, probs = 1 / 4)
)
Q3 <- c(
  quantile(DTrain_new$number_of_children, probs = c(3 / 4)),
  quantile(DTrain_new$number_other_product, probs = c(3 / 4)),
  quantile(DTrain_new$birth_year, probs = c(3 / 4)),
  quantile(DTrain_new$due_amount, probs = c(3 / 4)),
  quantile(DTrain_new$payed_amount, probs = c(3 / 4)),
  quantile(DTrain_new$delay, probs = 3 / 4)
)
Min <- c(
  min(DTrain_new$number_of_children),
  min(DTrain_new$number_other_product),
  min(DTrain_new$birth_year), min(DTrain_new$due_amount),
  min(DTrain_new$payed_amount), min(DTrain_new$delay)
)
Max <- c(
  max(DTrain_new$number_of_children),
  max(DTrain_new$number_other_product), max(DTrain_new$birth_year),
  max(DTrain_new$due_amount), max(DTrain_new$payed_amount),
  max(DTrain_new$delay)
)

summaryDTrain <- distinct(data.frame(headofTable, EX, VarX, Median, Q1, Q3, Min,
  Max,
  check.rows = FALSE, check.names = FALSE
))

############## exploring Data ########################

#### Train data statistic ####
# statistic addiction delay on gender
meanG_D <- DTrain_new %>%
  group_by(gender) %>%
  summarise(mean = mean(delay))

medG_D <- DTrain_new %>%
  group_by(gender) %>%
  summarise(med = median(delay))

maxG_D <- DTrain_new %>%
  group_by(gender) %>%
  summarise(max = max(delay))

minG_D <- DTrain_new %>%
  group_by(gender) %>%
  summarise(min = min(delay))

Q1G_D <- DTrain_new %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(delay, probs = 1 / 4))

Q3G_D <- DTrain_new %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(delay, probs = 3 / 4))

data_GD <- data.frame(meanG_D, medG_D[, 2], minG_D[, 2], maxG_D[, 2],
  Q1G_D[, 2], Q3G_D[, 2],
  check.names = FALSE
)

# statistic addiction payed amount to gender
meanG_PA <- DTrain_new %>%
  group_by(gender) %>%
  summarise(mean = mean(payed_amount))

medG_PA <- DTrain_new %>%
  group_by(gender) %>%
  summarise(med = median(payed_amount))

maxG_PA <- DTrain_new %>%
  group_by(gender) %>%
  summarise(max = max(payed_amount))

minG_PA <- DTrain_new %>%
  group_by(gender) %>%
  summarise(min = min(payed_amount))

Q1G_PA <- DTrain_new %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(payed_amount, probs = 1 / 4))

Q3G_PA <- DTrain_new %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(payed_amount, probs = 3 / 4))

data_GPA <- data.frame(meanG_PA, medG_PA[, 2], minG_PA[, 2], maxG_PA[, 2],
  Q1G_PA[, 2], Q3G_PA[, 2],
  check.names = FALSE
)
##### test data statictic #####
## statistic addiction delay on gender
TmeanG_D <- DTest_new %>%
  group_by(gender) %>%
  summarise(mean = mean(delay))

TmedG_D <- DTest_new %>%
  group_by(gender) %>%
  summarise(med = median(delay))

TmaxG_D <- DTest_new %>%
  group_by(gender) %>%
  summarise(max = max(delay))

TminG_D <- DTest_new %>%
  group_by(gender) %>%
  summarise(min = min(delay))

TQ1G_D <- DTest_new %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(delay, probs = 1 / 4))

TQ3G_D <- DTest_new %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(delay, probs = 3 / 4))

Tdata_GD <- data.frame(TmeanG_D, TmedG_D[, 2], TminG_D[, 2], TmaxG_D[, 2],
  TQ1G_D[, 2], TQ3G_D[, 2],
  check.names = FALSE
)

# statistic addiction payed amount to gender
TmeanG_PA <- DTest_new %>%
  group_by(gender) %>%
  summarise(mean = mean(payed_amount))

TmedG_PA <- DTest_new %>%
  group_by(gender) %>%
  summarise(med = median(payed_amount))

TmaxG_PA <- DTest_new %>%
  group_by(gender) %>%
  summarise(max = max(payed_amount))

TminG_PA <- DTest_new %>%
  group_by(gender) %>%
  summarise(min = min(payed_amount))

TQ1G_PA <- DTest_new %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(payed_amount, probs = 1 / 4))

TQ3G_PA <- DTest_new %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(payed_amount, probs = 3 / 4))

Tdata_GPA <- data.frame(TmeanG_PA, TmedG_PA[, 2], TminG_PA[, 2], TmaxG_PA[, 2],
  TQ1G_PA[, 2], TQ3G_PA[, 2],
  check.names = FALSE
)

# addiction payed amount to gender, product type and business discount
DTest_new %>%
  group_by(gender, product_type, business_discount) %>%
  summarise(payedAmount = mean(payed_amount)) %>%
  spread(gender, payedAmount)

DTest_new %>%
  group_by(gender, number_of_children) %>%
  summarise(delay = mean(delay)) %>%
  spread(gender, delay)

# Data preparation -------------------------------------------------------------
# Clean the data - estimation of missing data

# Create derived attributes
# Add a column for delay in days
data_collection$delay <- difftime(data_collection$payment_date,
  data_collection$due_date, tz,
  units = "days"
)
data_collection <- data_collection %>%
  mutate(delay = as.numeric(delay))
