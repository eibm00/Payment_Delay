
### Dependencies ---------------------------------------------------------------
rm(list = ls()) # clear

if (!require(tidyverse)) install.packages("tidyverse")
if (!require(naniar)) install.packages("naniar")
if (!require(styler)) install.packages("styler")
if (!require(GGally)) install.packages("GGally")
if (!require(skimr)) install.packages("skimr")

library(tidyverse)
library(naniar)
library(styler)
library(GGally)
library(skimr)

### Load the initial data ------------------------------------------------------

# Put data files outside of the git folder in order to avoid pushing too large
# files to repository
# path_to_data <- 'D:/..../payment_dates_final.csv'
path_to_data <- "D:/01 Skola VSE Statistika/DataX/zaverecny projekt/payment_dates_final.csv"
#path_to_data <- "..\\payment_dates_final.csv"

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
  mutate(contract_status = as.factor(contract_status))
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
# Problem! cf_val appears to not be a factor, despite the description file !!!
data_collection <- data_collection %>%
  mutate(cf_val = as.numeric(cf_val))
data_collection <- data_collection %>%
  mutate(kzmz_flag = as.factor(kzmz_flag))
data_collection <- data_collection %>%
  mutate(due_amount = as.numeric(due_amount))
data_collection <- data_collection %>%
  mutate(payed_amount = as.numeric(payed_ammount))

# Remove feature "payed_ammount" which was replaced by feature "payed_amount"
data_collection <- subset(data_collection, select = -payed_ammount)

# Create a feature for delay in days
data_collection$delay <- difftime(data_collection$payment_date,
  data_collection$due_date, tz,
  units = "days"
)
data_collection <- data_collection %>%
  mutate(delay = as.numeric(delay))

# Display the internal structure of the data
str(data_collection)

# Analyze correlations among numeric features
corr_pairs_name <- matrix(nrow = choose(ncol(data_collection), 2), ncol = 3)
numeric_rel <- matrix(nrow = choose(ncol(data_collection), 2), ncol = 2)
corr_vector <- vector("integer")
iteration <- 0
for (i in c(9:(ncol(data_collection) - 1))) {
  if (is.numeric(data_collection[, i])) {
    for (j in c((i + 1):ncol(data_collection))) {
      iteration <- iteration + 1
      if (is.numeric(data_collection[, j])) {
        correlation <- cor.test(
          x = data_collection[, i],
          y = data_collection[, j]
        )
        if (correlation$p.value <= 0.05) {
          numeric_rel[iteration, ] <- c(i, j)
          corr_pairs_name[iteration, 1] <- names(data_collection)[i]
          corr_pairs_name[iteration, 2] <- names(data_collection)[j]
          corr_pairs_name[iteration, 3] <- round(correlation$estimate, digits = 4)
          corr_vector <- c(corr_vector, i, j)
        }
      }
    }
  }
}

# Save the pairs of numeric features that are correlated into a data frame
corr_pairs_name <- as.data.frame(corr_pairs_name)
corr_pairs_name <- corr_pairs_name %>%
  filter_all(any_vars(!is.na(.)))
numeric_rel <- as.data.frame(numeric_rel)
numeric_rel <- numeric_rel %>%
  filter_all(any_vars(!is.na(.)))

# Create correlation plots and export them into PNG files
corr_vector <- unique(corr_vector)
par(mfrow = c(length(corr_vector), length(corr_vector)))
for (i in 1:nrow(numeric_rel)) {
  x <- data_collection[, numeric_rel[i, 1]]
  y <- data_collection[, numeric_rel[i, 2]]
  g <- ggplot(data_collection, aes(x, y)) +
    geom_point(size = 1) +
    xlab(names(data_collection)[numeric_rel[i, 1]]) +
    ylab(names(data_collection)[numeric_rel[i, 2]])
  ggsave(filename = paste0("correlation_", i, ".png"), g, width = 14, height = 10, units = "cm")
}

# Examine relationship between categorical features using chi-squared test with
#   the significance level 0.05
# Overestimating the matrix size saves time compared to building the matrix one
#  row at a time:
categorical_rel <- matrix(nrow = choose(ncol(data_collection), 2), ncol = 2)
cont_vector <- vector("integer")
iteration <- 0
for (i in c(1:(ncol(data_collection) - 1))) {
  if (is.factor(data_collection[, i])) {
    for (j in c((i + 1):ncol(data_collection))) {
      iteration <- iteration + 1
      if (is.factor(data_collection[, j])) {
        contingency_table <- table(data_collection[, i], data_collection[, j])
        chisq <- (chisq.test(contingency_table, correct = FALSE))
        if (chisq$p.value <= 0.05) {
          categorical_rel[iteration, ] <- c(i, j)
          cont_vector <- c(cont_vector, i, j)
        }
      }
    }
  }
}

# Save the pairs of categorical features that are correlated into a data frame
categorical_rel <- as.data.frame(categorical_rel)
categorical_rel <- categorical_rel %>%
  filter_all(any_vars(!is.na(.)))

cont_vector <- unique(cont_vector)

# Suggestions
data_collection %>%
  group_by(product_type) %>%
  summarize(
    n_records = n(), n_contracts = n_distinct(contract_id),
    mean_payed_amount = mean(payed_amount), min_birth_year = min(birth_year)
  )

ggplot(data_collection, aes(x = product_type, y = payed_amount)) +
  geom_boxplot() +
  theme_minimal() +
  labs(
    title = "Boxplot of paid amount by product type",
    x = "Product type", y = "Paid Amount"
  )

ggplot(data_collection, aes(x = payed_amount)) +
  geom_histogram(fill = "limegreen", alpha = 0.5) +
  theme_minimal() +
  labs(
    title = "Histogram of paid amount",
    x = "Paid amount", y = "Count"
  )


# Data exploration--------------------------------------------------------------

# Analyze properties of interesting attributes in detail include graphs and 
#   plots

# Summary statistics of the data
# Check attribute value ranges, coverage, NAs occurence
summary <- summary(data_collection)
print(summary)
detailed_statistics <- skim(data_collection)
print(detailed_statistics)

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

sum(is.na(data_collection$different_contact_area) == T &
      is.na(data_collection$cf_val) == T &
      is.na(data_collection$living_area) == T & 
      is.na(data_collection$kc_flag) == T)

sum(is.na(data_collection$kc_flag) == T)

# Zaver: ze ctyr promennych s nejvice NA zaznamy je vetsina NA pozorovani
# na stejnych radcich, to muze indikovat "missing not at random".


# Check for plausibility of values
# Check for plausibility of values
for (i in c(1:ncol)) {
  if (is.factor(data_collection[, i])) {
    print(colnames(data_collection[i]))
    print(prop.table(table(data_collection[, i])))
    cat(sep = "\n\n")
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

DTrain <- data_collection[index_train, ]
DTest <- data_collection[-index_train, ]

# Summary to find if data have NAs
summary(DTrain)
summary(DTest)

# Detailed summary of data
install.packages("skimr")
library(skimr)

Dtrainskim <- skim(DTrain)
Dtestskrim <- skim(DTest)

## See all NAs for all dataset
skim(DTrain)
skim(DTest)

## Create a new data set without NAs
DTrain_new <- na.omit(DTrain)
DTest_new <- na.omit(DTest)



# Number of column and row and summary in data train w-o NAs
dim(DTrain_new) # number of columns and rows for clean data Train
summary(DTrain_new)

# Number of column and row and summary in data test w-o NAs
dim(DTest_new) # number of columns and rows for clean data Test
summary(DTest_new)

# delete NAs in whole data source
data_collection <- na.omit(data_collection)
## Summary for each attribute
headofTable <- c(
  "Num. of Children", "Num. Other Product", "Year of Birth",
  "Due amount", "payed amount", "delay"
)
EX <- c(
  mean(data_collection$number_of_children),
  mean(data_collection$number_other_product), mean(data_collection$birth_year),
  mean(data_collection$due_amount), mean(data_collection$payed_amount),
  mean(data_collection$delay)
)
VarX <- c(
  var(data_collection$number_of_children),
  var(data_collection$number_other_product), var(data_collection$birth_year),
  var(data_collection$due_amount), var(data_collection$payed_amount),
  var(data_collection$delay)
)
Median <- c(
  median(data_collection$number_of_children),
  median(data_collection$number_other_product),
  median(data_collection$birth_year), median(data_collection$due_amount),
  median(data_collection$payed_amount), median(data_collection$delay)
)
Q1 <- c(
  quantile(data_collection$number_of_children, probs = 1 / 4),
  quantile(data_collection$number_other_product, probs = 1 / 4),
  quantile(data_collection$birth_year, probs = 1 / 4),
  quantile(data_collection$due_amount, probs = 1 / 4),
  quantile(data_collection$payed_amount, probs = 1 / 4),
  quantile(data_collection$delay, probs = 1 / 4)
)
Q3 <- c(
  quantile(data_collection$number_of_children, probs = c(3 / 4)),
  quantile(data_collection$number_other_product, probs = c(3 / 4)),
  quantile(data_collection$birth_year, probs = c(3 / 4)),
  quantile(data_collection$due_amount, probs = c(3 / 4)),
  quantile(data_collection$payed_amount, probs = c(3 / 4)),
  quantile(data_collection$delay, probs = 3 / 4)
)
Min <- c(
  min(data_collection$number_of_children),
  min(data_collection$number_other_product),
  min(data_collection$birth_year), min(data_collection$due_amount),
  min(data_collection$payed_amount), min(data_collection$delay)
)
Max <- c(
  max(data_collection$number_of_children),
  max(data_collection$number_other_product), max(data_collection$birth_year),
  max(data_collection$due_amount), max(data_collection$payed_amount),
  max(data_collection$delay)
)

summaryData <- distinct(data.frame(headofTable, EX, VarX, Median, Q1, Q3, Min,
  Max,
  check.rows = FALSE, check.names = FALSE
))

############## exploring Data ########################

#### Data statistic ####
# Statistic addiction delay on gender
meanG_D <- data_collection %>%
  group_by(gender) %>%
  summarise(mean = mean(delay))

medG_D <- data_collection %>%
  group_by(gender) %>%
  summarise(med = median(delay))

maxG_D <- data_collection %>%
  group_by(gender) %>%
  summarise(max = max(delay))

minG_D <- data_collection %>%
  group_by(gender) %>%
  summarise(min = min(delay))

Q1G_D <- data_collection %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(delay, probs = 1 / 4))

Q3G_D <- data_collection %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(delay, probs = 3 / 4))

data_GD <- data.frame(meanG_D, medG_D[, 2], minG_D[, 2], maxG_D[, 2],
  Q1G_D[, 2], Q3G_D[, 2],
  check.names = FALSE
)

# Statistic addiction payed amount to gender
meanG_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(mean = mean(payed_amount))

medG_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(med = median(payed_amount))

maxG_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(max = max(payed_amount))

minG_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(min = min(payed_amount))

Q1G_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(payed_amount, probs = 1 / 4))

Q3G_PA <- data_collection %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(payed_amount, probs = 3 / 4))

data_GPA <- data.frame(meanG_PA, medG_PA[, 2], minG_PA[, 2], maxG_PA[, 2],
  Q1G_PA[, 2], Q3G_PA[, 2],
  check.names = FALSE
)

# Statistic addiction due amount to gender
meanG_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(mean = mean(due_amount))

medG_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(med = median(due_amount))

maxG_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(max = max(due_amount))

minG_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(min = min(due_amount))

Q1G_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(Q1 = quantile(due_amount, probs = 1 / 4))

Q3G_DA <- data_collection %>%
  group_by(gender) %>%
  summarise(Q3 = quantile(due_amount, probs = 3 / 4))

data_GDA <- data.frame(meanG_DA, medG_DA[, 2], minG_DA[, 2], maxG_DA[, 2],
                       Q1G_DA[, 2], Q3G_DA[, 2],
                       check.names = FALSE
)
# Addiction payed amount to gender, product type and business discount
data_collection %>%
  group_by(gender, product_type, business_discount) %>%
  summarise(payedAmount = mean(payed_amount)) %>%
  spread(gender, payedAmount)

data_collection %>%
  group_by(gender, number_of_children) %>%
  summarise(delay = mean(delay)) %>%
  spread(gender, delay)


#### FEATURE ENGINEERING ------------------------------------------------------ 

# This function takes a vector and shifts its values by 1. This means that 
# on the second position is the first value, on the third position is 
# the second value etc. This is necessary for feature engineering. 
# We want a cumulative sum/cumulative mean for previous payments, but 
# functions cummean/cumsum applied to a row[i] make the calculations
# using also the number on this line. This is where this function comes in handy.
f_vec_shift <- function(x){
  new_vec <- c(0, x[1:length(x)-1])
  return(new_vec)
}

# Creating new features: was the delay larger than 21(140) days? 
data_collection <- data_collection %>%
  mutate(delay_21_y = ifelse(delay > 21, 1, 0)) %>%
  mutate(delay_140_y = ifelse(delay > 140, 1, 0))

# Creating new features: mean delay for the whole client's history
data_collection <- data_collection %>%
  nest(-contract_id) %>%
      mutate(delay_indiv_help = map(.x = data, .f = ~cummean(.x$delay))) %>%
      mutate(delay_indiv = map(.x = delay_indiv_help, .f = ~f_vec_shift(.x))) %>%
    select(-delay_indiv_help) %>%
  unnest(c(data, delay_indiv))

# Creating new features: number of delays larger than 21(140) days
# for the whole client's history. 
data_collection <- data_collection %>%
  nest(-contract_id) %>%
    mutate(delay_indiv_21_help = map(.x = data, .f = ~cumsum(.x$delay_21_y))) %>%
      mutate(delay_indiv_21 = map(.x = delay_indiv_21_help, .f = ~f_vec_shift(.x))) %>%
    mutate(delay_indiv_140_help = map(.x = data, .f = ~cumsum(.x$delay_140_y))) %>%
      mutate(delay_indiv_140 = map(.x = delay_indiv_140_help, .f = ~f_vec_shift(.x))) %>%
    select(-delay_indiv_21_help, -delay_indiv_140_help) %>%
  unnest(c(data, delay_indiv_21, delay_indiv_140))

# This part of the feature engineering process is the longest, but the syntax
# is not that complicated (compared to map/nest etc.).
# It works like this: 
# First, we create new features (variables, columns) for storing 
# the average payment delay for last 12/6/3/1 month.
# In this section, I am also creating a set of new features with suffix _help, 
# they serve only as a temporary helper. 

# It was necessasry rep() NA values, since 0 or any other number
# would be misleading. 
data_collection <- data_collection %>%
  mutate(mean_delay_12m = rep(NA, nrow(data_collection))) %>% 
  mutate(mean_delay_12m_help = rep(NA, nrow(data_collection))) %>%
  mutate(mean_delay_6m = rep(NA, nrow(data_collection))) %>% 
  mutate(mean_delay_6m_help = rep(NA, nrow(data_collection))) %>%
  mutate(mean_delay_3m = rep(NA, nrow(data_collection))) %>% 
  mutate(mean_delay_3m_help = rep(NA, nrow(data_collection))) %>%
  mutate(mean_delay_1m = rep(NA, nrow(data_collection))) %>% 
  mutate(mean_delay_1m_help = rep(NA, nrow(data_collection))) %>%
  filter(is.na(payment_order) == F) %>% # POZOR, OVERIT!!!!!!!! 
  filter(is.na(delay) == F) # POZOR, OVERIT!!!!!!!!

# Second we nest data again
data_collection <- data_collection %>%
  nest(-contract_id) 

# And third, we loop through the data. A lot. 
# The first loop loops through the nested data for each contract. 
# The purpose of this main loop is similar to map() function. 
# Maybe the inner loop could be written as a function and passed to
# map() in .f argument, but not sure how big the runtime gains would be. 
for(i in 1:nrow(data_collection)){
  df_actual <- data_collection$data[[i]]
  
  # The second loop loops through the dataframe for each contract. 
  # There are 4 if conditions in this for loop -> 12/6/3/1 M delay.
  # The logic is this: 
  # The algorithm identifies rows with due_date in the given date range, which
  # means last 12/6/3/1 months. These rows are marked with 1 in the _help column. 
  # Then, the average is calculated simply by multiplying delay column by 
  # _help column and divided by the sum of _help. This works because the 
  # _help column has only zeroes and ones. 
  # Lastly, the _help columned is restarted for the next round. 
  for(j in 1:nrow(df_actual)){
    
    # Mean for the last 12 months
    if(j > 12){
    # Mark relevant rows with 1
    df_actual <- df_actual %>%
      mutate(mean_delay_12m_help = ifelse(due_date >= df_actual$due_date[j] - months(12) &
                                            due_date < df_actual$due_date[j], 1, 0))
    
    # Calculate mean
    df_actual$mean_delay_12m[j] <-sum(df_actual$mean_delay_12m_help*df_actual$delay)/sum(df_actual$mean_delay_12m_help) 
    # Restart helper column
    df_actual$mean_delay_12m_help = rep(NA, nrow(df_actual))
    }
    
    # Mean for the last 6 months
    if(j > 6){
      # Mark relevant rows with 1 
      df_actual <- df_actual %>%
        mutate(mean_delay_6m_help = ifelse(due_date >= df_actual$due_date[j] - months(6) &
                                              due_date < df_actual$due_date[j], 1, 0))
      
      # Calculate mean
      df_actual$mean_delay_6m[j] <-sum(df_actual$mean_delay_6m_help*df_actual$delay)/sum(df_actual$mean_delay_6m_help) 
      # Restart helper column
      df_actual$mean_delay_6m_help = rep(NA, nrow(df_actual))
    }
    
    # Mean for the last 3 months
    if(j > 3){
      # Mark relevant rows with 1 
      df_actual <- df_actual %>%
        mutate(mean_delay_3m_help = ifelse(due_date >= df_actual$due_date[j] - months(3) &
                                             due_date < df_actual$due_date[j], 1, 0))
      
      # Calculate mean
      df_actual$mean_delay_3m[j] <-sum(df_actual$mean_delay_3m_help*df_actual$delay)/sum(df_actual$mean_delay_3m_help) 
      # Restart helper column
      df_actual$mean_delay_3m_help = rep(NA, nrow(df_actual))
    }
    
    # Mean for the last 1 month
    if(j > 1){
      # Mark relevant rows with 1
      df_actual <- df_actual %>%
        mutate(mean_delay_1m_help = ifelse(due_date >= df_actual$due_date[j] - months(1) &
                                             due_date < df_actual$due_date[j], 1, 0))
      
      # Calculate mean
      df_actual$mean_delay_1m[j] <-sum(df_actual$mean_delay_1m_help*df_actual$delay)/sum(df_actual$mean_delay_1m_help) 
      # Restart helper column
      df_actual$mean_delay_1m_help = rep(NA, nrow(df_actual))
    }
    
  }
  data_collection$data[[i]] <- df_actual 
  
  # Progress bar might be more elegant, someone can add later 
  # There is 86 980 observations to loop through, so printing [i] gives a good idea of progress. 
  print(i)
}

# Unnest the data
# Remove helper columns
data_collection <- data_collection %>%
  unnest(-data) %>%
    select(-mean_delay_12m_help, -mean_delay_6m_help, -mean_delay_3m_help, -mean_delay_1m_help)


# Write data to .txt for the model creation
write.table(data_collection, file = "data_collection_prepared.txt", sep = ";")









# Data preparation -------------------------------------------------------------
# Clean the data - estimation of missing data

# Create derived attributes
