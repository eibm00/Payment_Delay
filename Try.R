install.packages("tidyverse")
install.packages("naniar")
library(tidyverse)
library(naniar)


# Put data files outside of the git folder in order to avoid pushing too large files to repository
path_to_data <- '../data/payment_dates_final.csv'
data_collection <- read.csv(path_to_data)

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


