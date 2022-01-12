#packages
library(data.table)
library(lubridate)
library(plyr)
library(dplyr)

memory.limit(20000)

####read data####
train_data <- fread('../datas/train.csv')
test_data = fread('../datas/test.csv')
historical_data = fread('../datas/historical_transactions.csv')
newmerchant_data = fread('../datas/new_merchant_transactions.csv')
merchants_data = fread('../datas/merchants.csv')

##backup
bcakup_train <- train_data
backup_test <- test_data
backup_hist <- historical_data
backup_newm <- newmerchant_data
backup_merch <- merchants_data

####UTILITIES####
clean_special_string <- function(df){
  df[is.null(df)] <- NA
  return(df)
}
check_missing_values <- function(df){
  print(colnames(df)[colSums(is.na(df)) > 0])
}
get_mode <- function(feature){
  uniqv <- unique(feature)
  return(uniqv[which.max(tabulate(match(feature, uniqv)))])
}

#cleaning
train_data <- clean_special_string(train_data)
test_data <- clean_special_string(test_data)
historical_data <- clean_special_string(historical_data)
newmerchant_data <- clean_special_string(newmerchant_data)
merchants_data <- clean_special_string(merchants_data)

#check missing values
check_missing_values(train_data)
check_missing_values(test_data)
check_missing_values(historical_data)
check_missing_values(newmerchant_data)
check_missing_values(merchants_data)

#impute missing values
newmerchant_data$category_2[is.na(newmerchant_data$category_2)] <- get_mode(historical_data$category_2)
historical_data$category_2[is.na(historical_data$category_2)] <- get_mode(historical_data$category_2)
newmerchant_data$category_3[is.na(newmerchant_data$category_3)] <- get_mode(historical_data$category_3)
historical_data$category_3[is.na(historical_data$category_3)] <- get_mode(historical_data$category_3)
gc()
newmerchant_data$merchant_id[is.na(newmerchant_data$merchant_id)] <- get_mode(historical_data$merchant_id)
historical_data$merchant_id[is.na(historical_data$merchant_id)] <- get_mode(historical_data$merchant_id)

#check distribution and outlier
#hist(historical_data$purchase_amount)
quantile(historical_data$purchase_amount)
#hist(newmerchant_data$purchase_amount)
quantile(newmerchant_data$purchase_amount)
historical_data$purchase_amount[historical_data$purchase_amount > 200] <- median(historical_data$purchase_amount)
newmerchant_data$purchase_amount[newmerchant_data$purchase_amount > 200] <- median(newmerchant_data$purchase_amount)

# C_ID_c27b4f80f7
first_transaction <- historical_data[historical_data$card_id == 'C_ID_c27b4f80f7']$purchase_date[1]
test_data[is.na(test_data$card_id)] <-substring(first_transaction,1,7)
test_data$first_active_month[is.na(test_data$first_active_month)] <- "2017-03"
gc()#



####Date and Mapping Processing####
historical_data$purchase_date_year <- year(historical_data$purchase_date)
historical_data$purchase_date_weekofyear <- week(historical_data$purchase_date)
historical_data$purchase_date_month <- month(historical_data$purchase_date)
historical_data$purchase_date_dayofweek <- wday(historical_data$purchase_date)
historical_data$purchase_date_weekend <- 0
historical_data$purchase_date_weekend[historical_data$purchase_date_dayofweek>5] <- 1
historical_data$purchase_date_hour <- hour(historical_data$purchase_date)

newmerchant_data$purchase_date_year <- year(newmerchant_data$purchase_date)
newmerchant_data$purchase_date_weekofyear <- week(newmerchant_data$purchase_date)
newmerchant_data$purchase_date_month <- month(newmerchant_data$purchase_date)
newmerchant_data$purchase_date_dayofweek <- wday(newmerchant_data$purchase_date)
newmerchant_data$purchase_date_weekend <- 0
newmerchant_data$purchase_date_weekend[newmerchant_data$purchase_date_dayofweek>5] <- 1
newmerchant_data$purchase_date_hour <- hour(newmerchant_data$purchase_date)
gc()
head(historical_data)
head(newmerchant_data)

#
historical_data$authorized_flag <- mapvalues(historical_data$authorized_flag, c('Y','N'), c(1,0), warn_missing=FALSE)
historical_data$category_1 <- mapvalues(historical_data$category_1, c('Y','N'), c(1,0), warn_missing=FALSE)
historical_data$category_3 <- mapvalues(historical_data$category_3, c('A','B','C'), c(2,1,0), warn_missing=FALSE)
historical_data$installments <- mapvalues(historical_data$installments, c(-1,999), c(NA,NA), warn_missing=FALSE)
historical_data$authorized_flag <- as.integer(historical_data$authorized_flag)
historical_data$category_1 <- as.integer(historical_data$category_1)
historical_data$category_3 <- as.integer(historical_data$category_3)
gc()

newmerchant_data$authorized_flag <- mapvalues(newmerchant_data$authorized_flag, c('Y','N'), c(1,0), warn_missing=FALSE)
newmerchant_data$category_1 <- mapvalues(newmerchant_data$category_1, c('Y','N'), c(1,0), warn_missing=FALSE)
newmerchant_data$category_3 <- mapvalues(newmerchant_data$category_3, c('A','B','C'), c(2,1,0), warn_missing=FALSE)
newmerchant_data$installments <- mapvalues(newmerchant_data$installments, c(-1,999), c(NA,NA), warn_missing=FALSE)
newmerchant_data$authorized_flag <- as.integer(newmerchant_data$authorized_flag)
newmerchant_data$category_1 <- as.integer(newmerchant_data$category_1)
newmerchant_data$category_3 <- as.integer(newmerchant_data$category_3)
gc()

check_missing_values(historical_data)
check_missing_values(newmerchant_data)

historical_data$installments[is.na(historical_data$installments)] <- 0
newmerchant_data$installments[is.na(newmerchant_data$installments)] <- 0
historical_data$category_3[is.na(historical_data$category_3)] <- 0
newmerchant_data$category_3[is.na(newmerchant_data$category_3)] <- 0
gc()

####Part1 Grouping####

#groupby amount features
authorized_df <- historical_data[historical_data$authorized_flag == 1]
authorized_amount  <- authorized_df %>%
  group_by(card_id) %>%
  summarize(card_sum_authorized_flag_1_purchase_amount = sum(purchase_amount, na.rm = TRUE))

nonauthorized_df <- historical_data[historical_data$authorized_flag == 0]
nonauthorized_amount <-  nonauthorized_df %>%
  group_by(card_id) %>%
  summarize(card_sum_authorized_flag_0_purchase_amount = sum(purchase_amount, na.rm = TRUE))

newauthorized_df <- newmerchant_data[newmerchant_data$authorized_flag == 1]
newauthorized_amount <-  newauthorized_df %>%
  group_by(card_id) %>%
  summarize(card_sum_authorized_flag_1_purchase_amount = sum(purchase_amount, na.rm = TRUE))

check_missing_values(authorized_df)
check_missing_values(nonauthorized_df)
check_missing_values(newauthorized_df)

rm(authorized_df)
rm(nonauthorized_df)
rm(newauthorized_df)

historical_data <- merge(x=historical_data, y=authorized_amount, by='card_id', all.x = TRUE)
rm(authorized_amount)

historical_data <- merge(x=historical_data, y=nonauthorized_amount, by='card_id', all.x = TRUE)
rm(nonauthorized_amount)

newmerchant_data <- merge(x=newmerchant_data, y=newauthorized_amount, by='card_id', all.x = TRUE)
rm(newauthorized_amount)
gc()

#groupby1: date features 
merge1_date  <-  historical_data %>%
  group_by(card_id) %>%
  summarize(hist_purchase_date_month_nunique = n_distinct(purchase_date_month, na.rm = TRUE),
            hist_purchase_date_hour_nunique = n_distinct(purchase_date_hour, na.rm = TRUE),
            hist_purchase_date_weekofyear_nunique = n_distinct(purchase_date_weekofyear, na.rm = TRUE),
            hist_purchase_date_weekend_mean = mean(purchase_date_weekofyear, na.rm = TRUE), ###
            hist_purchase_date_min = min(purchase_date, na.rm = TRUE),
            hist_purchase_date_max = max(purchase_date, na.rm = TRUE),
            hist_month_lag_std = sd(month_lag, na.rm = TRUE))
merge1_date$hist_purchase_date_diff <- merge1_date$hist_purchase_date_max - merge1_date$hist_purchase_date_min
merge1_date$hist_purchase_date_uptonow <- as.integer(now() - merge1_date$hist_purchase_date_max)

#groupby2: amount features
#special processing from other competents
historical_data$hist_purchase_amount_new <- round(historical_data$purchase_amount / 0.00150265118 + 497.06,2)
merge2_amount  <-  historical_data %>%
  group_by(card_id) %>%
  summarize(hist_purchase_amount_std = sd(purchase_amount, na.rm = TRUE),
            hist_purchase_amount_sum = sum(purchase_amount, na.rm = TRUE),
            hist_purchase_amount_mean = mean(purchase_amount, na.rm = TRUE),
            hist_card_sum_authorized_flag_0_purchase_amount_mean = mean(card_sum_authorized_flag_0_purchase_amount, na.rm = TRUE),
            hist_card_sum_authorized_flag_1_purchase_amount_mean = mean(card_sum_authorized_flag_1_purchase_amount, na.rm = TRUE), ###
            hist_card_mean_purchase_amount_new = mean(hist_purchase_amount_new, na.rm = TRUE),
            hist_card_std_purchase_amount_new = sd(hist_purchase_amount_new, na.rm = TRUE),
            hist_card_sum_purchase_amount_new = sum(hist_purchase_amount_new, na.rm = TRUE))

#groupby3: categorical features
merge3_cat <- historical_data %>%
  group_by(card_id) %>%
  summarize(hist_subsector_id_nunique = n_distinct(subsector_id, na.rm = TRUE),
            hist_merchant_id_nunique = n_distinct(merchant_id, na.rm = TRUE),
            hist_merchant_category_id_nunique = n_distinct(merchant_category_id, na.rm = TRUE),
            hist_installments_std = sd(installments, na.rm = TRUE),
            hist_installments_sum = sum(installments, na.rm = TRUE),
            hist_authorized_flag_mean = mean(authorized_flag, na.rm = TRUE),
            hist_card_id_size = n_distinct(card_id, na.rm = TRUE),
            hist_category_1_mean = mean(category_1, na.rm = TRUE),
            hist_category_2_mean = mean(category_2, na.rm = TRUE),
            hist_category_3_mean = mean(category_3, na.rm = TRUE))
gc()

check_missing_values(merge1_date)
check_missing_values(merge2_amount)
check_missing_values(merge3_cat)

#Part1 Aggregate
train_data <- merge(x=train_data, y=merge1_date, by='card_id', all.x = TRUE)
train_data <- merge(x=train_data, y=merge2_amount, by='card_id', all.x = TRUE)
train_data <- merge(x=train_data, y=merge3_cat, by='card_id', all.x = TRUE)
gc()
test_data <- merge(x=test_data, y=merge1_date, by='card_id', all.x = TRUE)
test_data <- merge(x=test_data, y=merge2_amount, by='card_id', all.x = TRUE)
test_data <- merge(x=test_data, y=merge3_cat, by='card_id', all.x = TRUE)
#train_data[is.na(train_data)] <- 0
#test_data[is.na(test_data)] <- 0
rm(merge1_date)
rm(merge2_amount)
rm(merge3_cat)
gc()

check_missing_values(train_data)
check_missing_values(test_data)

##backup
bcakup_train <- train_data
backup_test <- test_data
backup_hist <- historical_data
backup_newm <- newmerchant_data
backup_merch <- merchants_data

#train_data <- backup_train
#test_data <- backup_test
#newmerchant_data <- backup_newm
#merchants_data <- backup_merch

####Part2 code(code after Release memory)####

#merchants_data features
#Fill missing values with respective mode values
merchants_data$avg_sales_lag3[is.na(merchants_data$avg_sales_lag3)] <- get_mode(merchants_data$avg_sales_lag3)
merchants_data$avg_sales_lag6[is.na(merchants_data$avg_sales_lag6)] <- get_mode(merchants_data$avg_sales_lag6)
merchants_data$avg_sales_lag12[is.na(merchants_data$avg_sales_lag12)] <- get_mode(merchants_data$avg_sales_lag12)
merchants_data$category_2[is.na(merchants_data$category_2)] <- get_mode(merchants_data$category_2)

check_missing_values(merchants_data)

#Mapping
merchants_data$most_recent_sales_range <- mapvalues(merchants_data$most_recent_sales_range, c('A','B','C','D','E'),c(5,4,3,2,1),warn_missing = FALSE)
merchants_data$most_recent_purchases_range <- mapvalues(merchants_data$most_recent_purchases_range, c('A','B','C','D','E'),c(5,4,3,2,1),warn_missing = FALSE)
merchants_data$category_1 <- mapvalues(merchants_data$category_1, c('Y','N'), c(1,0), warn_missing=FALSE)
merchants_data$category_4 <- mapvalues(merchants_data$category_4, c('Y','N'), c(1,0), warn_missing=FALSE)

merchants_data$category_1 <- as.integer(merchants_data$category_1)
merchants_data$category_4 <- as.integer(merchants_data$category_4)
merchants_data$most_recent_sales_range <- as.integer(merchants_data$most_recent_sales_range)
merchants_data$most_recent_purchases_range <- as.integer(merchants_data$most_recent_purchases_range)


#
check_missing_values(merchants_data)
sum(merchants_data$most_recent_sales_range)

##dealing with inf data##
merchants_data$avg_sales_lag3[is.infinite(merchants_data$avg_sales_lag3)] <- NA
merchants_data$avg_purchases_lag3[is.infinite(merchants_data$avg_purchases_lag3)] <- NA
merchants_data$active_months_lag3[is.infinite(merchants_data$active_months_lag3)] <- NA
merchants_data$avg_sales_lag6[is.infinite(merchants_data$avg_sales_lag6)] <- NA
merchants_data$avg_purchases_lag6[is.infinite(merchants_data$avg_purchases_lag6)] <- NA
merchants_data$active_months_lag6[is.infinite(merchants_data$active_months_lag6)] <- NA
merchants_data$avg_sales_lag12[is.infinite(merchants_data$avg_sales_lag12)] <- NA
merchants_data$avg_purchases_lag12[is.infinite(merchants_data$avg_purchases_lag12)] <- NA
merchants_data$active_months_lag12[is.infinite(merchants_data$active_months_lag12)] <- NA

merchants_data$most_recent_sales_range[is.infinite(merchants_data$most_recent_sales_range)] <- NA
merchants_data$most_recent_purchases_range[is.infinite(merchants_data$most_recent_purchases_range)] <- NA
merchants_data$avg_purchases_lag3[is.na(merchants_data$avg_purchases_lag3)] <- mean(merchants_data$avg_purchases_lag3, na.rm = TRUE)
merchants_data$avg_purchases_lag6[is.na(merchants_data$avg_purchases_lag6)] <- mean(merchants_data$avg_purchases_lag6, na.rm = TRUE)
merchants_data$avg_purchases_lag12[is.na(merchants_data$avg_purchases_lag12)] <- mean(merchants_data$avg_purchases_lag12, na.rm = TRUE)

summary(merchants_data)
#Grouping merchants_data features
merge_merchants <- merchants_data %>%
  group_by(merchant_id) %>%
  summarize(
    merchants_avg_sales_lag3_mean = mean(avg_sales_lag3, na.rm = TRUE),
    merchants_avg_purchases_lag3_mean = mean(avg_purchases_lag3, na.rm = TRUE),
    merchants_active_months_lag3_mean = mean(active_months_lag3, na.rm = TRUE),
    
    merchants_avg_sales_lag6_mean = mean(avg_sales_lag6, na.rm = TRUE),
    merchants_avg_purchases_lag6_mean = mean(avg_purchases_lag6, na.rm = TRUE),
    merchants_active_months_lag6_mean = mean(active_months_lag6, na.rm = TRUE),
    
    merchants_avg_sales_lag12_mean = mean(avg_sales_lag12, na.rm = TRUE),
    merchants_avg_purchases_lag12_mean = mean(avg_purchases_lag12, na.rm = TRUE),
    merchants_active_months_lag12_mean = mean(active_months_lag12, na.rm = TRUE),
    
    # Explore more features
    merchants_most_recent_sales_range_mean = mean(most_recent_sales_range, na.rm = TRUE),
    merchants_most_recent_purchases_range_mean = mean(most_recent_purchases_range, na.rm = TRUE),
    
    merchants_category_1_mean = mean(category_1, na.rm = TRUE),
    merchants_category_2_mean = mean(category_2, na.rm = TRUE),
    merchants_category_4_mean = mean(category_4, na.rm = TRUE),
    
    merchants_numerical_1_mean = mean(numerical_1, na.rm = TRUE),
    merchants_numerical_2_mean = mean(numerical_2, na.rm = TRUE)
  )

check_missing_values(merge_merchants)
#merge_merchants$merchants_category_1_mean[is.na(merge_merchants$merchants_category_1_mean)] <- 0
#merge_merchants$merchants_category_4_mean[is.na(merge_merchants$merchants_category_4_mean)] <- 0
#merge_merchants$merchants_avg_purchases_lag6_mean[is.na(merge_merchants$merchants_avg_purchases_lag6_mean)] <- 
#  mean(merge_merchants$merchants_avg_purchases_lag6_mean, na.rm = TRUE)
#merge_merchants$merchants_avg_purchases_lag12_mean[is.na(merge_merchants$merchants_avg_purchases_lag12_mean)] <- 
#  mean(merge_merchants$merchants_avg_purchases_lag12_mean, na.rm = TRUE)
summary(merge_merchants)
#########################
summary(merchants_data)

summary(newmerchant_data$merchant_id)
sum(newmerchant_with_merchants_data)
#merge newmerchant_data and merchants_data
newmerchant_with_merchants_data <- merge(x = newmerchant_data, y = merge_merchants, by='merchant_id',all.x = TRUE)

check_missing_values(newmerchant_with_merchants_data)
head(newmerchant_with_merchants_data)
summary(newmerchant_with_merchants_data)

#missing values
#newmerchant_with_merchants_data$col[is.infinite(newmerchant_with_merchants_data$col)] <- NA
#newmerchant_with_merchants_data$col[is.na(newmerchant_with_merchants_data$col)] <- 
#  get_mode(newmerchant_with_merchants_data$col)

rm_na_or_inf <- function(df){
  df[is.infinite(df)] <- NA
  df[is.na(df)] <- get_mode(df)
  return(df)
}

newmerchant_with_merchants_data$merchants_avg_sales_lag3_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_avg_sales_lag3_mean)
newmerchant_with_merchants_data$merchants_avg_purchases_lag3_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_avg_purchases_lag3_mean)
newmerchant_with_merchants_data$merchants_active_months_lag3_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_active_months_lag3_mean)

newmerchant_with_merchants_data$merchants_avg_sales_lag6_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_avg_sales_lag6_mean)
newmerchant_with_merchants_data$merchants_avg_purchases_lag6_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_avg_purchases_lag6_mean)
newmerchant_with_merchants_data$merchants_active_months_lag6_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_active_months_lag6_mean)

newmerchant_with_merchants_data$merchants_avg_sales_lag12_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_avg_sales_lag12_mean)
newmerchant_with_merchants_data$merchants_avg_purchases_lag12_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_avg_purchases_lag12_mean)
newmerchant_with_merchants_data$merchants_active_months_lag12_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_active_months_lag12_mean)

newmerchant_with_merchants_data$merchants_most_recent_sales_range_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_most_recent_sales_range_mean)
newmerchant_with_merchants_data$merchants_most_recent_purchases_range_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_most_recent_purchases_range_mean)
newmerchant_with_merchants_data$merchants_category_1_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_category_1_mean)
newmerchant_with_merchants_data$merchants_category_2_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_category_2_mean)
newmerchant_with_merchants_data$merchants_category_4_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_category_4_mean)
newmerchant_with_merchants_data$merchants_numerical_1_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_numerical_1_mean)
newmerchant_with_merchants_data$merchants_numerical_2_mean <- rm_na_or_inf(newmerchant_with_merchants_data$merchants_numerical_2_mean)

check_missing_values(newmerchant_with_merchants_data)

#Grouping newmerchants_with_merchants_data features
#groupby1¡GTime
group1_time <- newmerchant_with_merchants_data %>%
  group_by(card_id) %>%
  summarise(
    new_merchant_purchase_date_min = min(purchase_date,na.rm = TRUE),
    new_merchant_purchase_date_max = max(purchase_date,na.rm = TRUE),
    new_merchant_month_lag_std = sd(month_lag,na.rm = TRUE),
    new_merchant_purchase_date_weekend_mean = mean(purchase_date_weekend,na.rm = TRUE), 
    
    # 'avg_sales_lag3'
    new_merchant_merchants_avg_sales_lag3_mean_min = min(merchants_avg_sales_lag3_mean,na.rm = TRUE),
    new_merchant_merchants_avg_sales_lag3_mean_max = max(merchants_avg_sales_lag3_mean,na.rm = TRUE),
    new_merchant_merchants_avg_purchases_lag3_mean_min = min(merchants_avg_purchases_lag3_mean,na.rm = TRUE),
    new_merchant_merchants_avg_purchases_lag3_mean_max = max(merchants_avg_purchases_lag3_mean,na.rm = TRUE),
    new_merchant_merchants_active_months_lag3_mean_min = min(merchants_active_months_lag3_mean,na.rm = TRUE),
    new_merchant_merchants_active_months_lag3_mean_max = max(merchants_active_months_lag3_mean,na.rm = TRUE),
    
    # 'avg_sales_lag6'
    new_merchant_merchants_avg_sales_lag6_mean_min = min(merchants_avg_sales_lag6_mean,na.rm = TRUE),
    new_merchant_merchants_avg_sales_lag6_mean_max = max(merchants_avg_sales_lag6_mean,na.rm = TRUE),
    new_merchant_merchants_avg_purchases_lag6_mean_min = min(merchants_avg_purchases_lag6_mean,na.rm = TRUE),
    new_merchant_merchants_avg_purchases_lag6_mean_max = max(merchants_avg_purchases_lag6_mean,na.rm = TRUE),
    new_merchant_merchants_active_months_lag6_mean_min = min(merchants_active_months_lag6_mean,na.rm = TRUE),
    new_merchant_merchants_active_months_lag6_mean_max = max(merchants_active_months_lag6_mean,na.rm = TRUE),
    
    # 'avg_sales_lag12'
    new_merchant_merchants_avg_sales_lag12_mean_min = min(merchants_avg_sales_lag12_mean,na.rm = TRUE),
    new_merchant_merchants_avg_sales_lag12_mean_max = max(merchants_avg_sales_lag12_mean,na.rm = TRUE),
    new_merchant_merchants_avg_purchases_lag12_mean_min = min(merchants_avg_purchases_lag12_mean,na.rm = TRUE),
    new_merchant_merchants_avg_purchases_lag12_mean_max = max(merchants_avg_purchases_lag12_mean,na.rm = TRUE),
    new_merchant_merchants_active_months_lag12_mean_min  = min(merchants_active_months_lag12_mean,na.rm = TRUE),
    new_merchant_merchants_active_months_lag12_mean_max = max(merchants_active_months_lag12_mean,na.rm = TRUE)
  )
group1_time$new_merchant_purchase_date_diff <- group1_time$new_merchant_purchase_date_max - group1_time$new_merchant_purchase_date_min
group1_time$new_merchant_purchase_date_uptonow <- as.integer(now() - group1_time$new_merchant_purchase_date_max)

check_missing_values(group1_time)
summary(group1_time)

#groupby2¡Gamount
newmerchant_with_merchants_data$new_merchant_purchase_amount_new <- round(newmerchant_with_merchants_data$purchase_amount / 0.00150265118 + 497.06,2)
group2_amount <- newmerchant_with_merchants_data %>%
  group_by(card_id) %>%
  summarise(
    new_merchant_purchase_amount_std = sd(purchase_amount, na.rm = TRUE),
    new_merchant_purchase_amount_sum = sum(purchase_amount, na.rm = TRUE),
    new_merchant_purchase_amount_mean = mean(purchase_amount, na.rm = TRUE),
    new_merchant_card_mean_purchase_amount_new = mean(new_merchant_purchase_amount_new, na.rm = TRUE),
    new_merchant_card_std_purchase_amount_new = sd(new_merchant_purchase_amount_new, na.rm = TRUE),
    new_merchant_card_sum_purchase_amount_new = sum(new_merchant_purchase_amount_new, na.rm = TRUE)
  )
check_missing_values(group2_amount)
summary(group2_amount)
group2_amount$new_merchant_purchase_amount_std[is.na(group2_amount$new_merchant_purchase_amount_std)] <- mean(group2_amount$new_merchant_purchase_amount_std, na.rm = TRUE)
group2_amount$new_merchant_card_std_purchase_amount_new[is.na(group2_amount$new_merchant_card_std_purchase_amount_new)] <- mean(group2_amount$new_merchant_card_std_purchase_amount_new, na.rm = TRUE)


#groupby3¡G
group3_ <- newmerchant_with_merchants_data %>%
  group_by(card_id) %>%
  summarise(
    new_merchant_installments_std = sd(installments, na.rm = TRUE),
    new_merchant_installments_sum = sum(installments, na.rm = TRUE),
    new_merchant_card_id_size = n_distinct(card_id, na.rm = TRUE),
    
    new_merchant_merchants_most_recent_sales_range_mean_min = min(merchants_most_recent_sales_range_mean, na.rm = TRUE),
    new_merchant_merchants_most_recent_sales_range_mean_max = max(merchants_most_recent_sales_range_mean, na.rm = TRUE),
    new_merchant_merchants_most_recent_purchases_range_mean_min = min(merchants_most_recent_purchases_range_mean, na.rm = TRUE),
    new_merchant_merchants_most_recent_purchases_range_mean_max = max(merchants_most_recent_purchases_range_mean, na.rm = TRUE),
    
    new_merchant_category_1_mean_mean = mean(merchants_category_1_mean, na.rm = TRUE),
    new_merchant_category_2_mean_mean = mean(merchants_category_2_mean, na.rm = TRUE),
    new_merchant_category_4_mean_mean = mean(merchants_category_4_mean, na.rm = TRUE),
    
    new_merchant_numerical_1_mean_mean = mean(merchants_numerical_1_mean, na.rm = TRUE),
    new_merchant_numerical_2_mean_mean = mean(merchants_numerical_2_mean, na.rm = TRUE)
  )

check_missing_values(group3_)
summary(group3_)
group3_$new_merchant_installments_std[is.na(group3_$new_merchant_installments_std)] <- mean(group3_$new_merchant_installments_std, na.rm = TRUE)


#Part2 Aggregate
train_data <- merge(x=train_data, y=group1_time, by='card_id', all.x = TRUE)
train_data <- merge(x=train_data, y=group2_amount, by='card_id', all.x = TRUE)
train_data <- merge(x=train_data, y=group3_, by='card_id', all.x = TRUE)
test_data <- merge(x=test_data, y=group1_time, by='card_id', all.x = TRUE)
test_data <- merge(x=test_data, y=group2_amount, by='card_id', all.x = TRUE)
test_data <- merge(x=test_data, y=group3_, by='card_id', all.x = TRUE)
check_missing_values(train_data)
check_missing_values(test_data)
rm(group1_time)
rm(group2_amount)
rm(group3_)

check_missing_values(train_data)
check_missing_values(test_data)
gc()
summary(train_data)

#Fill missing value of X_train & X_test
fill_missing <- function(df){
  df$hist_purchase_date_min[is.na(df$hist_purchase_date_min)] <- min(historical_data$purchase_date, na.rm = TRUE)
  df$hist_purchase_date_max[is.na(df$hist_purchase_date_max)] <- max(historical_data$purchase_date, na.rm = TRUE)
  df$new_merchant_purchase_date_min[is.na( df$new_merchant_purchase_date_min)] <- min(newmerchant_data$purchase_date, na.rm = TRUE)
  df$new_merchant_purchase_date_max[is.na( df$new_merchant_purchase_date_max)] <- max(newmerchant_data$purchase_date, na.rm = TRUE)
  return(df)
}
train <- fill_missing(train_data)
test <- fill_missing(test_data)

check_missing_values(train_data)
#date_diff
date_diff <- function(df){
  #df$first_active_month  = ymd_hms(df$first_active_month)
  df$first_active_month <- as.POSIXct(paste(df$first_active_month, "01", sep = '-'), tz = "UTC")
  df$first_active_month_today_elapsed_time <- as.integer(now() - df$first_active_month)
  df$hist_pdmin_active_month_diff <- (df$hist_purchase_date_min - df$first_active_month)
  df$new_merchant_pdmin_active_month_diff <- (df$new_merchant_purchase_date_min - df$first_active_month)
  return(df)
}

train_data <- date_diff(train_data)
test_data <- date_diff(test_data)
summary(test)

# Convert Timestamp to int64 with imputed missing values
Convert_timestamp <- function(df){
  df$hist_purchase_date_min <- as.integer(df$hist_purchase_date_min)
  df$hist_purchase_date_max <- as.integer(df$hist_purchase_date_max)
  df$new_merchant_purchase_date_min <- as.integer(df$new_merchant_purchase_date_min)
  df$new_merchant_purchase_date_max <- as.integer(df$new_merchant_purchase_date_max)
  return(df)
}
train_data <- Convert_timestamp(train_data)
test_data <- Convert_timestamp(test_data)

#Ratio features
Ratio_features <- function(df){
  df$new_hist_purchase_date_max_ratio = df$new_merchant_purchase_date_max / df$hist_purchase_date_max
  df$new_hist_purchase_date_min_ratio = df$new_merchant_purchase_date_min / df$hist_purchase_date_min
  df$hist_purchase_date_maxmin_ratio = df$hist_purchase_date_max / df$hist_purchase_date_min
  df$new_purchase_date_maxmin_ratio = df$new_merchant_purchase_date_max / df$new_merchant_purchase_date_min
  df$new_hist_card_id_total = df$new_merchant_card_id_size + df$hist_card_id_size
  
  df$new_hist_purchase_date_uptonow_ratio = df$new_merchant_purchase_date_uptonow / df$hist_purchase_date_uptonow
  df$new_hist_purchase_date_uptonow_diff = df$new_merchant_purchase_date_uptonow - df$hist_purchase_date_uptonow  
  df$new_hist_card_id_size_ratio = df$new_merchant_card_id_size / df$hist_card_id_size
  df$new_hist_purchase_amount_mean_ratio = df$new_merchant_purchase_amount_mean / df$hist_purchase_amount_mean
  return(df)
}
train_data <- Ratio_features(train_data)
test_data <- Ratio_features(test_data)

# Filled mean information for the missing cards from newmerchant_data
# Fill missing values with mean values; maybe use median value

#excluded_features <- c(-first_active_month, -card_id, -target, -outliers)
#train_features <- subset(temp, select = c(-first_active_mp, -card_id, -target, -outliers))
fill_median <- function(df){
  for(col in df){
    if(is.double(typeof(df$col)) & col!=6){
      df$col[is.na(df$col)] <- df$col.median() #mean
      
    }
  }
  return(df)
}
train_data <- fill_median(train_data)
test_data <- fill_median(test_data)


#drop columns
#drop_col <- c('hist_purchase_date_max', 'hist_purchase_date_min', 'new_merchant_purchase_date_min', 'new_merchant_purchase_date_min', 'new_merchant_purchase_date_max')
train_data <- subset(train_data, select = c(-hist_purchase_date_max, -hist_purchase_date_min, -new_merchant_purchase_date_min, -new_merchant_purchase_date_min, -new_merchant_purchase_date_max))
train_data <- train_data[train_data$target>-30]
train_data$target_exp <- train_data[train_data$target]
test_data <- subset(test_data, select = c(-hist_purchase_date_max, -hist_purchase_date_min, -new_merchant_purchase_date_min, -new_merchant_purchase_date_min, -new_merchant_purchase_date_max))

#output
write.csv(train_data, 'train_data_part1_part2.csv', quote = F, row.names = F)
write.csv(test_data, 'test_data_part1_part2.csv', quote = F, row.names = F)
