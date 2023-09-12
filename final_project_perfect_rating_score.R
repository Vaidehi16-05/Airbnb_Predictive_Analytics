#install libraries
#install.packages("ROCR")

#load libraries
library(dplyr)
library(pROC)
library(tidyverse)
library(caret)
library(ROCR)
library(SnowballC)
library(glmnet)
library(randomForest)
library(gbm)
library(class)
#install.packages('e1071')
library(e1071)
library(parallel)

#get rid of scientific notations
options(scipen = 999)



#load data files
train_x <- read_csv("airbnb_train_x_2023.csv")
train_y <- read_csv("airbnb_train_y_2023.csv")
test_x <- read_csv("airbnb_test_x_2023.csv")

###merge train_x and test_x to clean the data together
merged_df <- rbind(train_x, test_x)
merged_df <- rownames_to_column(merged_df, var = "row_num")
merged_df$row_num <- parse_number(merged_df$row_num)

############data cleaning
# Convert the column to lowercase
merged_df$access <- str_to_lower(merged_df$access)
merged_df$host_about <- str_to_lower(merged_df$host_about)
merged_df$house_rules <- str_to_lower(merged_df$house_rules)


# Remove any punctuation marks or special characters
merged_df$access <- str_replace_all(merged_df$access, "[[:punct:]]", "")
merged_df$host_about <- str_replace_all(merged_df$host_about, "[[:punct:]]", "")
merged_df$house_rules <- str_replace_all(merged_df$house_rules, "[[:punct:]]", "")


merged_df <- merged_df %>%
  mutate(cancellation_policy = as.factor(ifelse(cancellation_policy == "super_strict_30" | cancellation_policy == "super_strict_60",
                                                "strict",cancellation_policy)),
         cleaning_fee = parse_number(merged_df$cleaning_fee),
         cleaning_fee = ifelse(is.na(cleaning_fee),0,cleaning_fee),
         has_cleaning_fee = as.factor(ifelse(cleaning_fee>0,"YES","NO")),
         price = parse_number(merged_df$price),
         price = ifelse(is.na(price),0,price),
         bedrooms = ifelse(is.na(bedrooms),mean(merged_df$bedrooms, na.rm = TRUE),bedrooms),
         beds = ifelse(is.na(beds),mean(merged_df$beds, na.rm = TRUE),beds),
         host_total_listings_count = ifelse(is.na(merged_df$host_total_listings_count),
                                            mean(host_total_listings_count, na.rm = TRUE),host_total_listings_count),
         price_per_person = price/accommodates,
         has_cleaning_fee = as.factor(ifelse(cleaning_fee>0, 'YES','NO')),
         bed_category = as.factor(ifelse(bed_type == "Real Bed", "bed","other")),
         property_category = as.factor(case_when(property_type=='Apartment' ~ "apartment",
                                                 property_type=='Serviced apartment' ~ "apartment",
                                                 property_type=='Loft' ~ "apartment",
                                                 property_type=='Bed & Breakfast' ~ "hotel",
                                                 property_type=='Boutique hotel' ~ "hotel",
                                                 property_type=='Hostel' ~ "hotel",
                                                 property_type=='Townhouse' ~ "condo",
                                                 property_type=='Condominium' ~ "condo",
                                                 property_type=='House' ~ "house",
                                                 property_type=='Bungalow' ~ "house",
                                                 TRUE ~ 'other')),
         bed_type = as.factor(bed_type),
         room_type = as.factor(room_type),
         property_category = as.factor(property_category))

property_category_medians <- merged_df %>% group_by(property_category)
property_category_medians <- property_category_medians %>% summarise(property_ppp_medeian = median(price_per_person))
merged_df <- merge(x=merged_df,y=property_category_medians, by="property_category")
merged_df <- merged_df %>% mutate(ppp_ind = case_when(price_per_person > property_ppp_medeian ~ 1,
                                              price_per_person <= property_ppp_medeian ~ 0),
                                  market = ifelse(is.na(market),"MISSING",market))

#summary(merged_df)
#cleaning part 3
merged_df <- merged_df %>% group_by(market)%>%mutate(count=n())%>%ungroup()%>%
  mutate(bathrooms = ifelse(is.na(merged_df$bathrooms),median(merged_df$bathrooms, na.rm = TRUE),bathrooms),
         host_is_superhost = ifelse(is.na(merged_df$host_is_superhost),FALSE,host_is_superhost),
         host_is_superhost = factor(host_is_superhost, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         extra_people = parse_number(merged_df$extra_people),
         charges_for_extra = as.factor(ifelse(extra_people>0,"YES","NO")),
         host_acceptance_rate = parse_number(merged_df$host_acceptance_rate),
         host_acceptance = as.factor(case_when(host_acceptance_rate==100 ~ "ALL",
                                               is.na(host_acceptance_rate) ~ "MISSING",
                                               host_acceptance_rate<100 ~ "SOME")),
         host_response_rate = parse_number(merged_df$host_response_rate),
         host_response = as.factor(case_when(host_response_rate==100 ~ "ALL",
                                             is.na(host_response_rate) ~ "MISSING",
                                             host_response_rate<100 ~ 'SOME')),
         has_min_nights = as.factor(ifelse(minimum_nights>1,"YES","NO")),
         market = as.factor(case_when(count<470 ~ "OTHER",TRUE ~ merged_df$market)))
#summary(merged_df)

###########raunak_cleaning
merged_df <- merged_df %>% group_by(state)%>%mutate(count=n())%>%ungroup()%>%
  mutate(host_has_profile_pic = ifelse(is.na(merged_df$host_has_profile_pic),FALSE,host_has_profile_pic),
         host_has_profile_pic = factor(host_has_profile_pic, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         host_identity_verified = ifelse(is.na(merged_df$host_identity_verified),FALSE,host_identity_verified),
         host_identity_verified = factor(host_identity_verified, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         instant_bookable = factor(instant_bookable, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         is_business_travel_ready = ifelse(is.na(merged_df$is_business_travel_ready),FALSE,is_business_travel_ready),
         is_business_travel_ready = factor(is_business_travel_ready, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         is_location_exact = factor(is_location_exact, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         license_status = as.factor(ifelse(is.na(license), "NO",
                                  ifelse(grepl("pending", license, ignore.case = TRUE), "PENDING", "YES"))),
         monthly_price = parse_number(merged_df$monthly_price),
         monthly_price = ifelse(is.na(monthly_price),0,monthly_price),
         require_guest_phone_verification = factor(require_guest_phone_verification, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         require_guest_profile_picture = factor(require_guest_profile_picture, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         requires_license = factor(requires_license, levels = c(FALSE, TRUE), labels = c("NO", "YES")),
         security_deposit = parse_number(merged_df$security_deposit),
         security_deposit = ifelse(is.na(security_deposit),0,security_deposit),
         has_security_deposit = as.factor(cut(security_deposit, 
                                      breaks=c(-Inf, 0,100, 200, 400, Inf),
                                      labels=c("No-Deposit","Low", "Medium-Low", "Medium", "High"),
                                      include.lowest=TRUE)),
         state = as.factor(case_when(count<300 ~ "OTHER",TRUE ~ merged_df$state)),
         area = as.factor(cut(square_feet, 
                              breaks=c(-Inf,372, 750, 913, Inf),
                              labels=c("Small", "Medium-Small", "Medium", "Large"),
                              include.lowest=TRUE)),
         weekly_price = parse_number(merged_df$weekly_price),
         weekly_price = ifelse(is.na(weekly_price),0,weekly_price),
         host_total_listings_count = ifelse(is.na(merged_df$host_total_listings_count),
                                            mean(host_total_listings_count, na.rm = TRUE),host_total_listings_count),
         years_since_first_review = as.integer(difftime(Sys.Date(), merged_df$first_review, units = "days")/365),
         host_response_time = as.factor(case_when(host_response_time=='within an hour' ~ 'one hour',
                                                  host_response_time=='within a few hours' ~ 'few hours',
                                                  host_response_time == 'within a day' ~ 'one day',
                                                  host_response_time == 'a few days or more' ~ 'few days',TRUE ~ 'other')),
         host_since_years = as.integer(difftime(Sys.Date(), merged_df$host_since, units = "days")/365),
         host_since_years = ifelse(is.na(host_since_years),0,host_since_years),
         maximum_nights = as.factor(ifelse(merged_df$maximum_nights>30,"short-term", "long-term")),
         has_summary = as.factor(ifelse(is.na(merged_df$summary),FALSE,TRUE)),
         has_host_about = as.factor(ifelse(is.na(merged_df$host_about),FALSE,TRUE)),
         has_notes = as.factor(ifelse(is.na(merged_df$notes),FALSE,TRUE)),
         transit_convenience = as.factor(ifelse(is.na(merged_df$transit),FALSE,TRUE)),
  )
#house rules
merged_df$hr_no_smoking <- as.factor(grepl("(smoke|smoking)", merged_df$house_rules, ignore.case = TRUE))
merged_df$hr_no_pets <- as.factor(grepl("(pet|pets)", merged_df$house_rules, ignore.case = TRUE))
merged_df$hr_no_parties <- as.factor(grepl("(party|parties|event)", merged_df$house_rules, ignore.case = TRUE))
merged_df$hr_clean_up <- as.factor(grepl("clean.*(themselves|yourself|the place)", merged_df$house_rules, ignore.case = TRUE))
merged_df$hr_restricted_checkin_out <- as.factor(grepl("(check|arrival|departure|checkout).*(restrict|restrictions|restricted)",
                                                             merged_df$house_rules, ignore.case = TRUE))
merged_df$hr_quite_hours <- as.factor(grepl("quiet.*hours|no.*noise|noise.*curfew", merged_df$house_rules, ignore.case = TRUE))
merged_df$hr_no_extra_guests <- as.factor(grepl("no.*extra.*guests|no.*additional.*guests|guests.*by.*approval.*only",
                                                      merged_df$house_rules, ignore.case = TRUE))
merged_df$hr_remove_shoes <-as.factor(grepl("no.*shoes.*inside|remove.*shoes", merged_df$house_rules, ignore.case = TRUE))
merged_df$hr_respect_neighbours <- as.factor(grepl("respect.*neighbors|keep.*noise.*down|be.*quiet|respect",
                                                         merged_df$house_rules, ignore.case = TRUE))

#access
merged_df$access_full <- as.factor(grepl("(complete|full|total|unrestricted).*access|(entire|whole).*(house|apartment|place)|
                                         all.*(amenities|areas)|no.*restrictions", merged_df$access, ignore.case = TRUE))
merged_df$access_parking <- as.factor(grepl("parking.*(available|spot|free|space|permit|access)|
                                        (free|dedicated|street|on-site|designated|street|off-street|reserved|paid).*parking|
                                        garage|driveway", merged_df$access, ignore.case = TRUE))
merged_df$access_restricted <- as.factor(grepl("(limited|restricted|controlled|specific).*(access|entry|areas|zones|sections)|
                                     access.*restrictions|not.*all.*(areas|sections|zones)", merged_df$access, ignore.case = TRUE))
merged_df$access_keyless_entry <- as.factor(grepl("keyless.*(entry|access)|(digital|smart|elctronic|code|touchpad|remote|bluetooth|wi-fi|wifi|
                                              fingerprint|biometric).*(lock|access)|(pin.*code|access.*code|remote.*code).*(access|lock)",
                                                        merged_df$access, ignore.case = TRUE))

##host about
#commercial host
merged_df$is_commercial_host <- as.factor(grepl("property.*manager|real.*estate|vacation.*rental|.*investment.*property|business.*owner|
                                            professional.*host|corporate|boutique.*hotel|(hospitality|management|real.*estate).*company|
                                            executive.*(stay|suite|hotel|listing|property|apartment)|serviced.*apartment|property.*portfolio|
                                            event.*space", merged_df$house_rules, ignore.case = TRUE))



#find the number of amenities
amenities_list <- lapply(merged_df$amenities, function(x) unlist(strsplit(gsub('[{}"]', '', x), ',')))
amenities_count <- numeric(length(amenities_list))
for (i in seq_along(amenities_list)) {
  amenities_count[i] <- length(amenities_list[[i]])
}
merged_df$amenities_count <- amenities_count




#order columns alphabetically
merged_df <- merged_df[, order(names(merged_df))]
##############cleaning end

#reorder shuffled rows
merged_df <- merged_df[order(merged_df$row_num),]

#split into train test after cleaning
train_x_clean <- merged_df[1:99981, ]
test_x_clean <- merged_df[99982:112186, ]

#train_x[99981,]
#train_x_clean[99981,]
#test_x[12205,]
#test_x_clean[12205,]

#join the training y to the training x file
#also turn the target variables into factors
train <- cbind(train_x_clean, train_y) %>%
  mutate(perfect_rating_score = as.factor(perfect_rating_score))%>%select(-high_booking_rate)

test <- test_x_clean


#find highly correlated predictors
#cor(train[, numeric_cols], use = "pairwise.complete.obs")

#exclude monthly_price and beds
#train <- train %>% select(-monthly_price, -beds)

# EXAMPLE PREDICTIONS FOR CONTEST 1

#split data into training and validation
modeling_df <- train %>% select(perfect_rating_score,accommodates,bathrooms,bed_category,bed_type,bedrooms,beds,cancellation_policy,
                                charges_for_extra,guests_included,has_cleaning_fee, has_min_nights,has_security_deposit,host_acceptance,host_has_profile_pic,
                                host_identity_verified ,host_is_superhost,host_response,host_total_listings_count,instant_bookable ,is_business_travel_ready,
                                is_location_exact, license_status,market,ppp_ind,price,property_category,require_guest_phone_verification,
                                require_guest_profile_picture, requires_license,room_type,access_full,access_keyless_entry,access_restricted,
                                access_parking,amenities_count,years_since_first_review,host_since_years,maximum_nights,has_host_about,has_summary,
                                transit_convenience,has_notes,hr_no_parties,hr_no_pets,hr_no_smoking,hr_clean_up,hr_restricted_checkin_out,
                                hr_quite_hours,hr_no_extra_guests,hr_remove_shoes,hr_respect_neighbours)
modeling_df_test <- test %>% select(accommodates,bathrooms,bed_category,bed_type,bedrooms,beds,cancellation_policy,
                                    charges_for_extra,guests_included,has_cleaning_fee, has_min_nights,has_security_deposit,host_acceptance,host_has_profile_pic,
                                    host_identity_verified ,host_is_superhost,host_response,host_total_listings_count,instant_bookable ,is_business_travel_ready,
                                    is_location_exact, license_status,market,ppp_ind,price,property_category,require_guest_phone_verification,
                                    require_guest_profile_picture, requires_license,room_type,access_full,access_keyless_entry,access_restricted,
                                    access_parking,amenities_count,years_since_first_review,host_since_years,maximum_nights,has_host_about,has_summary,
                                    transit_convenience,has_notes,hr_no_parties,hr_no_pets,hr_no_smoking,hr_clean_up,hr_restricted_checkin_out,
                                    hr_quite_hours,hr_no_extra_guests,hr_remove_shoes,hr_respect_neighbours)

#create dummy variables for all variables
dummy <- dummyVars( ~ . , data=modeling_df,fullRank = TRUE)
dummy_test <- dummyVars(~.,data=modeling_df_test,fullRank = TRUE)

#one hot encoding for train
one_hot_airbnb <- data.frame(predict(dummy, newdata = modeling_df))
#one_hot_airbnb$perfect_rating_score = as.factor(one_hot_airbnb$perfect_rating_score)
#one_hot_airbnb <- select(one_hot_airbnb, -(perfect_rating_score.YES))

#one hot encoding for test
one_hot_airbnb_test <- data.frame(predict(dummy_test, newdata = modeling_df_test))


#########data modeling
#split the data into train and valid
train_insts = sample(nrow(one_hot_airbnb), .7*nrow(one_hot_airbnb))

#for log model
data_train <- one_hot_airbnb[train_insts,]
data_valid <- one_hot_airbnb[-train_insts,]

#for ridge and lasso
# remove the target variable from the matrix of features
airbnb_x <- select(one_hot_airbnb, -(perfect_rating_score.YES))
# movies_y is a factor
airbnb_y <- one_hot_airbnb$perfect_rating_score.YES

data_train_x <- airbnb_x[train_insts,]
data_valid_x <- airbnb_x[-train_insts,]
data_train_y <- airbnb_y[train_insts]
data_test_y <- airbnb_y[-train_insts]


valid_actuals <- data_valid$perfect_rating_score
valid_actuals <- ifelse(valid_actuals == 1, 1, 0)
valid_actuals <- ifelse(is.na(valid_actuals), 0, valid_actuals)
valid_actuals <- as.factor(valid_actuals)

############################################################# ride & lasso
data_valid_matrix <- as.matrix(data_valid_x)
#what are the coefficients for an unregularized model?
unreg <- glmnet(data_train_x, data_train_y, family = "binomial", alpha = 0, lambda = 0)
coef(unreg)

#what if we have a very large lambda?
lotsofreg <- glmnet(data_train_x, data_train_y, family = "binomial", alpha = 0, lambda = 10)
coef(lotsofreg)

############################################################ linear model
linear_perfect <- lm(perfect_rating_score.YES~.,data=data_train)
linear_preds <- predict(linear_perfect,newdata = data_valid)
linear_classes <- ifelse(linear_preds>0.45,1,0)
accuracy <- mean(linear_classes == valid_actuals)

############################################################# log model
logistic_perfect <- glm(perfect_rating_score.YES~., data = data_train, family = "binomial")
logit_preds <- predict(logistic_perfect, newdata = data_valid, type = "response")
logit_preds <- ifelse(is.na(logit_preds), 0, logit_preds)
############################################################# optimal ridge and lasso
############################# find optimal ridge & lasso
#seq function to generate a large list
grid <- 10^seq(-4,4,length=100)

# define a function to calculate accuracy
accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}

#storage vector
accs_ridge <- rep(0, length(grid))
accs_lasso <- rep(0, length(grid))

for(i in c(1:length(grid))){
  lam = grid[i] #current value of lambda
  
  #train a ridge model with lambda = lam
  glmout_ridge <- glmnet(data_train_x, data_train_y, family = "binomial", alpha = 0, lambda = lam)
  #train a lasso model with lambda = lam
  glmout_lasso <- glmnet(data_train_x, data_train_y, family = "binomial", alpha = 1, lambda = lam)
  
  #make predictions as usual
  data_valid_matrix <- as.matrix(data_valid_x)
  preds_ridge <- predict(glmout_ridge, newx = data_valid_matrix, type = "response")
  
  preds_lasso <- predict(glmout_lasso, newx = data_valid_matrix, type = "response")
  
  #classify and compute accuracy
  classifications_ridge <- ifelse(preds_ridge > .46, 1, 0)
  inner_ridge_acc <- accuracy(classifications_ridge, data_test_y)
  accs_ridge[i] <- inner_ridge_acc
  
  #classify and compute accuracy
  classifications_lasso <- ifelse(preds_lasso > .46, 1, 0)
  inner_lasso_acc <- accuracy(classifications_lasso, data_test_y)
  accs_lasso[i] <- inner_lasso_acc
}

#plot fitting curve - easier to read if we plot logs
plot(log10(grid), accs_ridge)
plot(log10(grid), accs_lasso)

best_validation_index_ridge <- which.max(accs_ridge)
best_lambda_ridge <- grid[best_validation_index_ridge]
#best_lambda_ridge

best_validation_index_lasso<- which.max(accs_lasso)
best_lambda_lasso <- grid[best_validation_index_lasso]
#best_lambda_lasso


optimal_ridge <- glmnet(data_train_x, data_train_y, family = "binomial", alpha = 0, lambda = best_lambda_ridge)
data_valid_matrix <- as.matrix(data_valid_x)
ridge_preds  <- predict(optimal_ridge, newx = data_valid_matrix, type = "response")
ridge_preds  <- ifelse(is.na(ridge_preds), 0, ridge_preds)

optimal_lasso = glmnet(data_train_x, data_train_y, family = "binomial", alpha = 1, lambda = best_lambda_lasso)
lasso_preds <- predict(optimal_lasso, newx = data_valid_matrix, type = "response")
lasso_preds <- ifelse(is.na(lasso_preds), 0, lasso_preds)
############################################################ kNN optimized
accuracy <- function(classifications, actuals){
  correct_classifications <- ifelse(classifications == actuals, 1, 0)
  acc <- sum(correct_classifications)/length(classifications)
  return(acc)
}

kvec <- c(1:50)*2 #every even number between 1 and 40
kvec

#initialize storage
va_acc <- rep(0, length(kvec))
tr_acc <- rep(0, length(kvec))

#for loop
for(i in 1:length(kvec)){
  inner_k <- kvec[i]
  
  inner_tr_preds <- knn(data_train_x, data_train_x, data_train_y, k=inner_k, prob = TRUE) 
  inner_tr_acc <- accuracy(inner_tr_preds, train_y)
  tr_acc[i] <- inner_tr_acc
  
  #now repeat for predictions in the validation data
  inner_va_preds <- knn(data_train_x, data_valid_x, data_train_y, k=inner_k, prob = TRUE) 
  inner_va_acc <- accuracy(inner_va_preds, valid_y)
  va_acc[i] <- inner_va_acc
  
}
#create plot here
plot(kvec, tr_acc, col = "blue", type = 'l', ylim = c(.6,1))
lines(kvec, va_acc, col = "red")


#Pick the best k from above 

best_validation_index <- which.max(va_acc)
best_k <- kvec[best_validation_index]

#make predictions with the best k and retrieve the probability that Y=1
best_k_preds <- knn(data_train_x, data_valid_x, data_train_y, 10, prob=TRUE)
best_probs <- attr(best_k_preds, "prob")
best_probofYES <- ifelse(best_k_preds == "YES", best_probs, 1-best_probs)

#made a function to classify given a cutoff and assess accuracy, TPR, and TNR
classify_evaluate <- function(predicted_probs, actual_y, cutoff){
  
  classifications <- ifelse(predicted_probs > cutoff, "YES", "NO")
  classifications <- factor(classifications, levels = levels(actual_y))
  
  CM <- confusionMatrix(data = classifications,
                        reference = actual_y,
                        positive = "YES")
  
  CM_accuracy <- as.numeric(CM$overall["Accuracy"])
  CM_TPR <- as.numeric(CM$byClass["Sensitivity"])
  CM_TNR <- as.numeric(CM$byClass["Specificity"])
  
  return(c(CM_accuracy, CM_TPR, CM_TNR))
  
}

#print the performance for each cutoff
classify_evaluate(best_probofYES, valid_actuals, .25)
classify_evaluate(best_probofYES, valid_actuals, .5)
classify_evaluate(best_probofYES, valid_actuals, .75)



############################################################ boosting
#needs a numerical target variable
train$perfect_rating_score <- as.numeric(train$perfect_rating_score)

boost.mod <- gbm(perfect_rating_score.YES~.,data=data_train,
                 distribution="bernoulli",
                 n.trees=1000,
                 interaction.depth=4)
boost_preds <- predict(boost.mod,newdata=data_valid,type='response',n.trees=1000)

#classify with a cutoff and compute accuracy
boost_class <- ifelse(boost_preds>.46,1,0)
boost_acc <- mean(ifelse(boost_class==data_valid$perfect_rating_score.YES,1,0))
boost_acc
########################################################## boosting optimization
tune.grid <- expand.grid(
  n.trees = c(500, 1000, 2000),
  interaction.depth = c(3, 4, 5),
  shrinkage = c(0.1, 0.01),
  n.minobsinnode = c(10, 20)
)

# Perform grid search cross-validation
set.seed(123) # for reproducibility
boost.model <- train(
  perfect_rating_score.YES ~ .,
  data = data_train,
  method = "gbm",
  trControl = trainControl(method = "cv", number = 5),
  tuneGrid = tune.grid
)
boost.model$bestTune
boost.model$results

boost.mod
# print best model performance and parameters
boost_tuned <- gbm(perfect_rating_score.YES~.,data=data_train,
                 distribution="bernoulli",
                 n.trees=1000,
                 interaction.depth=5,
                 shrinkage=0.1,
                 n.minobsinnode = 10)

# make predictions on validation data using best model
best_boost_preds <- predict(boost_tuned, newdata = data_valid)
best_boost_acc <- mean(ifelse(boost_class==data_valid$perfect_rating_score.YES,1,0))
best_boost_acc
############################################################# cutoff optimization

# calculate TPR, FPR, and accuracy for different cutoffs
cutoffs <- seq(0, 1, by = 0.01)
linear_tpr <- numeric(length(cutoffs))
linear_fpr <- numeric(length(cutoffs))
linear_acc <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
  linear_preds_class <- ifelse(linear_preds > cutoffs[i], 1, 0)
  linear_tpr[i] <- sum(linear_preds_class[valid_actuals == 1] == 1) / sum(valid_actuals == 1)
  linear_fpr[i] <- sum(linear_preds_class[valid_actuals == 0] == 1) / sum(valid_actuals == 0)
  linear_acc[i] <- sum(linear_preds_class == valid_actuals) / length(valid_actuals)
}

logit_tpr <- numeric(length(cutoffs))
logit_fpr <- numeric(length(cutoffs))
logit_acc <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
  logit_preds_class <- ifelse(logit_preds > cutoffs[i], 1, 0)
  logit_tpr[i] <- sum(logit_preds_class[valid_actuals == 1] == 1) / sum(valid_actuals == 1)
  logit_fpr[i] <- sum(logit_preds_class[valid_actuals == 0] == 1) / sum(valid_actuals == 0)
  logit_acc[i] <- sum(logit_preds_class == valid_actuals) / length(valid_actuals)
}

ridge_tpr <- numeric(length(cutoffs))
ridge_fpr <- numeric(length(cutoffs))
ridge_acc <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
  ridge_preds_class <- ifelse(ridge_preds > cutoffs[i], 1, 0)
  ridge_tpr[i] <- sum(ridge_preds_class[valid_actuals == 1] == 1) / sum(valid_actuals == 1)
  ridge_fpr[i] <- sum(ridge_preds_class[valid_actuals == 0] == 1) / sum(valid_actuals == 0)
  ridge_acc[i] <- sum(ridge_preds_class == valid_actuals) / length(valid_actuals)
  
}

lasso_tpr <- numeric(length(cutoffs))
lasso_fpr <- numeric(length(cutoffs))
lasso_acc <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
  lasso_preds_class <- ifelse(lasso_preds > cutoffs[i], 1, 0)
  lasso_tpr[i] <- sum(lasso_preds_class[valid_actuals == 1] == 1) / sum(valid_actuals == 1)
  lasso_fpr[i] <- sum(lasso_preds_class[valid_actuals == 0] == 1) / sum(valid_actuals == 0)
  lasso_acc[i] <- sum(lasso_preds_class == valid_actuals) / length(valid_actuals)
}

knn_tpr <- numeric(length(cutoffs))
knn_fpr <- numeric(length(cutoffs))
knn_acc <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
  knn_preds_class <- ifelse(best_probofYES > cutoffs[i], 1, 0)
  knn_tpr[i] <- sum(knn_preds_class[valid_actuals == 1] == 1) / sum(valid_actuals == 1)
  knn_fpr[i] <- sum(knn_preds_class[valid_actuals == 0] == 1) / sum(valid_actuals == 0)
  knn_acc[i] <- sum(knn_preds_class == valid_actuals) / length(valid_actuals)
}

boost_tpr <- numeric(length(cutoffs))
boost_fpr <- numeric(length(cutoffs))
boost_acc <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
  boost_preds_class <- ifelse(boost_preds > cutoffs[i], 1, 0)
  boost_tpr[i] <- sum(boost_preds_class[valid_actuals == 1] == 1) / sum(valid_actuals == 1)
  boost_fpr[i] <- sum(boost_preds_class[valid_actuals == 0] == 1) / sum(valid_actuals == 0)
  boost_acc[i] <- sum(boost_preds_class == valid_actuals) / length(valid_actuals)
}

best_boost_tpr <- numeric(length(cutoffs))
best_boost_fpr <- numeric(length(cutoffs))
best_boost_acc <- numeric(length(cutoffs))
for (i in 1:length(cutoffs)) {
  best_boost_preds_class <- ifelse(best_boost_preds > cutoffs[i], 1, 0)
  best_boost_tpr[i] <- sum(best_boost_preds_class[valid_actuals == 1] == 1) / sum(valid_actuals == 1)
  best_boost_fpr[i] <- sum(best_boost_preds_class[valid_actuals == 0] == 1) / sum(valid_actuals == 0)
  best_boost_acc[i] <- sum(best_boost_preds_class == valid_actuals) / length(valid_actuals)
}

# find the maximum TPR and the lowest FPR below 10% for each model

linear_cutoff <- cutoffs[which.max(linear_tpr * (linear_fpr < 0.1))]
linear_acc_at_cutoff <- linear_acc[which(cutoffs == linear_cutoff)]
linear_tpr_at_cutoff <- linear_tpr[which(cutoffs == linear_cutoff)]
linear_fpr_at_cutoff <- linear_fpr[which(cutoffs == linear_cutoff)]

logit_cutoff <- cutoffs[which.max(logit_tpr * (logit_fpr < 0.1))]
logit_acc_at_cutoff <- logit_acc[which(cutoffs == logit_cutoff)]
logit_tpr_at_cutoff <- logit_tpr[which(cutoffs == logit_cutoff)]
logit_fpr_at_cutoff <- logit_fpr[which(cutoffs == logit_cutoff)]

ridge_cutoff <- cutoffs[which.max(ridge_tpr * (ridge_fpr < 0.1))]
ridge_acc_at_cutoff <- ridge_acc[which(cutoffs == ridge_cutoff)]
ridge_tpr_at_cutoff <- ridge_tpr[which(cutoffs == ridge_cutoff)]
ridge_fpr_at_cutoff <- ridge_fpr[which(cutoffs == ridge_cutoff)]

lasso_cutoff <- cutoffs[which.max(lasso_tpr * (lasso_fpr < 0.1))]
lasso_acc_at_cutoff <- lasso_acc[which(cutoffs == lasso_cutoff)]
lasso_tpr_at_cutoff <- lasso_tpr[which(cutoffs == lasso_cutoff)]
lasso_fpr_at_cutoff <- lasso_fpr[which(cutoffs == lasso_cutoff)]

knn_cutoff <- cutoffs[which.max(knn_tpr * (knn_fpr < 0.1))]
knn_acc_at_cutoff <- knn_acc[which(cutoffs == knn_cutoff)]
knn_tpr_at_cutoff <- knn_tpr[which(cutoffs == knn_cutoff)]
knn_fpr_at_cutoff <- knn_fpr[which(cutoffs == knn_cutoff)]

boost_cutoff <- cutoffs[which.max(boost_tpr * (boost_fpr < 0.1))]
boost_acc_at_cutoff <- boost_acc[which(cutoffs == boost_cutoff)]
boost_tpr_at_cutoff <- boost_tpr[which(cutoffs == boost_cutoff)]
boost_fpr_at_cutoff <- boost_fpr[which(cutoffs == boost_cutoff)]

best_boost_cutoff <- cutoffs[which.max(best_boost_tpr * (best_boost_fpr < 0.1))]
best_boost_acc_at_cutoff <- best_boost_acc[which(cutoffs == best_boost_cutoff)]
best_boost_tpr_at_cutoff <- best_boost_tpr[which(cutoffs == best_boost_cutoff)]
best_boost_fpr_at_cutoff <- best_boost_fpr[which(cutoffs == best_boost_cutoff)]


######################################################################
# calculate TPR, FPR, and accuracy for different cutoffs
logit_perf <- roc(valid_actuals, logit_preds)
ridge_perf <- roc(valid_actuals, ridge_preds)
lasso_perf <- roc(valid_actuals, lasso_preds)
knn_perf <- roc(valid_actuals,best_probofYES )
boost_perf <- roc(valid_actuals, boost_preds)
boost_tuned_perf <- roc(valid_actuals, best_boost_preds)

# plot ROC curves
plot(linear_perfect, col="blue")
plot(logistic_perfect, col="yellow")
plot(optimal_ridge, add=TRUE, col="red")
plot(optimal_lasso, add=TRUE, col="orange")
plot(best_k_preds, add=TRUE, col="brown")
plot(boost.mod, add=TRUE, col="gray")
plot(boost_tuned, add=TRUE, col="green")

# Compute the ROC curves for each model
roc_model <- roc(valid_actuals, linear_preds, ret = TRUE)
roc_model1 <- roc(valid_actuals, logit_preds, ret = TRUE)
roc_model2 <- roc(valid_actuals, ridge_preds, ret = TRUE)
roc_model3 <- roc(valid_actuals, lasso_preds, ret = TRUE)
roc_model4 <- roc(valid_actuals, best_probofYES, ret = TRUE)
roc_model5 <- roc(valid_actuals, boost_preds, ret = TRUE)
roc_model6 <- roc(valid_actuals, best_boost_preds, ret = TRUE)

# Create an empty plot
plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1),
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     main = "ROC Curves for Multiple Models")

# Add ROC curves for each model to the plot
lines(roc_model1, col = "red", lwd = 2)
lines(roc_model2, col = "blue", lwd = 2)
lines(roc_model3, col = "brown", lwd = 2)
lines(roc_model4, col = "gray", lwd = 2)
lines(roc_model5, col = "yellow", lwd = 2)
lines(roc_model6, col = "green", lwd = 2)
lines(roc_model, col = "orange", lwd = 2)

# Add a legend
legend("bottomright", legend = c("Linear Regression", "Logistic Regression", "Redge Regression","Lasso Regression", "kNN","Boosting","Boosting Tuned"),
       col = c("orange","red", "blue", "brown","gray","yellow","green"), lwd = 2)

# Add the diagonal line (random classifier)
abline(a = 0, b = 1, col = "gray", lty = "dashed")

# calculate AUC
linear_auc <- auc(roc_model)
logit_auc <- auc(roc_model1)
ridge_auc <- auc(roc_model2)
lasso_auc <- auc(roc_model3)
knn_auc <- auc(roc_model4)
boost_auc <- auc(roc_model5)
boost_tuned_auc <- auc(roc_model6)

# print("Linear model:")
# print(paste("Max TPR at optimal cutoff:", linear_tpr_at_cutoff))
# print(paste("Min FPR at optimal cutoff below 10%:", linear_fpr_at_cutoff, ""))
# print(paste("Cutoff:", linear_cutoff, ""))
# print(paste("Accuracy at cutoff:", linear_acc_at_cutoff, ""))
# print(paste("Linear AUC:", linear_auc, ""))
# 
# print("Logit model:")
# print(paste("Max TPR at optimal cutoff:", logit_tpr_at_cutoff, ""))
# print(paste("Min FPR at optimal cutoff below 10%:", logit_tpr_at_cutoff, ""))
# print(paste("Optimal Cutoff:", logit_cutoff, ""))
# print(paste("Accuracy at Optimal cutoff:", logit_acc_at_cutoff, ""))
# print(paste("Logit AUC:", logit_auc, ""))
# 
# print("Ridge model:")
# print(paste("Max TPR at optimal cutoff:", ridge_tpr_at_cutoff, ""))
# print(paste("Min FPR at optimal cutoff below 10%:", ridge_tpr_at_cutoff, ""))
# print(paste("Optimal Cutoff:", ridge_cutoff, ""))
# print(paste("Accuracy at Optimal cutoff:", ridge_acc_at_cutoff, ""))
# print(paste("Ridge AUC:", ridge_auc, ""))
# 
# print("Lasso model:")
# print(paste("Max TPR at optimal cutoff:", lasso_tpr_at_cutoff, ""))
# print(paste("Min FPR at optimal cutoff below 10%:", lasso_tpr_at_cutoff, ""))
# print(paste("Optimal Cutoff:", lasso_cutoff, ""))
# print(paste("Accuracy at Optimal cutoff:", lasso_acc_at_cutoff, ""))
# print(paste("Lasso AUC:", lasso_auc, ""))
# 
# print("Best kNN model:")
# print(paste("Max TPR at optimal cutoff:", knn_tpr_at_cutoff, ""))
# print(paste("Min FPR at optimal cutoff below 10%:", knn_tpr_at_cutoff, ""))
# print(paste("OptimalCutoff:", knn_cutoff, ""))
# print(paste("Accuracy at Optimal cutoff:", knn_acc_at_cutoff, ""))
# print(paste("kNN AUC:", knn_auc, ""))
# 
# print("Boosting model:")
# print(paste("Max TPR at optimal cutoff:", boost_tpr_at_cutoff, ""))
# print(paste("Min FPR at optimal cutoff below 10%:", boost_tpr_at_cutoff, ""))
# print(paste("Optimal Cutoff:", boost_cutoff, ""))
# print(paste("Accuracy at Optimal cutoff:", boost_acc_at_cutoff, ""))
# print(paste("Boost AUC:", boost_auc, ""))
# 
# print("Best Boosting model:")
# print(paste("Max TPR at optimal cutoff:", best_boost_tpr_at_cutoff, ""))
# print(paste("Min FPR at optimal cutoff below 10%:", best_boost_fpr_at_cutoff, ""))
# print(paste("Optimal Cutoff:", best_boost_cutoff, ""))
# print(paste("Accuracy at Optimal cutoff:", best_boost_acc_at_cutoff, ""))
# print(paste("Boosting Tuned AUC:",boost_tuned_auc,""))




########################################cross validation
# create a list of the five models
models <- list(
  "Logistic Regression" = logistic_perfect,
  "Ridge Regression" = optimal_ridge,
  "Lasso Regression" = optimal_lasso,
  "Linear Regression" = linear_perfect,
  "Best k kNN" = best_k_preds,
  "Boosting" = boost.mod,
  "Best Boosting" = boost_tuned
)

# define the 5-fold cross-validation
ctrl <- trainControl(method = "cv", number = 5)

# perform cross-validation on each model and store the results
results <- lapply(models, function(model) {
  # use the train() function from caret to perform cross-validation
  cv <- train(
    x = train[, -ncol(train)],
    y = train$perfect_rating_score,
    method = model,
    trControl = ctrl,
    tuneLength = 5
  )
  
  # extract the mean and standard deviation of the area under the ROC curve (AUC) from the cross-validation results
  auc_mean <- mean(cv$results$ROC)
  auc_sd <- sd(cv$results$ROC)
  
  # return the model name, mean AUC, and standard deviation of AUC
  return(c(model = names(model), mean_AUC = auc_mean, sd_AUC = auc_sd))
})

# combine the cross-validation results into a data frame
results_df <- do.call(rbind, results)

# print the results
print(results_df)

####################################################### output final predictions
final_boost_tuned <- gbm(perfect_rating_score.YES~.,data=data_train,
                   distribution="bernoulli",
                   n.trees=1000,
                   interaction.depth=5,
                   shrinkage=0.1,
                   n.minobsinnode = 10)

# make predictions on validation data using best model
final_best_boost_preds <- predict(boost_tuned, newdata = data_valid)

# For perfect_rating_score, each row should be a binary YES (is perfect) or NO (not perfect)
#make binary classifications (make sure to check for NAs!)
classifications_perfect <- ifelse(boost_preds > .06, "YES", "NO")
classifications_perfect <- ifelse(is.na(classifications_perfect), "NO", classifications_perfect)
summary(classifications_perfect)
#this code creates sample outputs in the correct format
write.table(classifications_perfect, "perfect_rating_score_group14.csv", row.names = FALSE)


########################################### graphical analysis & visualizations
# Chart1 
chart1 <- merged_df %>%
  #group_by(`city_name`) %>%
  summarise(count(merged_df,city_name))

bar_chart1 <- chart1 %>%
  ggplot(aes(y= reorder(`city_name`, `n`), x=`n`)) +
  geom_col(fill="red4") +
  theme_classic() +
  labs(title="Number of Residence for Each City",
       x="Number of Residence", y="City")

bar_chart1 

# chart2
chart2 <- merged_df %>%
  group_by(`city_name`) %>%
  summarise(mean_total = mean(cleaning_fee),
            total = sum(cleaning_fee))
c2 <- chart2 %>%
  ggplot(aes(x=mean_total, y=reorder(`city_name`, mean_total))) +
  #geom_col(fill="red4")+
  geom_point(aes(color="grey50"),size=5)+
  theme_classic()+
  geom_text(aes(x=mean_total,
                label=paste("$", round(mean_total, digits = 0), sep="")),
            color="black")+
  labs(title="Avg. Clean Fee for Each City",
       x="Avg. Clean Fee", y="City")

c2

#Chart 3
chart3 <- merged_df %>%
  group_by(cancellation_policy, has_security_deposit) %>%
  count() %>%
  ungroup() %>%
  mutate(City = fct_relevel(has_security_deposit, "No-Deposit","Low", "Medium-Low", "Medium", "High"))

c3 <- chart3 %>%
  ggplot(aes(y=reorder(has_security_deposit, n), x=n, group=cancellation_policy)) +
  geom_col(aes(fill=cancellation_policy)) +
  theme_classic() +
  labs(title="Security Deposit ",
       x="Number of Residence", y="Categories of Security Deposit")

c3


# Chart4
c4 <- ggplot(merged_df, aes(x=host_acceptance_rate)) +
  geom_histogram(bins=10, fill="steelblue", color="white")+
  theme_classic() +
  labs(title="Histogram of Host Acceptance Rate",
       x="Histogram of Host Acceptance Rate", y="Count")
c4

# Chart5
chart5 <- merged_df %>% 
  select(price_per_person, security_deposit, 
         cleaning_fee,
         extra_people, 
         weekly_price) %>%
  na.omit() %>%
  cor() %>% as.data.frame() 

head(chart5)

chart5a <- chart5 %>% 
  rownames_to_column(var = "Var1") %>% 
  pivot_longer(price_per_person:weekly_price, 
               names_to="Var2", values_to="corr") 

chart5a %>%
  ggplot(aes(x=Var2, y=Var1, fill=corr)) + 
  geom_tile(color="white") +
  labs(title="Correlation Between Each Fee",
       x="", y="")
