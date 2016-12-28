loan_train <- subset(loan_train, select=-c(date))
loan_test <- subset(loan_test, select=-c(date))

#discarded account table

disp_owners <- disp %>% filter(type == 2)
disp_owners <- subset(disp_owners, select = -c(type,disp_id))

accounts <- merge(disp_owners,client)

loan_train <- merge(loan_train, accounts)
loan_test <- merge(loan_test, accounts)

#household
account_household_train <- trans_train %>% slice(which(k_symbol == 1))
account_household_train <- aggregate(account_household_train[,"amount"], list(account_id = account_household_train$account_id), mean)
colnames(account_household_train)[2] <- "household_amount"

account_household_test <- trans_test %>% slice(which(k_symbol == 1))
account_household_test <- aggregate(account_household_test[,"amount"], list(account_id = account_household_test$account_id), mean)
colnames(account_household_test)[2] <- "household_amount"

#add household to loan
loan_train <- left_join(loan_train,account_household_train)
loan_test <- left_join(loan_test,account_household_test)

loan_train$household_amount[is.na(loan_train$household_amount)] <- 0
loan_test$household_amount[is.na(loan_test$household_amount)] <- 0

#add districts
loan_train <- merge(loan_train, district, by.x="district_id", by.y="code", all.x = TRUE)
loan_test <- merge(loan_test, district, by.x="district_id", by.y="code", all.x = TRUE)

##############################################################################

#join cards
card_account_train <- merge(card_train, by.x="disp_id", by.y="disp_id", disp, all.x = TRUE)
card_account_test <- merge(card_test, disp, by.x="disp_id", by.y="disp_id", all.x = TRUE)

card_account_train <- subset(card_account_train, select = -c(`type.x`, disp_id, card_id, client_id, issued))
card_account_test <- subset(card_account_test, select = -c(`type.x`, disp_id, card_id, client_id, issued))

loan_train <- merge(loan_train, card_account_train, all.x = TRUE)
loan_test <- merge(loan_test, card_account_test, all.x = TRUE)

colnames(loan_train)[23] <- "type_card"
colnames(loan_test)[23] <- "type_card"

loan_train$type_card[is.na(loan_train$type_card)] <- 0
loan_test$type_card[is.na(loan_test$type_card)] <- 0

# balance sd

  #train
  account_balances_max <- trans_train %>% group_by(account_id) %>% slice(which.max(balance))
  account_balances_max <- subset(account_balances_max, select = c("trans_id","balance"))
  account_balances_min <- trans_train %>% group_by(account_id) %>% slice(which.min(balance))
  account_balances_min <- subset(account_balances_min, select = c("trans_id","balance"))
  
  # trans_train
  sd_balance <- aggregate(trans_train[,"balance"], list(account_id = trans_train$account_id), sd)
  sd_balance[,"balance_diff"] <- account_balances_max$balance - account_balances_min$balance
  colnames(sd_balance)[2] <- "balance_sd"
  
  loan_train <- merge(loan_train, sd_balance, all.x = TRUE)
  
  # test
  account_balances_max_test <- trans_test %>% group_by(account_id) %>% slice(which.max(balance))
  account_balances_max_test <- subset(account_balances_max_test, select = c("trans_id","balance"))
  account_balances_min_test <- trans_test %>% group_by(account_id) %>% slice(which.min(balance))
  account_balances_min_test <- subset(account_balances_min_test, select = c("trans_id","balance"))
  
  # trans_test
  sd_balance_test <- aggregate(trans_test[,"balance"], list(account_id = trans_test$account_id), sd)
  sd_balance_test[,"balance_diff"] <- account_balances_max_test$balance - account_balances_min_test$balance
  colnames(sd_balance_test)[2] <- "balance_sd"
  
  loan_test <- merge(loan_test, sd_balance_test, all.x = TRUE)

  
account_temp <- subset(account, select = c(account_id, frequency))

loan_train <- merge(loan_train, account_temp, all.x = TRUE)  
loan_test <- merge(loan_test, account_temp, all.x = TRUE)  

loan_train <- subset(loan_train, select = -c(client_id, account_id, name))
loan_test <- subset(loan_test, select = -c(client_id, account_id, name))

#write to fvile
colnames(loan_train)[2] <- "Id"
colnames(loan_train)[6] <- "Predicted"
write.csv(loan_train, file = "C:\\Repositories\\ecac-bank-loans\\datasets\\train_14.csv", row.names = FALSE)
colnames(loan_test)[2] <- "Id"
colnames(loan_test)[6] <- "Predicted"
write.csv(loan_test, file = "C:\\Repositories\\ecac-bank-loans\\datasets\\test_14.csv", row.names = FALSE)