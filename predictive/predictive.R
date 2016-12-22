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

#write to file
colnames(loan_train)[2] <- "Id"
colnames(loan_train)[6] <- "Predicted"
write.csv(loan_train, file = "C:\\Users\\andre\\Documents\\GitHub\\ecac-bank-loans\\datasets\\train_11.csv", row.names = FALSE)
colnames(loan_test)[2] <- "Id"
colnames(loan_test)[6] <- "Predicted"
write.csv(loan_test, file = "C:\\Users\\andre\\Documents\\GitHub\\ecac-bank-loans\\datasets\\test_11.csv", row.names = FALSE)