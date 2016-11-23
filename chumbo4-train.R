library(dplyr)

account_balances_max <- trans_train %>% group_by(account_id) %>% slice(which.max(balance))
account_balances_max <- subset(account_balances_max, select = c("trans_id","balance"))
account_balances_min <- trans_train %>% group_by(account_id) %>% slice(which.min(balance))
account_balances_min <- subset(account_balances_min, select = c("trans_id","balance"))

account_household <- trans_train %>% slice(which(k_symbol == 1))
account_household <- aggregate(account_household[,"amount"], list(account_id = account_household$account_id), mean)
colnames(account_household)[2] <- "household_amount"

# trans_train
sd_balance <- aggregate(trans_train[,"balance"], list(account_id = trans_train$account_id), sd)
sd_balance[,"balance_diff"] <- account_balances_max$balance - account_balances_min$balance
colnames(sd_balance)[2] <- "balance_sd"

#join trans with loan
loan_train <- merge(loan_train, sd_balance, by = "account_id")
loan_train <- subset(loan_train, select = -amount)

loans_household <- left_join(loan_train, account_household)
loans_household$household_amount[is.na(loans_household$household_amount)] <- 0

#clients
client[,"birth_year"] <- as.numeric(format(client$birth_number,'%Y'))
client <- subset(client, select = - birth_number)

district <- subset(district, select = c("code","region","no. of inhabitants","ratio of urban inhabitants","average salary", "diff_unemploymant","diff_crimes"))

client_district <- merge(client, district, by.x = "district_id", by.y = "code")
client_district <- subset(client_district, select = - district_id)

#account
disp <- disp %>% slice(which (type == 1))
account_disp <- merge(account, disp)
account_disp <- subset(account_disp, select = c("account_id","client_id"))

account_client <- merge(client_district,account_disp)

#final
final_dataset <- left_join(loans_household, account_client)
final_dataset <- subset(final_dataset, select = -c(client_id,account_id,date))

colnames(final_dataset)[1] <- "Id"

write.csv(final_dataset, file = "C:\\Users\\andre\\Documents\\GitHub\\ecac-bank-loans\\datasets\\train_07.csv", row.names = FALSE)