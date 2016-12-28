library(dplyr)

#get women
women <- client %>% filter( ((birth_number / 100) %% 100) > 50)
men <- client %>% filter( ((birth_number / 100) %% 100) < 50)

#normalize date
women["birth_number"] <- women$birth_number - 5000
#set gender
women[,"gender"] <- 0
men[,"gender"] <- 1

client <- rbind(women,men)
client <- client[ with(client, order(client_id)),]

#client
client$birth_number <- as.Date(as.character(client$birth_number + 19000000), "%Y%m%d")
client[,"birth_year"] <- as.numeric(format(client$birth_number,'%Y'))

client_district <- merge(client, district, by.x = "district_id", by.y = "code")

account_balances <- trans_train %>% group_by(account_id) %>% slice(which.max(date))
account_balances <- subset(account_balances, select = c(account_id, balance, date))

account_household <- trans_train %>% slice(which(k_symbol == "household"))
account_household <- aggregate(account_household[,"amount"], list(account_id = account_household$account_id), mean)
colnames(account_household)[2] <- "household_amount"

account_household <- merge(account, account_household, by = "account_id", all.x = TRUE)

#############################################
client_district <- subset(client_district, select = c(client_id, birth_year, birth_number,gender,birth_year,name,region,`no. of inhabitants`))

account_client <- merge(disp, account)
account_client <- merge(account_client, client_district)
account_client <- subset(account_client, select = -c(frequency,type,disp_id,district_id))

descriptive_dataset <- merge(account_client, account_household)
descriptive_dataset <- subset(descriptive_dataset, select = -c(district_id,date,birth_year))


##########################################


# sd balance
sd_balance <- aggregate(trans_train[,"balance"], list(account_id = trans_train$account_id), sd)
sd_balance_test <- aggregate(trans_test[,"balance"], list(account_id = trans_test$account_id), sd)

colnames(sd_balance)[2] <- "balance_sd"
colnames(sd_balance_test)[2] <- "balance_sd"

sd_balance_all <- merge(x = sd_balance, y = sd_balance_test, all = TRUE)

descriptive_dataset <- merge(descriptive_dataset, sd_balance_all, all.x=TRUE)

#avg balance
avg_balance <- aggregate(trans_train[,"balance"], list(account_id = trans_train$account_id), mean)
avg_balance_test <- aggregate(trans_test[,"balance"], list(account_id = trans_test$account_id), mean)

colnames(avg_balance)[2] <- "balance_avg"
colnames(avg_balance_test)[2] <- "balance_avg"

avg_balance_all <- merge(x = avg_balance, y = avg_balance_test, all = TRUE)

descriptive_dataset <- merge(descriptive_dataset, avg_balance_all, all.x=TRUE)




write.csv(descriptive_dataset, file = "C:\\Repositories\\ecac-bank-loans\\datasets\\descriptive_01_avg_sd_balance.csv", row.names = FALSE)