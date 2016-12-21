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