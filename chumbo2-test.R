library(dplyr)

women <- client %>% filter( ((birth_number / 100) %% 100) > 50)
men <- client %>% filter( ((birth_number / 100) %% 100) < 50)

women["birth_number"] <- women$birth_number - 5000
women[,"gender"] <- "f"
men[,"gender"] <- "m"

new_client <- rbind(men,women)
new_client <- new_client[ with(new_client, order(client_id)),]
new_client[,"year"] <- floor(new_client$birth_number / 10000)

client_district <- merge(new_client, district, by.x = c("district_id"), by.y = c("code"))

account_disp <- merge(account, disp)
colnames(account_disp)[4] <- "date_account"
colnames(account_disp)[7] <- "type_disp"

account_disp_trans_test <- merge(account_disp, trans_test, by = "account_id")
colnames(account_disp_trans_test)[9] <- "date_trans"
colnames(account_disp_trans_test)[10] <- "type_trans"
colnames(account_disp_trans_test)[12] <- "ammount_trans"

account_disp_trans_loans_test <- merge(account_disp_trans_test, loan_test, by = "account_id")
colnames(account_disp_trans_loans_test)[18] <- "date_loan"
colnames(account_disp_trans_loans_test)[19] <- "ammount_loan"

account_balances <- account_disp_trans_loans_test %>% group_by(account_id) %>% slice(date_trans < date_loan && which.max(date_trans))
sd_balance <- aggregate(trans_test[,"balance"], list(account_id = trans_test$account_id), sd)
sd_balance <- subset(sd_balance, select = c("account_id","balance"))
colnames(sd_balance)[2] <- "balance_sd"

account_balances_sd <- merge(account_balances,sd_balance)
account_balances_sd <- subset(account_balances_sd, select = c("client_id","account_id","district_id","loan_id","disp_id","type_disp","frequency","date_account","ammount_loan","duration","payments","balance","balance_sd","status"))

account_balances_sd_cards <- full_join(account_balances_sd,card_test)
colnames(account_balances_sd_cards)[16] <- "type_card"
account_balances_sd_cards <- subset(account_balances_sd_cards, select = -c(card_id,issued))

account_balances_sd_cards_clients <- merge(account_balances_sd_cards, new_client)
account_balances_sd_cards_clients <- subset(account_balances_sd_cards_clients, select = -district_id)

write.csv(account_balances_sd_cards_clients, file = "C:\\Users\\andre\\Dropbox\\Universidade\\UC\\5º Ano\\ECAC\\Projeto\\test_02.csv", row.names = FALSE)
