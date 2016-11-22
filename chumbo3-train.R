library(dplyr)

client_district <- merge(client, district, by.x = c("district_id"), by.y = c("code"))

account_disp <- merge(account, disp)
colnames(account_disp)[4] <- "date_account"
colnames(account_disp)[7] <- "type_disp"

account_disp_trans_train <- merge(account_disp, trans_train, by = "account_id")
colnames(account_disp_trans_train)[9] <- "date_trans"
colnames(account_disp_trans_train)[10] <- "type_trans"
colnames(account_disp_trans_train)[12] <- "ammount_trans"

account_disp_trans_loans_train <- merge(account_disp_trans_train, loan_train, by = "account_id")
colnames(account_disp_trans_loans_train)[18] <- "date_loan"
colnames(account_disp_trans_loans_train)[19] <- "ammount_loan"

account_balances <- account_disp_trans_loans_train %>% group_by(account_id) %>% slice(date_trans < date_loan && which.max(date_trans))
sd_balance <- aggregate(trans_train[,"balance"], list(account_id = trans_train$account_id), sd)
sd_balance <- subset(sd_balance, select = c("account_id","balance"))
colnames(sd_balance)[2] <- "balance_sd"

account_balances_sd <- merge(account_balances,sd_balance)
account_balances_sd <- subset(account_balances_sd, select = -c(trans_id,date_trans,type_trans,operation,ammount_trans,k_symbol,bank,account,date_loan,ammount_loan))

account_balances_sd_cards <- full_join(account_balances_sd,card_train)
colnames(account_balances_sd_cards)[15] <- "type_card"
account_balances_sd_cards <- subset(account_balances_sd_cards, select = -c(card_id,issued))

account_balances_sd_cards_clients <- merge(account_balances_sd_cards, client)
account_balances_sd_cards_clients <- subset(account_balances_sd_cards_clients, select = -district_id)
colnames(account_balances_sd_cards)[2] <- "district_id_account"

all_dataset_without_account_district <- merge(account_balances_sd_cards, client_district)

#remove useless ids
final_dataset <- subset(all_dataset_without_account_district, select = -c(district_id_account,client_id,account_id,disp_id,district_id, date_account))

final_dataset$type_card[is.na(final_dataset$type_card)] <- 0

write.csv(final_dataset, file = "C:\\Users\\andre\\Documents\\GitHub\\ecac-bank-loans\\datasets\\train_06.csv", row.names = FALSE)
