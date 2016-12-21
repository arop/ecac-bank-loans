library(dplyr)

#join women and men
new_client <- rbind(men,women)
new_client <- new_client[ with(new_client, order(client_id)),]
new_client[,"year"] <- floor(new_client$birth_number / 10000)

client_district <- merge(new_client, district, by.x = c("district_id"), by.y = c("code"))

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
account_balances_sd <- subset(account_balances_sd, select = c("client_id","account_id","district_id","loan_id","disp_id","type_disp","frequency","date_account","ammount_loan","duration","payments","balance","balance_sd","status"))

account_balances_sd_cards <- full_join(account_balances_sd,card_train)
colnames(account_balances_sd_cards)[16] <- "type_card"
account_balances_sd_cards <- subset(account_balances_sd_cards, select = -c(card_id,issued))

account_balances_sd_cards_clients <- merge(account_balances_sd_cards, new_client)
account_balances_sd_cards_clients <- subset(account_balances_sd_cards_clients, select = -district_id)
colnames(account_balances_sd_cards)[3] <- "district_id_account"

all_dataset_without_account_district <- merge(account_balances_sd_cards, client_district)  
all_dataset <- merge(all_dataset_without_account_district, district, by.x = c("district_id_account"), by.y = c("code"), suffixes = c("_client","_account"))

#remove useless ids
final_dataset <- subset(all_dataset, select = -c(district_id_account,client_id,account_id,disp_id,district_id))

final_dataset$type_card[is.na(final_dataset$type_card)] <- 0

write.csv(final_dataset, file = "C:\\Users\\andre\\Documents\\GitHub\\ecac-bank-loans\\datasets\\train_05.csv", row.names = FALSE)
