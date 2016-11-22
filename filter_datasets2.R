
# districts
district[,"diff_unemploymant"] <- district[,"unemploymant rate '96"] - district[,"unemploymant rate '95"]

district <- subset(district, select = -c(`unemploymant rate '95`, `unemploymant rate '96`))

district[,"diff_crimes"] <- district[,"no. of commited crimes '96"] - district[,"no. of commited crimes '95"]

district <- subset(district, select = -c(`no. of commited crimes '95`, `no. of commited crimes '96`))

#account
account$date <- as.Date(as.character(account$date + 19000000), "%Y%m%d")

#cards
card_test$issued <- as.Date(as.character(card_test$issued + 19000000), "%Y%m%d")
card_train$issued <- as.Date(as.character(card_train$issued + 19000000), "%Y%m%d")

#client
client$birth_number <- as.Date(as.character(client$birth_number + 19000000), "%Y%m%d")

#loan
loan_test$date <- as.Date(as.character(loan_test$date + 19000000), "%Y%m%d")
loan_train$date <- as.Date(as.character(loan_train$date + 19000000), "%Y%m%d")

#trans
trans_test$date <- as.Date(as.character(trans_test$date + 19000000), "%Y%m%d")
trans_train$date <- as.Date(as.character(trans_train$date + 19000000), "%Y%m%d")