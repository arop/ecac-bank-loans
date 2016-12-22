
# districts
district[,"diff_unemploymant"] <- district[,"unemploymant rate '96"] - district[,"unemploymant rate '95"]
district[,"avg_unemploymant"] <- (district[,"unemploymant rate '96"] + district[,"unemploymant rate '95"])/2

district <- subset(district, select = -c(`unemploymant rate '95`, `unemploymant rate '96`))

district[,"diff_crimes"] <- district[,"no. of commited crimes '96"] - district[,"no. of commited crimes '95"]
district[,"avg_crimes"] <- (district[,"no. of commited crimes '96"] + district[,"no. of commited crimes '95"])/2
district[,"ratio_crimes"] <- district[,"avg_crimes"]/district[,"no. of inhabitants"]

district <- subset(district, select = -c(`no. of commited crimes '95`, `no. of commited crimes '96`))

district <- subset(district, select = -c(`no. of municipalities with inhabitants < 499`,`no. of municipalities with inhabitants 500-1999`,`no. of municipalities with inhabitants 2000-9999`,`no. of municipalities with inhabitants >10000`,`no. of cities`))

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