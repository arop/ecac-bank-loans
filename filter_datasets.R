#################################
## FILTER DATASETS ##############
# transform strings to int
account$frequency <- as.factor(account$frequency)
levels(account$frequency) <- 1:length(levels(account$frequency))
account$frequency <- as.numeric(account$frequency)

card_test$type <- as.factor(card_test$type)
levels(card_test$type) <- 1:length(levels(card_test$type))
card_test$type <- as.numeric(card_test$type)

card_train$type <- as.factor(card_train$type)
levels(card_train$type) <- 1:length(levels(card_train$type))
card_train$type <- as.numeric(card_train$type)

disp$type <- as.factor(disp$type)
levels(disp$type) <- 1:length(levels(disp$type))
disp$type <- as.numeric(disp$type)

district$name <- as.factor(district$name)
levels(district$name) <- 1:length(levels(district$name))
district$name <- as.numeric(district$name)

district$region <- as.factor(district$region)
levels(district$region) <- 1:length(levels(district$region))
district$region <- as.numeric(district$region)

trans_train$type <- as.factor(trans_train$type)
levels(trans_train$type) <- 1:length(levels(trans_train$type))
trans_train$type <- as.numeric(trans_train$type)

trans_train$operation <- as.factor(trans_train$operation)
levels(trans_train$operation) <- 1:length(levels(trans_train$operation))
trans_train$operation <- as.numeric(trans_train$operation)

trans_train$k_symbol <- as.factor(trans_train$k_symbol)
levels(trans_train$k_symbol) <- 1:length(levels(trans_train$k_symbol))
trans_train$k_symbol <- as.numeric(trans_train$k_symbol)

trans_test$type <- as.factor(trans_test$type)
levels(trans_test$type) <- 1:length(levels(trans_test$type))
trans_test$type <- as.numeric(trans_test$type)

trans_test$operation <- as.factor(trans_test$operation)
levels(trans_test$operation) <- 1:length(levels(trans_test$operation))
trans_test$operation <- as.numeric(trans_test$operation)

trans_test$k_symbol <- as.factor(trans_test$k_symbol)
levels(trans_test$k_symbol) <- 1:length(levels(trans_test$k_symbol))
trans_test$k_symbol <- as.numeric(trans_test$k_symbol)

#set missing values
missing_un_95 <- (district$`unemploymant rate '95`) == "?"
district$`unemploymant rate '95`[missing_un_95] <- district$`unemploymant rate '96`[missing_un_95]

missing_crimes_95 <- (district$`no. of commited crimes '95`) == "?"
district$`no. of commited crimes '95`[missing_crimes_95] <- district$`no. of commited crimes '96`[missing_crimes_95]

#get women
women <- client %>% filter( ((birth_number / 100) %% 100) > 50)
men <- client %>% filter( ((birth_number / 100) %% 100) < 50)

#normalize date
women["birth_number"] <- women$birth_number - 5000
#set gender
women[,"gender"] <- 0
men[,"gender"] <- 1

## /FILTER DATASETS #############
#################################