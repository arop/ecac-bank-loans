library(plyr)
library(dplyr)
library(plotly) # https://plot.ly/r/bar-charts/

Sys.setenv("plotly_username"="norim_13")
Sys.setenv("plotly_api_key"="bmci9fqy6d")

#last transactions' balance
account_balances <- trans_train %>% group_by(account_id) %>% slice(which.max(date))


#############################################################################################################
#############################################################################################################

# get set rows with balance between min and max
subsetBalanceInterval<-function(set,min,max)
{
  set %>% filter(balance >= min) %>% filter(balance < max)
}


#############################################################################################################
#############################################################################################################

#plot rejects by account balance

balance_loan_status <- subset(merge(account_balances, loan_train, by = "account_id"), select = c("account_id", "balance", "status"))

reject_percents <- c()
delta <- 10000
limit_index <- 135000/delta + 1
for (k in 1:limit_index) {
  temp1 <- subsetBalanceInterval(balance_loan_status,(k-1)*delta,k*delta)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents[k] <- nrow(temp2)/nrow(temp1)
}
plot(reject_percents, type = "l")


#############################################################################################################
#############################################################################################################

# get set rows with ratio between min and max
subsetRatioInterval<-function(set,min,max)
{
  set %>% filter(ratio >= min) %>% filter(ratio < max)
}


# TODO plot by loan amount/avg balance
avg_balance <- aggregate(trans_train[,"balance"], list(account_id=trans_train$account_id), mean)
#ddply(trans_train, .(account_id), summarize,  balance=mean(balance))

balance_loantotal_ratio <- subset(merge(loan_train, avg_balance, by = "account_id"), select = c("account_id", "balance", "status", "amount"))
balance_loantotal_ratio[, "ratio"] <- balance_loantotal_ratio[, "amount"] / balance_loantotal_ratio[, "balance"]
balance_loantotal_ratio

reject_percents <- c()
delta <- 1
limit_index <- 10/delta + 1
for (k in 1:limit_index) {
  temp1 <- subsetRatioInterval(balance_loantotal_ratio,(k-1)*delta,k*delta)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents[k] <- nrow(temp2)/nrow(temp1)
}
#plot(reject_percents, type = "l")

plot_ly(
  x = rownames(reject_percents),
  y = reject_percents,
  name = "SF Zoo",
  type = "bar"
)

#############################################################################################################
#############################################################################################################

# TODO plot by loan duration

#############################################################################################################
#############################################################################################################

# plot by loan mensalidade/avg balance

balance_loanpayments_ratio <- subset(merge(loan_train, avg_balance, by = "account_id"), select = c("account_id", "balance", "status", "payments"))
balance_loanpayments_ratio[, "ratio"] <- balance_loanpayments_ratio[, "payments"] / balance_loanpayments_ratio[, "balance"]
balance_loanpayments_ratio

reject_percents_keys <- c()
reject_percents_values <- c()
delta <- 0.05
limit_index <- 0.4/delta + 1
for (k in 1:limit_index) {
  temp1 <- subsetRatioInterval(balance_loanpayments_ratio,(k-1)*delta,k*delta)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_keys[k] <- k*0.05
  reject_percents_values[k] <- nrow(temp2)/nrow(temp1)
}

reject_percents = data.frame(
      "racio mensalidade / saldo médio" = reject_percents_keys, 
      "percentagem rejeições" = reject_percents_values)
plot(reject_percents, type = "l")

