library(plotly) # https://plot.ly/r/bar-charts/

################### USE chumbo 4 #############################
#loans rejected ratio by age
reject_percents_by_decade <- c()
for (k in 1:6) {
  temp1 <- final_dataset %>% filter(birth_year >= k*10+1920) %>% filter(birth_year < k*10+1930)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_decade[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_decade, type = "l")
plot_ly(
  x = rownames(reject_percents_by_decade),
  y = reject_percents_by_decade,
  name = "Reject percents by decade",
  type = "bar"
)
#os mais velhos sao mais rejeitados

#loans rejected ratio by 5 years
reject_percents_by_5years <- c()
for (k in 1:12) {
  temp1 <- final_dataset %>% filter(birth_year >= k*5+1925) %>% filter(birth_year < k*5+1930)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_5years[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_5years, type = "l")
plot_ly(
  x = rownames(reject_percents_by_5years),
  y = reject_percents_by_5years,
  name = "Reject percents by 5 years",
  type = "bar"
)

#########################################################
reject_percents_by_duration <- c()
for (k in 1:6) {
  temp1 <- final_dataset %>% filter(duration == k*12)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_duration[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_duration, type = "l")
plot_ly(
  x = rownames(reject_percents_by_duration),
  y = reject_percents_by_duration,
  name = "reject_percents_by_duration",
  type = "bar"
)
#inconclusivo
#########################################################
reject_percents_by_balance_sd <- c()
for (k in 1:7) {
  temp1 <- final_dataset %>% filter(balance_sd >= k*5000) %>% filter(balance_sd < k*5000+5000)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_balance_sd[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_balance_sd, type = "l")
plot_ly(
  x = rownames(reject_percents_by_balance_sd),
  y = reject_percents_by_balance_sd,
  name = "reject_percents_by_balance_sd",
  type = "bar"
)

#inconclusivo
#########################################################
reject_percents_by_avg_salary <- c()
for (k in 1:2) {
  temp1 <- final_dataset %>% filter(`average salary` >= k*1000+7000) %>% filter(`average salary` < k*1000+8000)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_avg_salary[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_avg_salary, type = "l")
plot_ly(
  x = rownames(reject_percents_by_avg_salary),
  y = reject_percents_by_avg_salary,
  name = "reject_percents_by_avg_salary",
  type = "bar"
)

#parece inconclusivo
#########################################################

table((final_dataset %>% filter(household_amount > 0))$status)
table((final_dataset %>% filter(household_amount == 0))$status)

table((final_dataset %>% filter(gender == 0))$status) #f
table((final_dataset %>% filter(gender == 1))$status) #m
#inconclusivo

##############################################################
################### USE chumbo 3 #############################
reject_percents_by_balance <- c()
for (k in 0:13) {
  temp1 <- final_dataset %>% filter(balance >= k*10000) %>% filter(balance < k*10000+10000)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_balance[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_balance, type = "l")
plot_ly(
  x = rownames(reject_percents_by_balance),
  y = reject_percents_by_balance,
  name = "reject_percents_by_balance",
  type = "bar"
)

#######################
# CRIMES
reject_percents_by_ratio_crimes <- c() #0.01 - 0.08
for (k in 0:8) {
  temp1 <- final_dataset %>% filter(ratio_crimes >= k*0.01) %>% filter(ratio_crimes < k*0.01+0.01)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_ratio_crimes[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_ratio_crimes, type = "l")
plot_ly(
  x = rownames(reject_percents_by_ratio_crimes),
  y = reject_percents_by_ratio_crimes,
  name = "reject_percents_by_ratio_crimes",
  type = "bar"
)

# UNEMPLOYMANT
reject_percents_by_avg_unemploymant <- c() #0 - 9
for (k in 0:8) {
  temp1 <- final_dataset %>% filter(avg_unemploymant >= k) %>% filter(avg_unemploymant < k+1)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_avg_unemploymant[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_avg_unemploymant, type = "l")
plot_ly(
  x = rownames(reject_percents_by_avg_unemploymant),
  y = reject_percents_by_avg_unemploymant,
  name = "reject_percents_by_avg_unemploymant",
  type = "bar"
)

reject_percents_by_diff_unemploymant <- c() #-0.5 - 2.5
for (k in 0:6) {
  temp1 <- final_dataset %>% filter(diff_unemploymant >= k*0.5-0.5) %>% filter(diff_unemploymant < k*0.5)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_diff_unemploymant[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_diff_unemploymant, type = "l")
plot_ly(
  x = rownames(reject_percents_by_diff_unemploymant),
  y = reject_percents_by_diff_unemploymant,
  name = "reject_percents_by_diff_unemploymant",
  type = "bar"
)

# URBAN INHABITANTS
reject_percents_by_urban_inhabitants <- c() #0 - 9
for (k in 0:9) {
  temp1 <- final_dataset %>% filter(`ratio of urban inhabitants` >= k*10) %>% filter(`ratio of urban inhabitants` < k*10+10)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_urban_inhabitants[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_urban_inhabitants, type = "l")
plot_ly(
  x = rownames(reject_percents_by_urban_inhabitants),
  y = reject_percents_by_urban_inhabitants,
  name = "reject_percents_by_urban_inhabitants",
  type = "bar"
)

# REGION
reject_percents_by_region <- c() #0 - 8
for (k in 0:9) {
  temp1 <- final_dataset %>% filter(region >= k) %>% filter(region < k+1)
  temp2 <- temp1 %>% filter(status == -1)
  reject_percents_by_region[k] <- nrow(temp2)/nrow(temp1)
}

plot(reject_percents_by_region, type = "l")
plot_ly(
  x = rownames(reject_percents_by_region),
  y = reject_percents_by_region,
  name = "reject_percents_by_region",
  type = "bar"
)