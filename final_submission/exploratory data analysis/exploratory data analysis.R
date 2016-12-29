library(dplyr)
library(plotly)

######################################################################
# AGE
clients_per_decade <- c()
for (k in 1:6) {
  temp1 <- client %>% filter(birth_year >= k*10+1920) %>% filter(birth_year < k*10+1930)
  clients_per_decade[k] <- nrow(temp1)
}

plot(clients_per_decade, type = "l")
plot_ly(
  x = c("1920-30", "1930-40", "1940-50", "1950-60", "1960-70", "1970-80"),
  y = clients_per_decade,
  name = "Clients per decade",
  type = "bar"
)

######################################################################
# GENDER

clients_per_gender <- c(nrow(men), nrow(women))
plot(clients_per_gender, type = "l")
plot_ly(
  x = c("Men", "Women"),
  y = clients_per_gender,
  name = "Clients per gender",
  type = "bar"
)

######################################################################
# DISTRICT - REGION

clients_per_region <- c()
region_names <- unique(district$region)
for (r in region_names) {
  temp1 <- client_district %>% filter(region == r)
  clients_per_region[r] <- nrow(temp1)
}
plot(clients_per_district, type = "l")
plot_ly(
  x = region_names,
  y = clients_per_region,
  name = "Clients per region",
  type = "bar"
)


######################################################################
# DISTRIC POPULATION

clients_per_district_population <- c()
for (k in 1:6) {
  temp1 <- client_district %>% filter(`no. of inhabitants` >= k*50000-50000) %>% filter(`no. of inhabitants` < k*50000)
  clients_per_district_population[k] <- nrow(temp1)
}
temp1 <- client_district %>% filter(`no. of inhabitants` >= 300000)
clients_per_district_population[7] <- nrow(temp1)

#plot(clients_per_district_population, type = "l")
plot_ly(
  x = c("000-050", "050-100", "100-150", "150-200", "200-250", "250-300", "300-1500"),
  y = clients_per_district_population,
  name = "Clients per district population",
  type = "bar"
)


######################################################################
# LAST BALANCE BY ACCOUNT
accounts_by_balance <- c()

temp1 <- account_balances %>% filter( balance < 0 )
accounts_by_balance[1] <- nrow(temp1)
for (i in 1:10){
  temp1 <- account_balances %>% filter(balance >= i*10000-10000) %>% filter(balance < i*10000)
  accounts_by_balance[i+1] <- nrow(temp1)
}
temp1 <- account_balances %>% filter( balance >= 100000 )
accounts_by_balance[12] <- nrow(temp1)

plot_ly(
  x = c("< 0", "00-10", "10-20", "20-30", "30-40", "40-50", "50-60","60-70", "70-80", "80-90", "90-100", "> 100"),
  y = accounts_by_balance,
  name = "Account by final balance",
  type = "bar"
)


######################################################################
# HAS HOUSEHOLD?

account_has_household <- c()

temp1 <- account_household %>% filter( is.na(household_amount) )
account_has_household[1] <- nrow(temp1)

temp1 <- account_household %>% filter( !is.na(household_amount) )
account_has_household[2] <- nrow(temp1)

plot_ly(
  x = c("No", "Yes"),
  y = account_has_household,
  name = "Pays household?",
  type = "bar"
)

######################################################################
# ACCOUNTS BY HOUSEHOLD AMOUNT
accounts_by_household_amount <- c()

accounts_with_household <- account_household %>% filter( !is.na(household_amount) )

for (i in 1:10){
  temp1 <- accounts_with_household %>% filter(household_amount >= i*1000-1000) %>% filter(household_amount < i*1000)
  accounts_by_household_amount[i] <- nrow(temp1)
}
temp1 <- accounts_with_household %>% filter(household_amount >= 10000)
accounts_by_household_amount[11] <- nrow(temp1)

plot_ly(
  x = c("00-01k", "01-02k", "02-03k", "03-04k", "04-05k", "05-06k", "06-07k", "07-08k", "08-09k", "09-10k", "10k -  " ),
  y = accounts_by_household_amount,
  name = "Accounts by household ammount",
  type = "bar"
)

