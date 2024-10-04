#Installing packages

install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("cluster")
install.packages("shiny")
install.packages("stats")
install.packages("pacman")

#Loading Packages
pacman::p_load(pacman,dplyr,shiny,ggplot2,shiny,tidyr,cluster,stats)


# Creating Web_source data
set.seed(123)
Web_Source <- data.frame(
  date = seq.Date(from = as.Date("2024-03-01"), to = as.Date("2024-08-31"), by = "day"),
  exit_rate = runif(184, min = 0.05, max = 0.13),
  potential_views = runif(184, min = 0.03, max = 0.09),
  No_of_users = sample(1500:6000, 184, replace = TRUE),
  Device_type = sample(c("Mobile", "Tablet", "Laptop"), 184, replace = TRUE),
  Old_VS_New = sample(c("Old", "New"), 184, replace = TRUE)
)
# Writing Web_Source data to csv file

write.csv(Web_Source,"Website_data.csv", row.names = FALSE)

# Creating Social media data
social_media_data <- data.frame(
  websites = c("Facebook", "X_Twitter", "LinkedIn", "Instagram", "Medium"),
  no_of_followers = c(25566, 40340, 13560, 34511, 11231),
  no_of_posts = c(1233, 1100, 890, 991, 230),
  no_of_interactions = c(13500, 12000, 1350, 7690, 4980),
  no_of_reach = c(11021, 27839, 7890, 18907, 8921),
  no_of_shares = c(750, 982, 230, 340, 120)
)
write.csv(social_media_data,"Social_media_data.csv", row.names = FALSE)


# Creating Email capaign data

Email_campaign_data <- data.frame(
  campaign_no = 1:6,
  no_of_sent_emails = c(45200, 59000, 31001, 57880, 78330, 43900),
  generated_revenue = c(8900, 15900, 12000, 11000, 15000, 9500),
  response_rate = runif(6, min = 0.31, max = 0.45),
  Delivery_failure_rate = runif(6, min = 0.05, max = 0.11),
  ignorance_rate = runif(6, min = 0.3, max = 0.55)
)
write.csv(Email_campaign_data,"Email_Campaign_data.csv", row.names = FALSE)

# Creating Customer transaction data

Customer_transaction_data <- data.frame(
  Customer_id = 1:1000,
  sex = sample(c("Male", "Female"), 1000, replace = TRUE),
  No_of_products = sample(1:8, 1000, replace = TRUE),
  Amount_spent = sample(100:500, 1000, replace = TRUE),
  Location = sample(c("Ontario", "Alberta", "Quebec"), 1000, replace = TRUE),
  Payment_method = sample(c("credit", "debit", "Mastercard", "Pay by cash"), 1000, replace = TRUE)
  
)
write.csv(Customer_transaction_data,"Customer_transaction_data.csv", row.names = FALSE)
