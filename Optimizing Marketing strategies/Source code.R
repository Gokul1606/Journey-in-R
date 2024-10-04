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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Data collection and Preprocessing
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read the csv file and storing it in data frames to proceed for the analysis

Web_Source_final <- read.csv("C:/Users/Owner/Documents/Assignment_1/Website_data.csv", stringsAsFactors = FALSE)
Social_media_data_final <- read.csv("C:/Users/Owner/Documents/Assignment_1/Social_media_data.csv", stringsAsFactors = FALSE)
Email_campaign_data_final <- read.csv("C:/Users/Owner/Documents/Assignment_1/Email_Campaign_data.csv", stringsAsFactors = FALSE)
Customer_transaction_data_final <- read.csv("C:/Users/Owner/Documents/Assignment_1/Customer_transaction_data.csv", stringsAsFactors = FALSE)

# Data Pre-processing steps

#----For Web_source data------------------------------------------------
 # Removing duplicates
Web_Source_final <- Web_Source_final %>% distinct()

# Convert Device_type and Old_VS_New to factors for linear models to work bett
Web_Source_final$Device_type <- as.factor(Web_Source_final$Device_type)
Web_Source_final$Old_VS_New <- as.factor(Web_Source_final$Old_VS_New)

# Standarizing the column names
names(Web_Source_final) <- tools::toTitleCase(names(Web_Source_final))

#Converting rate columns to percentage
Web_Source_final$Exit_rate <- Web_Source_final$Exit_rate * 100
Web_Source_final$Potential_views <- Web_Source_final$Potential_views * 100

#Rounding the decimals to 1 digit
Web_Source_final$Exit_rate <- round(Web_Source_final$Exit_rate,1)
Web_Source_final$Potential_views <- round(Web_Source_final$Potential_views,1)
print(Web_Source_final)

#----For social_media_data----------------------------------------------
 # Removing duplicates
Social_media_data_final <- Social_media_data_final %>% distinct()

 # Standarizing the column names
names(Social_media_data_final) <- tools::toTitleCase(names(Social_media_data_final))
print(Social_media_data_final)


#----For Email campaign data--------------------------------------------
# Removing duplicates
Email_campaign_data_final <- Email_campaign_data_final %>% distinct()
str(Email_campaign_data_final)

#Converting rates to percentage and rounding it to 2 digits
Email_campaign_data_final$response_rate = round(Email_campaign_data_final$response_rate * 100, 2)
Email_campaign_data_final$Delivery_failure_rate = round(Email_campaign_data_final$Delivery_failure_rate * 100, 2)
Email_campaign_data_final$ignorance_rate = round(Email_campaign_data_final$ignorance_rate * 100, 2)

# Converting campaign no column as factor
Email_campaign_data_final$campaign_no = as.factor(Email_campaign_data_final$campaign_no)
print(Email_campaign_data_final)

#---For Customer transaction data---------------------------------------
# Removing duplicates
Customer_transaction_data_final <- Customer_transaction_data_final %>% distinct()
str(Customer_transaction_data_final)

#Converting sex, location, and payment method as categorical
Customer_transaction_data_final$sex = as.factor(Customer_transaction_data_final$sex)
Customer_transaction_data_final$Location = as.factor(Customer_transaction_data_final$Location)
Customer_transaction_data_final$Payment_method = as.factor(Customer_transaction_data_final$Payment_method)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Exploratory Data Analysis (EDA)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

print(Web_Source_final)
summary(Web_Source_final)
print(Social_media_data_final)
summary(Social_media_data_final)
print(Email_campaign_data_final)
summary(Email_campaign_data_final)
print(Customer_transaction_data_final)
summary(Customer_transaction_data_final)

# Visualizing the no of users for each device types
ggplot(Web_Source_final, aes(x = Device_type)) + geom_bar(fill = "grey") + 
      labs(title = "Different channels for ABC Corporation", x = "Types of Devices", y = "Number of users") + theme_minimal()

# Visualizing the Number of old and new users
ggplot(Web_Source_final, aes(x = Old_VS_New, y = No_of_users, fill = Old_VS_New)) +
  geom_boxplot() +
  labs(title = "Old vs New Users", x = "User Type", y = "Number of Users")

# Visualize the Exit rates of the users by their device types
ggplot(Web_Source_final, aes(x = Device_type, y = Exit_rate, fill = Device_type)) +
  geom_boxplot() +
  labs(title = "Exit Rate by Device Type", x = "Device Type", y = "Exit Rate")

# Visualize the number of followers by different platforms
ggplot(Social_media_data_final, aes(x = Websites, y = No_of_followers)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Followers by Platform", x = "Platform", y = "Number of Followers")

# Visualize Social media interactions by Platform
ggplot(Social_media_data_final, aes(x = Websites, y = No_of_interactions, fill = Websites)) +
  geom_bar(stat = "identity") +
  labs(title = "Social Media Interactions by Platform", x = "Platform", y = "Interactions")

#Visualize Email campaign response rate
ggplot(Email_campaign_data_final, aes(x = campaign_no, y = response_rate, fill = campaign_no)) +
  geom_bar(stat = "identity") +
  labs(title = "Email Campaign Response Rate", x = "Campaign no", y = "Response Rate")

#Visualizing the amount spent by location
ggplot(Customer_transaction_data_final, aes(x = Location, y = Amount_spent)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Amount Spent by Location", x = "Location", y = "Amount Spent")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Customer Segmentation and Profiling
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#  Select relevant features for clustering (No_of_products, Amount_spent)
customer_features <- Customer_transaction_data_final %>% select(No_of_products, Amount_spent)

# K-Means clustering
set.seed(123)
customer_clusters <- kmeans(scale(customer_features), centers = 3)

# Add cluster labels to original data
Customer_transaction_data_final$segment <- as.factor(customer_clusters$cluster)

#Profile customer segments
segment_summary <- Customer_transaction_data_final %>%
  group_by(segment) %>%
  summarise(
    avg_no_of_products = mean(No_of_products),
    avg_spent = mean(Amount_spent)
  )

# Print the segment summary
print(segment_summary)

# Visualize customer segmentation
ggplot(Customer_transaction_data_final, aes(x = Amount_spent, y = No_of_products, color = segment)) +
  geom_point() +
  labs(title = "Customer Segmentation Based on Products and Spending", x = "Amount Spent", y = "Number of Products")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Predictive Modeling and Forecasting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Predictive modeling using linear regression to predict revenue generated from new email campaigns

Prediction_model <- lm(generated_revenue ~ no_of_sent_emails + response_rate + Delivery_failure_rate, data = Email_campaign_data_final)
New_Email_campaigns <- data.frame(no_of_sent_emails = 25609, response_rate = 28, Delivery_failure_rate = 5.2)
Email_campaign_prediction <- round(predict(Prediction_model, New_Email_campaigns), 0)

print(paste("The new email campaign can generate the revenue of about:$",Email_campaign_prediction))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: A/B Testing and Optimization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Conducted the A/B testing on email campaigns to find the winning email campaign set
campaign_set1 <- Email_campaign_data_final$generated_revenue[1:3]
campaign_set2 <- Email_campaign_data_final$generated_revenue[4:6]

# Perform a t-test to compare the means of the two campaigns
t_test_result <- t.test(campaign_set1, campaign_set2)
print(t_test_result)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Reporting and Visualization
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Define UI for the app
ui <- fluidPage(
  titlePanel("Revenue Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("emails", "Number of Emails Sent:", value = 1000, min = 0),
      numericInput("response", "Response Rate (%):", value = 10, min = 0, max = 100),
      numericInput("failure", "Delivery Failure Rate (%):", value = 5, min = 0, max = 100),
      actionButton("predict", "Predict Revenue")
    ),
    
    mainPanel(
      textOutput("revenue")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  # Define the coefficients of the linear model
  beta_0 <- 1000  # Intercept
  beta_1 <- 2     # Coefficient for no_of_emails_sent
  beta_2 <- 50    # Coefficient for response_rate
  beta_3 <- -30   # Coefficient for delivery_failure_rate
  
  observeEvent(input$predict, {
    # Calculate the revenue based on the inputs and the linear model
    revenue <- beta_0 + beta_1 * input$emails + beta_2 * (input$response / 100) + beta_3 * (input$failure / 100)
    
    # Display the revenue
    output$revenue <- renderText({
      paste("Estimated Revenue: $", round(revenue, 2))
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)











