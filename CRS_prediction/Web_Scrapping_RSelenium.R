# Install required packages if not already installed
install.packages('RSelenium')
install.packages('dplyr')
install.packages('rvest')

# Load the libraries
library(RSelenium)
library(dplyr)
library(rvest)

# Start an RSelenium server and browser
rD <- rsDriver(browser = "chrome", port = 4545L, verbose = FALSE)
remDr <- rD$client

# Navigate to the web page
url <- 'https://www.canada.ca/en/immigration-refugees-citizenship/corporate/mandate/policies-operational-instructions-agreements/ministerial-instructions/express-entry-rounds.html'
remDr$navigate(url)

# Allow the page to fully load
Sys.sleep(5)

# Initialize an empty list to store the data from all pages
Raw_data <- list()

# Find the total number of pages manually
total_pages <- 13  

# Loop through each page
for (page_num in 1:total_pages) {
  # Allow time for the table to load on each page
  Sys.sleep(3)
  
  # Get the page source and parse it
  page_source <- remDr$getPageSource()[[1]]
  webpage <- read_html(page_source)
  
  # Extract the table on the current page
  table_data <- webpage %>%
    html_element("table") %>%
    html_table(fill = TRUE)
  
  # Check the structure of the table data
  print(str(table_data))
  
  # Standardize column types (if necessary)
  # Convert all columns to character to avoid type mismatches
  table_data <- table_data %>% mutate(across(everything(), as.character))
  
  # Add the table data from this page to the list
  Raw_data[[page_num]] <- table_data
  
  # Print progress
  print(paste("Page", page_num, "scraped successfully."))
  
  # If not the last page, click the "Next" button to go to the next page
  if (page_num < total_pages) {
    next_button <- remDr$findElement(using = 'css selector', '.paginate_button.next')
    next_button$clickElement()
  }
}

# Combine all pages into one dataframe
final_data <- bind_rows(Raw_data)

# Save the combined data to a CSV file
write.csv(final_data, "Express_entry_data_Rselenium.csv", row.names = FALSE)

# Close the browser
remDr$close()
rD$server$stop()

print("Data successfully scraped and saved to express_entry_rounds_all_pages.csv.")
