
#Installing packages
install.packages("rvest")
install.packages("httr")
install.packages("ggplot2")

#Loading libraries
library(httr)
library(rvest)
library(ggplot2)

# URL for the page
url <- "https://companiesmarketcap.com/cad/canada/largest-companies-in-canada-by-market-cap/"

response <- httr::GET(url)
print(response)

# Scrapping the data as text
html_data <- content(response, as = "text") 
html_data <- read_html(html_data)
print(html_data)

# Extracting company names
company_names <- html_data %>%
html_nodes("td.name-td .company-name") %>% html_text2()
print(company_names)

# Extract marketcap

market_cap <- html_data %>% html_nodes("td.td-right") %>% html_text2() 
print(market_cap)

# Data cleaning
market_cap <- gsub("[^C\\$0-9.B]", "", market_cap) 
market_cap <- market_cap[grepl("C\\$", market_cap)]
market_cap <- gsub("[B]","",market_cap)

# removng "C$" to make it numeric
market_cap <- gsub("[C\\$]", "", market_cap)
print(market_cap)


if (length(company_names) == length(market_cap)) {
      df <- data.frame(Companies = company_names,
                    Market_cap = market_cap)
      print(df)
   } else {
     print("There is a mismatch between the compnaies and market cap count")
   }

df$Market_cap <- as.numeric(df$Market_cap)
df

# Plotting with flipped axes
ggplot(df, aes(x = Market_cap, y = Companies)) +  # Flip x and y
  geom_segment(aes(x = 0, xend = Market_cap, y = Companies, yend = Companies), color = "blue") +
  geom_point(color = "red", size = 2) +
  theme_minimal() +
  labs(title = "Top 100 largest companies in Canada",
       x = "Market Cap (in billions)",
       y = "Companies") 
  
