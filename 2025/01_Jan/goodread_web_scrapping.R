
## Author:    Steven Ponce
## Date:      2025-01-03

# Load Required Libraries
library(rvest)
library(polite)
library(dplyr)
library(stringr)

# Define User-Agent
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.82 Safari/537.36"

# Define URLs for the first page
urls <- c(
  "https://www.goodreads.com/book/show/320.One_Hundred_Years_of_Solitude?from_search=true&from_srp=true&qid=1GomKIPmtF&rank=1"
)

# Initialize an empty dataframe to store data
goodreads_reviews <- tibble()

# Loop Through URLs
for (url in urls) {
  print(paste("Scraping URL:", url))
  
  # Start a polite session
  session <- polite::bow(url, user_agent = user_agent)
  
  # Scrape the webpage
  page_content <- polite::scrape(session)  
  
  # Extract all ReviewCards
  review_cards <- page_content |> html_nodes("article.ReviewCard") # Class for review cards
  
  # Loop through each ReviewCard to extract details
  for (card in review_cards) {
    # Extract Reviewer Name
    reviewer_name <- card |>
      html_node(".ReviewerProfile__name") |> # Selector for reviewer name
      html_text(trim = TRUE)
    
    # Extract Star Rating
    star_rating <- card |>
      html_node("span.RatingStars__small") |> # Selector for star rating
      html_attr("aria-label") |>
      str_extract("Rating \\d out of \\d") |>
      str_extract("\\d") |>
      as.integer()
    
    # Extract Review Date
    review_date <- card |>
      html_node("span.Text__body3 a") |> # Selector for review date
      html_text(trim = TRUE)
    
    # Extract Review Text
    review_text <- card |>
      html_node("div.TruncatedContent__text span.Formatted") |> # Selector for review text
      html_text(trim = TRUE)
    
    # Combine data into a temporary dataframe
    temp_df <- tibble(
      reviewer_name = reviewer_name,
      star_rating = star_rating,
      review_date = review_date,
      review_text = review_text
    )
    
    # Append the temporary dataframe to the final dataframe
    goodreads_reviews <- bind_rows(goodreads_reviews, temp_df)
  }
  
  # Pause between requests to avoid overloading the server
  Sys.sleep(2)
}

# Print a glimpse of the scraped data
glimpse(goodreads_reviews)

goodreads_reviews <- goodreads_reviews |> 
  mutate(source = "goodreads")

# Save the scraped data to a CSV file for inspection
write.csv(goodreads_reviews, 
          "2025/01_Jan/goodreads_reviews_full.csv", 
          row.names = FALSE)
