
## Author:    Steven Ponce
## Date:      2025-01-03

# Load Required Libraries
library(rvest)
library(polite)
library(tidyverse)

# Define User-Agent
user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/93.0.4577.82 Safari/537.36"

# Define Base URL for LibraryThing Reviews
base_url <- "https://www.librarything.com/work/5864/reviews"

# Start a polite session
session <- polite::bow(base_url, user_agent = user_agent)

# Initialize an empty dataframe to store reviews
librarything_reviews <- tibble()

# Loop through pages of reviews (adjust range as needed)
for (page in 1:3) {
  # Print progress
  print(paste("Scraping page:", page))
  
  # Construct the URL for the current page
  url <- paste0(base_url, "?page=", page)
  
  # Update the session with the new URL
  current_session <- polite::nod(session, path = url)
  
  # Scrape the webpage
  page_content <- polite::scrape(current_session)
  
  # Extract Reviewer Names
  reviewer_names <- page_content |>    
    html_nodes("span.controlItems a") |> # Selector for reviewer names
    html_text(trim = TRUE) |>
    .[. != ""] # Remove empty strings
  
  # Extract Review Dates
  review_dates <- page_content |>
    html_nodes("span.controlItems") |> # Selector for the entire control items section
    html_text(trim = TRUE) |>
    str_extract("\\|\\s*(\\w+\\s\\d{1,2},\\s\\d{4})\\s*\\|") |> # Extract date text
    str_remove_all("\\|") |>
    trimws() # Remove leading/trailing whitespace
  
  # Extract Review Ratings (both `ssx` and numeric values)
  review_ratings <- page_content |>
    html_nodes("span.rating img") |>  # Target the <img> tag within the rating span
    html_attr("src") |>              # Get the src attribute (URL)
    str_extract("ss\\d+") |>         # Extract the file name (e.g., "ss8")
    tolower()                         # Ensure lowercase consistency
  
  # Map `ssx_rating` values to `numeric_rating`
  rating_values <- review_ratings |>
    str_remove("ss") |>               # Remove the "ss" prefix
    as.numeric() |>                   # Convert to numeric
    {                                  # Use curly braces to pass the vector explicitly
      case_when(
        . == 0  ~ 0,
        . == 1  ~ 0.5,
        . == 2  ~ 1,
        . == 3  ~ 1.5,
        . == 4  ~ 2,
        . == 5  ~ 2.5,
        . == 6  ~ 3,
        . == 7  ~ 3.5,
        . == 8  ~ 4,
        . == 9  ~ 4.5,
        . == 10 ~ 5,
        TRUE ~ NA_real_                # Handle unexpected values
      )
    }
  
  # Extract Review Texts
  review_texts <- page_content |>
    html_nodes("div.commentText") |> # Selector for review text
    html_text(trim = TRUE)
  
  # Debugging: Print lengths
  print(paste("Number of Reviewer Names:", length(reviewer_names)))
  print(paste("Number of Review Dates:", length(review_dates)))
  print(paste("Number of Review Texts:", length(review_texts)))
  
  # Align lengths by padding shorter vectors with NA
  max_length <- max(length(reviewer_names), length(review_dates), length(review_texts))
  reviewer_names <- c(reviewer_names, rep(NA, max_length - length(reviewer_names)))
  review_dates <- c(review_dates, rep(NA, max_length - length(review_dates)))
  review_texts <- c(review_texts, rep(NA, max_length - length(review_texts)))
  review_ratings <- c(review_ratings, rep(NA, max_length - length(review_ratings)))
  rating_values <- c(rating_values, rep(NA, max_length - length(rating_values)))
  
  # Combine extracted data into a dataframe
  temp_df <- tibble(
    reviewer_name = reviewer_names,
    review_date = review_dates,
    review_text = review_texts,      # Add `review_text` column
    ssx_rating = review_ratings,     # Add `ssx` column
    numeric_rating = rating_values   # Add numeric ratings
  )
  
  # Append the temporary dataframe to the main dataframe
  librarything_reviews <- bind_rows(librarything_reviews, temp_df)
  
  # Pause between requests to avoid overloading the server
  Sys.sleep(3)
}

# Remove duplicate rows
librarything_reviews <- librarything_reviews |>
  distinct() |> 
  drop_na(numeric_rating, review_text) |> 
  select(-ssx_rating) |> 
  mutate(source = "librarything")

# Print a glimpse of the cleaned data
glimpse(librarything_reviews)

# Save the cleaned data to a CSV file
write.csv(librarything_reviews, 
          "2025/01_Jan/librarything_reviews_full.csv", 
          row.names = FALSE)
