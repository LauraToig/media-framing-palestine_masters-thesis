
# OPENAI API CODE
# Code to develop the framing score

library(httr)
library(jsonlite)
library(readr)

# Set your API key (make sure it's saved safely)
Sys.setenv(OPENAI_API_KEY = "your_api_key")

# Define the annotation function
get_sentiment_score <- function(headline) {
  prompt <- paste0(
    "Rate the following news headline from 0 to 10, where:\n",
    "0 = focus on humanitarian issues or protection of civilians,\n",
    "5 = neutral or balanced perspective,\n",
    "10 = focus on military victory, warlike tone or targeting Hamas.\n",
    "If the focus is unclear or unrelated to the conflict, return NA.\n\n",
    "Headline: ", headline, "\nAnswer:"
  )
  
  response <- tryCatch({
    POST(
      url = "https://api.openai.com/v1/chat/completions",
      add_headers(
        Authorization = paste("Bearer", Sys.getenv("OPENAI_API_KEY")),
        `Content-Type` = "application/json"
      ),
      body = toJSON(list(
        model = "gpt-3.5-turbo",
        messages = list(list(role = "user", content = prompt)),
        temperature = 0.2
      ), auto_unbox = TRUE),
      timeout(20)
    )
  }, error = function(e) {
    message("API request failed: ", e$message)
    return("ERROR")
  })
  
  if (is.null(response)) return("ERROR")
  
  content_data <- tryCatch({
    content(response, as = "parsed")
  }, error = function(e) {
    message("Failed to parse response: ", e$message)
    return(NULL)
  })
  
  # Handle malformed or unexpected content
  if (is.null(content_data)) {
    return("ERROR")
  }
  
  # Check if response contains expected structure
  if (!is.list(content_data) || is.null(content_data$choices)) {
    message("Unexpected API response format")
    return("ERROR")
  }
  
  
  result <- tryCatch({
    content_data$choices[[1]]$message$content
  }, error = function(e) {
    message("No content in response.")
    return("NA")
  })
  
  return(trimws(result))
}


# Do batches periodically to save your progress

news_conflict$sentiment_score <- as.character(news_conflict$sentiment_score)

# Parameters
batch_size <- 100
total_rows <- nrow(news_conflict)

# Start batch loop
for (start_idx in seq(1, total_rows, by = batch_size)) {
  end_idx <- min(start_idx + batch_size - 1, total_rows)
  batch <- news_conflict[start_idx:end_idx, ]
  
  # Ensure the sentiment_score column exists in batch
  batch$sentiment_score <- news_conflict$sentiment_score[start_idx:end_idx]
  
  message("Processing headlines ", start_idx, " to ", end_idx)
  
  for (i in seq_len(nrow(batch))) {
    if (is.na(batch$sentiment_score[i])) {
      headline <- batch$title[i]
      score <- get_sentiment_score(headline)
      batch$sentiment_score[i] <- score
      Sys.sleep(1.1)
    }
  }
  
  # Update the main dataframe with results
  news_conflict[start_idx:end_idx, "sentiment_score"] <- batch$sentiment_score
  
  # Save progress after each batch
  write_csv(news_conflict, "news_conflict_progress.csv")
}