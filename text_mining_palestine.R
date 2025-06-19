
library(dplyr)
library(stringr)
library(readr)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(ggplot2)
library(gt)
library(plotly)
library(quanteda)
library(httr)
library(jsonlite)
library(lubridate)

#csv

news <- read_csv("final_selected_media.csv")

# Word Frequency
## Not grouped by media

# Load English stop words
data("stop_words")

# Global word frequency (not grouped)
word_counts <- news %>%
  ungroup() %>%  # Remove any previous grouping
  mutate(title = str_to_lower(title)) %>%  # Convert to lowercase
  mutate(title = str_remove_all(title, "[^[:alnum:]\\s]")) %>%  # Remove punctuation
  mutate(title = str_remove_all(title, "\\d+")) %>%  # Remove numbers
  unnest_tokens(word, title) %>%  # Tokenize into words
  anti_join(stop_words, by = "word") %>%  # Remove stopwords
  count(word, sort = TRUE)  # Count word frequency

#Wordcloud
wordcloud::wordcloud(
  words = word_counts$word,
  freq = word_counts$n,
  min.freq = 5,             # You can adjust this
  max.words = 100,          # Limit to top 100
  random.order = FALSE,
  scale = c(3, 0.8),
  colors = brewer.pal(8, "Dark2")
)

## Grouped by media

# Define custom words to exclude
excluded_words <- c("israel", "israels", "gaza")

# Clean text
cleaned_titles <- news %>%
  mutate(title = str_to_lower(title)) %>%  # Convert to lowercase
  mutate(title = str_remove_all(title, "[^[:alnum:]\\s]")) %>%  # Remove punctuation
  mutate(title = str_remove_all(title, "\\d+"))  # Remove numbers

# Tokenize and count words by media
word_counts_by_media <- cleaned_titles %>%
  unnest_tokens(word, title) %>%
  filter(!word %in% excluded_words) %>%                   # Remove custom exclusions
  anti_join(stop_words, by = "word") %>%
  count(media_name, word, sort = TRUE)                    # Count by media

# View result
word_counts_by_media

# Select top 5 words per media
top_words <- word_counts_by_media %>%
  group_by(media_name) %>%
  slice_max(n, n = 5) %>%
  ungroup()

# Plot
ggplot(top_words, aes(x = reorder_within(word, n, media_name), y = n, fill = media_name)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  facet_wrap(~ media_name, scales = "free_y") +
  coord_flip() +
  labs(title = "Top 5 Most Frequent Words per Media Outlet",
       x = NULL, y = "Frequency") +
  theme_minimal(base_size = 11)


# TF-IDF

tf_idf_filter <- c(
  # Conflict actors and identity terms
  "hamas", "idf", "israel", "israeli", "palestine", "palestinian", "palestinians",
  
  # Military actions and framing
  "strike", "strikes", "raid", "raids", "bomb", "bombing", "airstrike", "airstrikes",
  "shelling", "offensive", "assault", "military", "rocket", "rockets",
  "gunfire", "target", "targeted", "operation", "operations", "missile", "missiles",
  "soldier", "soldiers", "forces", "counterattack", "retaliation",
  
  # Violence and casualties
  "killed", "kill", "dead", "deaths", "wounded", "injured", "massacre", "casualties", "bodies",
  "hospital", "hospitals", "children", "civilians", "refugees", "displaced",
  
  # Hostage/terror framing
  "hostage", "hostages", "terrorist", "terrorists", "extremist", "militant", "militants",
  "radical", "jihad",
  
  # Peace/diplomacy framing
  "ceasefire", "truce", "peace", "negotiation", "negotiations", "talks", "agreement",
  "support", "international", "biden", "un", "united nations", "eu", "aid", "deal", "mediation",
  "protest", "protests",
  
  # Settlements / occupation
  "occupation", "occupied", "settler", "settlers"
)

tidy_titles2 <- tidy_titles %>%
  filter(word %in% tf_idf_filter)

# Count word frequency by media
word_counts_by_media2 <- tidy_titles2 %>%
  count(media_name, word, sort = TRUE)

# Compute tf-idf
tf_idf_words2 <- word_counts_by_media2 %>%
  bind_tf_idf(word, media_name, n) %>%
  arrange(desc(tf_idf))

# View top tf-idf words
tf_idf_words2 %>%
  group_by(media_name) %>%
  slice_max(tf_idf, n = 5) %>%  # Top 5 words per media
  ungroup() %>%
  mutate(word = reorder_within(word, tf_idf, media_name)) %>%
  ggplot(aes(x = word, y = tf_idf, fill = media_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ media_name, scales = "free_y") +
  labs(
    title = "Top TF-IDF Words by Media Outlet",
    x = "Word",
    y = "TF-IDF Score"
  )


# Bigrams

# Tokenize into bigrams
bigrams <- cleaned_titles %>%
  unnest_tokens(bigram, title, token = "ngrams", n = 2)

# Filter out stopwords from both parts of the bigram
bigrams_filtered <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ")

# Show top bigrams
bigrams_filtered %>% slice_max(n, n = 20)

# Bigrams per media outlet
# Clean and tokenize into bigrams
bigrams_by_media <- news %>%
  mutate(title = str_to_lower(title)) %>%  # Lowercase
  mutate(title = str_remove_all(title, "[^[:alnum:]\\s]")) %>%  # Remove punctuation
  mutate(title = str_remove_all(title, "\\d+")) %>%  # Remove numbers
  unnest_tokens(bigram, title, token = "ngrams", n = 2) %>%
  separate(bigram, into = c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(media_name, bigram, sort = TRUE)

# Get top 5 bigrams for each media
top_bigrams_per_media <- bigrams_by_media %>%
  group_by(media_name) %>%
  slice_max(n, n = 5) %>%
  ungroup()

# Plot: Top 5 bigrams per media (with facets)
top_bigrams_per_media %>%
  mutate(bigram = reorder_within(bigram, n, media_name)) %>%
  ggplot(aes(x = bigram, y = n, fill = media_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ media_name, scales = "free_y") +
  labs(
    title = "Top 5 Most Common Bigrams per Media Outlet",
    x = "Bigram",
    y = "Frequency"
  )


# Bigrams filtered by keywords

bigrams_filtered2 <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word)

###Palestinian bigrams

palestinian_terms <- c("palestinian", "palestinians", "palestine", "hamas")

#Filter bigrams where one word matches palestinian terms
bigrams_palestinian <- bigrams_filtered2 %>%
  filter(word1 %in% palestinian_terms | word2 %in% palestinian_terms) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ")

#View results
bigrams_palestinian %>% slice_max(n, n = 20)

###Israeli bigrams

israel_terms <- c("israel", "israeli", "idf")

#Filter bigrams where one word matches israel terms
bigrams_israel <- bigrams_filtered2 %>%
  filter(word1 %in% israel_terms | word2 %in% israel_terms) %>%
  count(word1, word2, sort = TRUE) %>%
  unite(bigram, word1, word2, sep = " ")

#View results
bigrams_israel %>% slice_max(n, n = 20)

# Create a combined dataframe with a label
bigrams_plot <- bind_rows(
  bigrams_palestinian %>%
    slice_max(n, n = 10) %>%
    mutate(group = "Palestinian"),
  
  bigrams_israel %>%
    slice_max(n, n = 10) %>%
    mutate(group = "Israeli")
)

# Order bigrams inside groups
bigrams_plot <- bigrams_plot %>%
  mutate(bigram = reorder_within(bigram, n, group))

# Plot
bigrams_plot %>%
  ggplot(aes(x = bigram, y = n, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(
    title = "Top Bigrams Referencing Israeli vs Palestinian Actors",
    x = "Bigram",
    y = "Frequency"
  ) +
  theme_minimal()


# Temporal evolution of keywords

# Ensure dates are in proper Date format
news <- news %>%
  mutate(date = as.Date(publish_date))

# Define keywords of interest
keywords <- c("palestinian", "palestine", "attack",
              "gaza", "israel", "israeli",
              "hamas", "idf", "children", "terrorists")

# Tokenize and count keyword frequencies by date
word_trends <- news %>%
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word") %>%
  filter(word %in% keywords) %>%
  mutate(week = floor_date(date, unit = "week")) %>%  
  count(week, word)

# Static plot
p <- ggplot(word_trends, aes(x = week, y = n, color = word)) +
  geom_line(size = 1) +
  labs(
    title = "Temporal Evolution of Selected Keywords",
    x = "Date",
    y = "Frequency",
    color = "Keyword"
  ) +
  theme_minimal()

# Convert to interactive plot
interactive_plot <- ggplotly(p)

# Show interactive plot
interactive_plot


# Framing

#Establish a framing dictionary

framing_dict <- list(
  violence = c("attack", "attacks", "strike", "strikes", "clash", "clashes", "bombing", "bombings",
               "raid", "raids", "assault", "shelling", "gunfire", "gunmen", "killed", "killing",
               "massacre", "dead", "bomb", "destruction", "genocide", "ethnic cleansing", "occupation",
               "invasion"),
  victims = c("children", "child", "civilians", "wounded", "injured", "hospital", "hospitals",
              "death", "school", "schools", "deaths", "casualties", "bodies", "refugees",
              "displaced", "families", "residents", "dead", "civilian", "occupied", "settler",
              "settlers", "suffering", "devastation", "starvation", "hunger", "residential"),
  terrorism = c("terrorist", "terrorists", "terror", "hamas", "jihad", "militant", "militants",
                "extremist", "extremists", "radical", "radicals", "rocket", "rockets", "hostage",
                "hostages"),
  military = c("idf", "airstrike", "airstrikes", "soldier", "soldiers", "defense",
               "operation", "operations", "military", "counterattack", "retaliation",
               "strike", "strikes", "forces", "missile", "missiles", "target", "targeted"),
  diplomacy = c("ceasefire", "peace", "negotiation", "negotiations", "talks", "agreement", "truce",
                "international", "biden", "un", "united nations", "eu", "aid", "support", "mediation",
                "deal", "rally", "embassy", "humanitarian")
)


# Tokenize titles
tokens <- news %>%
  mutate(title = str_to_lower(title)) %>%
  mutate(title = str_remove_all(title, "[^[:alnum:]\\s]")) %>%
  unnest_tokens(word, title)

# Match words to dictionary
framing_counts <- map_df(names(framing_dict), function(category) {
  words <- framing_dict[[category]]
  tokens %>%
    filter(word %in% words) %>%
    count(media_name, name = "n") %>%
    mutate(category = category)
})

# Spread for proportion analysis
framing_wide <- framing_counts %>%
  group_by(media_name, category) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(media_name) %>%
  mutate(total = sum(n),
         proportion = n / total) %>%
  ungroup()

# View proportions
framing_wide

# Create percentage labels for plotting
framing_wide <- framing_wide %>%
  mutate(label = paste0(round(proportion * 100), "%"))

#Plot
framing_wide %>%
  ggplot(aes(x = reorder(media_name, proportion), y = proportion, fill = category)) +
  geom_col(position = "fill") +
  geom_text(aes(label = label),
            position = position_fill(vjust = 0.5),
            size = 3, color = "white") +
  coord_flip() +
  labs(
    title = "Framing Proportions by Media Outlet",
    x = "Media",
    y = "Proportion",
    fill = "Framing Category"
  ) +
  theme_minimal()


# KWIC

# Step 1: Add unique doc_id
news_kwic <- news %>%
  mutate(doc_id = paste0("doc_", row_number()))

# Step 2: Create corpus with doc_id and media_name as metadata
corp <- corpus(news_kwic, text_field = "title", docid_field = "doc_id")
docvars(corp, "media_name") <- news_kwic$media_name

# Step 3: Tokenize the corpus
toks <- tokens(corp)

# Step 4: Run KWIC on tokenized corpus
kwic_terrorist <- kwic(toks, pattern = "terrorist", window = 5)

# Step 5: View results
head(kwic_terrorist, 20)

# Convert KWIC result to a regular data frame
kwic_df <- as.data.frame(kwic_terrorist)

# Combine with media_name
kwic_table <- kwic_df %>%
  select(docname, pre, keyword, post) %>%
  rename(Media = docname, Before = pre, Term = keyword, After = post)

kwic_table



# Sentiment Analysis

tokenized <- news %>%
  mutate(title = str_to_lower(title),                          # Lowercase
         title = str_remove_all(title, "[^[:alnum:]\\s]")) %>% # Remove punctuation
  unnest_tokens(word, title) %>%
  anti_join(stop_words, by = "word")                           # Remove stopwords

# AFINN lexicon
# Load AFINN lexicon
afinn <- get_sentiments("afinn")

# Tokenize and join with AFINN
afinn_sentiment <- tokenized %>%
  inner_join(afinn, by = "word") %>%
  group_by(media_name) %>%
  summarise(afinn_score = sum(value, na.rm = TRUE)) %>%
  replace_na(list(afinn_score = 0))

# Plot AFINN score per media
afinn_sentiment %>%
  ggplot(aes(x = reorder(media_name, afinn_score), y = afinn_score)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(
    title = "AFINN Sentiment Score by Media",
    x = "Media",
    y = "AFINN Score"
  ) +
  theme_minimal()

#NRC lexicon
# Load NRC lexicon
nrc <- get_sentiments("nrc")

# Select only desired emotions
selected_emotions <- c("anger", "fear", "sadness", "trust")

# Tokenize and join with NRC, filtering for selected emotions
nrc_sentiment_filtered <- tokenized %>%
  inner_join(nrc, by = "word") %>%
  filter(sentiment %in% selected_emotions) %>%  # Keep only selected sentiments
  count(media_name, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0)

# Reshape and plot sentiment counts
nrc_sentiment_filtered %>%
  pivot_longer(cols = -media_name, names_to = "sentiment", values_to = "count") %>%
  ggplot(aes(x = reorder(media_name, count), y = count, fill = sentiment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  facet_wrap(~ sentiment, scales = "free_y") +
  labs(
    title = "Selected NRC Emotions by Media Outlet",
    x = "Media",
    y = "Word Count",
    fill = "Emotion"
  ) +
  theme_minimal()


# Framing score
# Step 1: create a dictionary of terms to avoid the inclusion of headlines related to economy, diplomacy, etc.

conflict_terms <- c(
  # Actors / entities
  "hamas", "idf", "israel", "palestine", "palestinian", "palestinians", "israeli",
  
  # Military action
  "strike", "strikes", "airstrike", "airstrikes", "raid", "raids", "bomb", "bombing", "shelling", 
  "clash", "clashes", "offensive", "assault", "military", "rocket", "rockets", "fire", "gunfire",
  "occupation", "settler", "settlers", "occupied",
  
  # Casualties and victims
  "killed", "dead", "deaths", "wounded", "injured", "casualties", "victims", "bodies", "hospital", "hospitals",
  "massacre", "kill",
  
  # Hostage situations
  "hostage", "hostages", "abduction", "kidnapping",
  
  # Conflict framing
  "ceasefire", "war", "conflict", "truce", "escalation", "invasion", "resistance", "terrorist", "terrorists"
)


# Filter headlines containing at least one conflict-related term
news_conflict <- news %>%
  filter(str_detect(str_to_lower(title), str_c(conflict_terms, collapse = "|"))) %>%
  mutate(sentiment_score = NA_character_)

# Step 2: set the function to annotate text using the OpenAI API Code
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


# Step 3: load your new csv and check how many observations you have for each value

news2 <- read.csv("news_conflict_progress.csv")

table(news2$sentiment_score, useNA = "ifany")

# Step 4: Framing score by media outlet
#Mean by media outlet

media_scores <- news2 %>%
  group_by(media_name) %>%
  summarise(
    mean_score = mean(sentiment_score, na.rm = TRUE),
    n = n(),
    valid = sum(!is.na(sentiment_score))
  ) %>%
  arrange(desc(mean_score))

#Plot
ggplot(media_scores, aes(x = reorder(media_name, mean_score), y = mean_score)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = valid), hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Average Framing Score by Media Outlet",
    subtitle = "0 = Dovish (humanitarian) | 10 = Hawkish (military emphasis)",
    x = "Media outlet",
    y = "Mean framing score",
    caption = "Number of valid headlines shown as labels"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10)
  ) +
  ylim(0, 10)  # Ensures full scale


#UMAP
#Load UMAP csv, generated with code in python
df_umap <- read_csv("umap_with_sentiment_and_media.csv")

# Make sure sentiment score is numeric
df_umap$sentiment_score <- as.numeric(df_umap2$sentiment_score)

ggplot(df_umap, aes(x = umap_x, y = umap_y, color = sentiment_score)) +
  geom_point(size = 1.5, alpha = 0.8) +
  scale_color_gradient(low = "blue", high = "red", na.value = "gray") +
  facet_wrap(~ media_name) +
  labs(
    title = "UMAP Projection Faceted by Media",
    x = "UMAP 1", y = "UMAP 2", color = "Framing Score"
  ) +
  theme_minimal()


# Framing with framing score (excluding NAs)
df_umap %>%
  mutate(framing_cat = case_when(
    sentiment_score <= 3 ~ "Dovish",
    sentiment_score <= 6 ~ "Neutral",
    sentiment_score > 6 ~ "Hawkish",
    TRUE ~ NA_character_
  )) %>%
  count(media_name, framing_cat) %>%
  group_by(media_name) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = reorder(media_name, -percentage), y = percentage, fill = framing_cat)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = scales::percent(percentage, accuracy = 1)),
    position = position_fill(vjust = 0.5),
    size = 3,
    color = "white"
  ) +
  coord_flip() +
  labs(
    title = "Framing Distribution by Media Outlet",
    x = "Media Outlet",
    y = "Proportion of Headlines",
    fill = "Framing Category"
  ) +
  theme_minimal()

# Weekly evolution of framing score

news2 %>%
  mutate(date = as.Date(publish_date)) %>%
  mutate(week = floor_date(date, unit = "week")) %>%  # group by date
  group_by(week) %>%
  summarise(mean_score = mean(sentiment_score, na.rm = TRUE)) %>%
  ggplot(aes(x = week, y = mean_score)) +
  geom_line(color = "steelblue", linewidth = 1) +
  labs(
    title = "Framing Score in Headlines, Weekly Evolution",
    x = "Week", y = "Mean Framing Score (0 = Dovish, 10 = Hawkish)"
  ) +
  theme_minimal()
