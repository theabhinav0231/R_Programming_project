library(rtweet)
library(readr)
library(tidytext)
library(dplyr)
library(ggplot2)
library(stringr)
library(tidyr)

tweets <- read_csv("/cloud/project/climate_change_tweets.csv")
# View first few tweets
head(tweets$text)

# Unnest tokens (break into words)
tweet_words <- tweets %>%
  select(status_id, text) %>%
  unnest_tokens(word, text)

# Remove stop words
data("stop_words")
cleaned_tweets <- tweet_words %>%
  anti_join(stop_words, by = "word")

bing_sentiments <- cleaned_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE)

head(bing_sentiments)

sentiment_count <- bing_sentiments %>%
  count(sentiment)

ggplot(sentiment_count, aes(x = sentiment, y = n, fill = sentiment)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Sentiment Distribution of Tweets",
       x = "Sentiment", y = "Count")

top_words <- bing_sentiments %>%
  group_by(sentiment) %>%
  top_n(10, n) %>%
  ungroup() %>%
  arrange(sentiment, -n)

ggplot(top_words, aes(x = reorder(word, n), y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Top Words by Sentiment",
       x = "Words", y = "Frequency")