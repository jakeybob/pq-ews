# looking at different text classification ideas

# https://tensorflow.rstudio.com/keras/articles/tutorial_basic_text_classification.html ####

library(keras)
library(dplyr)
library(magrittr)
library(ggplot2)
library(purrr)

imdb <- dataset_imdb(num_words = 10000)

c(train_data, train_labels) %<-% imdb$train
c(test_data, test_labels) %<-% imdb$test

word_index <- dataset_imdb_word_index()

paste0("Training entries: ", length(train_data), ", labels: ", length(train_labels))

# words in first review
length(train_data[[1]])

word_index_df <- data.frame(
  word = names(word_index),
  idx = unlist(word_index, use.names = FALSE),
  stringsAsFactors = FALSE
)

# The first indices are reserved  
word_index_df <- word_index_df %>% mutate(idx = idx + 3)
word_index_df <- word_index_df %>%
  add_row(word = "<PAD>", idx = 0)%>%
  add_row(word = "<START>", idx = 1)%>%
  add_row(word = "<UNK>", idx = 2)%>%
  add_row(word = "<UNUSED>", idx = 3)
word_index_df <- word_index_df %>% arrange(idx)

decode_review <- function(text){
  paste(map(text, function(number) word_index_df %>%
              filter(idx == number) %>%
              select(word) %>% 
              pull()),
        collapse = " ")
}

decode_review(train_data[[1]])


train_data <- pad_sequences(
  train_data,
  value = word_index_df %>% filter(word == "<PAD>") %>% select(idx) %>% pull(),
  padding = "post",
  maxlen = 256
)

test_data <- pad_sequences(
  test_data,
  value = word_index_df %>% filter(word == "<PAD>") %>% select(idx) %>% pull(),
  padding = "post",
  maxlen = 256
)

# input shape is the vocabulary count used for the movie reviews (10,000 words)
vocab_size <- 10000

model <- keras_model_sequential()
model %>% 
  layer_embedding(input_dim = vocab_size, output_dim = 16) %>%
  layer_global_average_pooling_1d() %>%
  layer_dense(units = 16, activation = "relu") %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% summary()

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)

x_val <- train_data[1:10000, ]
partial_x_train <- train_data[10001:nrow(train_data), ]

y_val <- train_labels[1:10000]
partial_y_train <- train_labels[10001:length(train_labels)]


history <- model %>% fit(
  partial_x_train,
  partial_y_train,
  epochs = 40,
  batch_size = 512,
  validation_data = list(x_val, y_val),
  verbose=1
)

results <- model %>% evaluate(test_data, test_labels)
results


#### https://cfss.uchicago.edu/text_classification.html ####

library(tidyverse)
library(tidytext)
library(stringr)
library(caret)
library(tm)
library(RTextTools)
library(randomForest)

set.seed(1234)
theme_set(theme_minimal())

# get USCongress data
data(USCongress, package = "RTextTools")

congress <- as_tibble(USCongress) %>%
    mutate(text = as.character(text))

congress_tokens <- congress %>%
    unnest_tokens(output = word, input = text) %>%
    # remove numbers
    filter(!str_detect(word, "^[0-9]*$")) %>%
    # remove stop words
    anti_join(stop_words) %>%
    # stem the words
    mutate(word = SnowballC::wordStem(word))

(congress_dtm <- congress_tokens %>%
    # get count of each token in each document
    count(ID, word) %>%
    # create a document-term matrix with all features and tf weighting
    cast_dtm(document = ID, term = word, value = n))

congress_dtm <- removeSparseTerms(congress_dtm, sparse = .99)

# exploratory
(congress_tfidf <- congress_tokens %>%
    count(major, word) %>%
    bind_tf_idf(term = word, document = major, n = n))

# sort the data frame and convert word to a factor column
plot_congress <- congress_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

# graph the top 10 tokens for 4 categories
plot_congress %>%
  filter(major %in% c(1, 2, 3, 6)) %>%
  mutate(major = factor(major, levels = c(1, 2, 3, 6),
                        labels = c("Macroeconomics", "Civil Rights",
                                   "Health", "Education"))) %>%
  group_by(major) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~major, scales = "free") +
  coord_flip()

# ~300 secs on linode, ~230s on windows laptop -- won't run on NSS server as R = v3.2

congress_rf_200 <- train(x = as.matrix(congress_dtm),
                       y = factor(congress$major),
                       method = "rf",
                       ntree = 200,
                       trControl = trainControl(method = "oob"))

