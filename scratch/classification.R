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



#### https://www.springboard.com/blog/machine-learning-with-r/ ####

library("kernlab") 
library("caret") 
library("tm") 
library("dplyr") 
library("splitstackshape")
library("e1071")

setwd("scratch")
# note: need to add Training and Test folders from 
# https://github.com/Rogerh91/Springboard-Blog-Tutorials/tree/master/Machine%20Learning%20with%20R%20Tutorial

# Step 2. Create your document term matrices for the training data.
train <- VCorpus(DirSource("Training", encoding = "UTF-8"), readerControl=list(language="English"))
train <- tm_map(train, content_transformer(stripWhitespace))
train <- tm_map(train, content_transformer(tolower))
train <- tm_map(train, content_transformer(removeNumbers))
train <- tm_map(train, content_transformer(removePunctuation))

train.dtm <- as.matrix(DocumentTermMatrix(train, control=list(wordLengths=c(1,Inf))))

# Step 3. Repeat steps 1 & 2 above for the Test set.
test <- VCorpus(DirSource("Test", encoding = "UTF-8"), readerControl=list(language="English"))
test <- tm_map(test, content_transformer(stripWhitespace))
test <- tm_map(test, content_transformer(tolower))
test <- tm_map(test, content_transformer(removeNumbers))
test <- tm_map(test, content_transformer(removePunctuation))
test.dtm <- as.matrix(DocumentTermMatrix(test, control=list(wordLengths=c(1,Inf))))

# Step 4. Make test and train matrices of identical length (find intersection)
train.df <- data.frame(train.dtm[,intersect(colnames(train.dtm), colnames(test.dtm))])
test.df <- data.frame(test.dtm[,intersect(colnames(test.dtm), colnames(train.dtm))])

# Step 5. Retrieve the correct labels for training data and put dummy values for testing data
label.df <- data.frame(row.names(train.df))
colnames(label.df) <- c("filenames")
label.df<- cSplit(label.df, 'filenames', sep="_", type.convert=FALSE)
train.df$corpus<- label.df$filenames_1
test.df$corpus <- c("Neg")

# Step 6. Create folds of your data, then run the training once to inspect results
df.train <- train.df
df.test <- train.df
df.model <- ksvm(corpus~., data= df.train, kernel="rbfdot")
df.pred <- predict(df.model, df.test)
# con.matrix <- confusionMatrix(df.pred, df.test$corpus)
con.matrix <- confusionMatrix(table(df.pred, df.test$corpus)) 
print(con.matrix)

# Step 7. Run the final prediction on the test data and re-attach file names.
df.test <- test.df
df.pred <- predict(df.model, df.test)
results <- as.data.frame(df.pred)
rownames(results) <- rownames(test.df)
print(results)
