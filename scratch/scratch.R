library(tidyverse)
library(jsonlite)
library(lubridate)
library(rvest)
library(xml2)

num_results = 200
url <- paste0("http://www.parliament.scot/parliamentarybusiness/28877.aspx?SearchType=Advance&DateTo=01/02/2019%2023:59:59&SortBy=DateSubmitted&Answers=OnlyQuestionAwaitingAnswer&SearchFor=AllQuestions&ResultsPerPage=", as.character(num_results))
regex_ID_selector <- "(?<=\\_ctl00\\_ctl)(.*?)(?=\\_\\_)" # to pull out a unique ID for each search result

tempFileName <- paste0(".tmp", paste0(sample(1e10, 1)))
download.file(url, destfile = tempFileName, quiet=TRUE) # necessary to avoid proxy issues
webpage <- read_html(tempFileName)
file.remove(tempFileName)

#### current status bumf ####
current_status <- html_nodes(webpage, "#MAQA_Search_gvResults_ctl00 div div span") # expected dates and search result IDs
current_status_IDs <- str_extract(html_attr(current_status, name="id")[c(TRUE, FALSE)], regex_ID_selector)
current_status_text <- paste0(html_text(current_status)[c(TRUE, FALSE)], html_text(current_status)[c(FALSE, TRUE)])

# create tibble of related data
current_status_df <- tibble(searchID = current_status_IDs, current_status_text = current_status_text)

# if "expected answer date" string detected, extract expected answer date and convert to date
current_status_df$expected_answer_date <- if_else(str_detect(current_status_df$current_status_text, "Expected Answer date"),
                                                  str_extract(current_status_df$current_status_text, "(?<=Expected Answer date ).*$"),
                                                  "")
current_status_df$expected_answer_date <- dmy(current_status_df$expected_answer_date)



#### question details bumf ####

question_details <- html_nodes(webpage, "strong span") # PQ S5W ref, MSP name, party, date lodged etc, plus search result ID
question_details_IDs <- str_extract(html_attr(question_details, name="id"), regex_ID_selector)
question_details_text <- str_split_fixed(html_text(question_details), ", ", n=4)
question_details_PQID <- str_extract(question_details_text[,1], "(?<=Question  )(.*?)(?=:)")
question_details_MSPname <- str_split(question_details_text[,1], ": ", simplify = TRUE)[,2]

question_details_df <- tibble(searchID = question_details_IDs,
                              PQID = question_details_PQID,
                              MSPname = question_details_MSPname,
                              area = question_details_text[,2],
                              party = question_details_text[,3],
                              date = dmy(question_details_text[,4]),
                              question_details_text = html_text(question_details))


#### question text bumf ####

question_text_searchID <- question_details_df$searchID
css_selector <- str_flatten(paste0("#MAQA_Search_gvResults_ctl00_ctl", question_text_searchID, "__lblQuestionTitle"), collapse=", ")
question_text_text <- html_text(html_nodes(webpage, css_selector))

question_text_df <- tibble(searchID = question_text_searchID,
                            question_text = question_text_text)


#### merge ####

df <- current_status_df %>%
  inner_join(question_details_df, by="searchID") %>%
  inner_join(question_text_df, by="searchID") %>%
  select(-c(current_status_text, question_details_text))

saveRDS(df, "recentPQs.rds")
