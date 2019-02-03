library(tidyverse)
library(jsonlite)
library(lubridate)
library(rvest)
library(xml2)
library(selectr)

num_results = 10
url <- paste0("http://www.parliament.scot/parliamentarybusiness/28877.aspx?SearchType=Advance&DateTo=01/02/2019%2023:59:59&SortBy=DateSubmitted&Answers=OnlyQuestionAwaitingAnswer&SearchFor=AllQuestions&ResultsPerPage=", as.character(num_results))

tempFileName <- paste0(".tmp", paste0(sample(1e10, 1)))
download.file(url, destfile = tempFileName, quiet=TRUE) # necessary to avoid proxy issues
webpage <- read_html(tempFileName)
file.remove(tempFileName)

# g <- html_table(html_nodes(webpage, "table"), fill=T)

expected_answer_dates <- html_nodes(webpage, "#MAQA_Search_gvResults_ctl00 div div span") # expected dates and search result IDs

question_details <- html_nodes(webpage, "strong span") # PQ S5W ref, MSP name, party, date lodged etc, plus search result ID

question_text <- html_nodes(webpage, "#MAQA_Search_gvResults_ctl00 p") # question text, no search result ID though
question_text2 <- html_nodes(webpage, "#MAQA_Search_gvResults_ctl00_ctl04__pnlQuestionHeader , #MAQA_Search_gvResults_ctl00_ctl06__pnlQuestionHeader, #MAQA_Search_gvResults_ctl00_ctl08__lblQuestionTitle p, #MAQA_Search_gvResults_ctl00_ctl08__pnlQuestionHeader, #MAQA_Search_gvResults_ctl00_ctl06__lblQuestionTitle p, #MAQA_Search_gvResults_ctl00_ctl04__lblQuestionTitle p") # question text, no search result ID though
