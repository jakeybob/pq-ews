library(tidyverse)
library(jsonlite)
library(lubridate)
library(rvest)
library(xml2)
library(selectr)


url <- "http://www.parliament.scot/parliamentarybusiness/28877.aspx?SearchType=Advance&DateTo=01/02/2019%2023:59:59&SortBy=DateSubmitted&Answers=OnlyQuestionAwaitingAnswer&SearchFor=AllQuestions&ResultsPerPage=10"

tempFileName <- paste0(".tmp", paste0(sample(1e10, 1)))
download.file(url, destfile = tempFileName, quiet=TRUE) # necessary to avoid proxy issues
webpage <- read_html(tempFileName)
file.remove(tempFileName)


a <- html_nodes(webpage, 
xpath="//*[(@id = \"ctl01\")] | //p | //*[(@id = \"MAQA_Search_gvResults_ctl00__0\")]//td")

b <- html_nodes(webpage, "table tr")


