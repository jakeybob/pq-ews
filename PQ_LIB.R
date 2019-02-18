library(tidyverse)
library(jsonlite)
library(lubridate)
library(rvest)
library(xml2)
library(gmailr)

setwd("/home/bob/pq-ews")

default_email_from <- "j.boaby@gmail.com"
default_email_to <- c("bob.taylor@nhs.net")

generate_archive_opendata <- function(start_year=2017, save=TRUE, ...){
  
  current_year <- as.integer(format(Sys.time(), "%Y"))
  years <- as.character(seq(from=start_year, to=current_year))
  
  #### URLS ####
  event_subtypes_url <- "https://data.parliament.scot/api/motionsquestionsanswerseventsubtypes/json"
  MSP_url <- "https://data.parliament.scot/api/members/json"
  constituency_url <- "https://data.parliament.scot/api/constituencies/json"
  region_url <- "https://data.parliament.scot/api/regions/json"
  PQ_urls <- paste0("https://data.parliament.scot/api/motionsquestionsanswersquestions?year=", years)
  
  
  #### PQ DATA ####

  current_year_PQ_IDs <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(PQ_urls[length(PQ_urls)]), simplifyDataFrame = TRUE))$EventID
  if(save==TRUE){
    saveRDS(current_year_PQ_IDs, "recentPQ_IDs.rds")
  }
  
  PQ_data <- dplyr::tibble()
  for(PQ_url in PQ_urls){
    PQ_data <- dplyr::bind_rows(PQ_data, dplyr::as_tibble(jsonlite::fromJSON(txt=url(PQ_url), simplifyDataFrame = TRUE)))
    print(paste(stringr::str_sub(PQ_url, -4), "data added."))
  }
  
  # will hardcode cols_to_keep as below to keep consistency 
  cols_to_keep <- c("UniqueID", "EventID", "EventTypeID", "EventSubTypeID", "MSPID", "Party", "RegionID", "ConstituencyID", 
                    "ApprovedDate", "SubmissionDateTime", "ItemText", "AnswerText", "AnswerDate", 
                    "ExpectedAnswerDate", "MeetingDate", "AnsweredByMSP", "RegisteredInterest")
  
  PQ_data <- PQ_data %>%
    select(cols_to_keep) %>%
    mutate_at(vars(contains("Date")), ~as_datetime(.))
  

  #### EVENT CLASSIFICATION ####
  
  event_subtypes_data <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(event_subtypes_url), simplifyDataFrame = TRUE))
  
  event_subtypes_df <- event_subtypes_data %>%
    select(-IntroText) %>%
    filter(str_detect(EventSubType, fixed("question", ignore_case=TRUE)))
  
  
  
  #### REGION CLASSIFICATION ####
  # RegionID variable already takes care of region names changing over time etc, so don't require extra logic
  region_data <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(region_url), simplifyDataFrame = TRUE))
  
  region_df <- region_data %>%
    mutate_at(vars(contains("Date")), ~as_datetime(.))  #  convert date/time columns from char to datetime
  
  
  #### CONSTITUENCY CLASSIFICATION ####
  # Likewise, constituency IDs are also unique over time so shouldn't have to take account of
  constituency_data <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(constituency_url), simplifyDataFrame = TRUE))
  
  constituency_df <- constituency_data %>%
    mutate_at(vars(contains("Date")), ~as_datetime(.)) %>%  #  convert date/time columns from char to datetime
    select(-RegionID)
  
  
  #### MSP DATA ####  
  MSP_data <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(MSP_url), simplifyDataFrame = TRUE))
  
  MSP_df <- MSP_data %>%
    select(-BirthDateIsProtected) %>%
    mutate_at(vars(contains("Date")), ~as_datetime(.)) %>%  #  convert date/time columns from char to datetime
    separate(ParliamentaryName, into=c("Surname", "FirstName"), sep=", ", extra="merge", remove=TRUE) %>%
    mutate(FullPreferredName = paste(PreferredName, Surname),
           age_years = year(as.period(interval(BirthDate, today()), unit = "years")),
           refresh_date = today()) %>%
    rename(MSPID = PersonID)
  
  
  #### DATA MERGE ####
  df <- PQ_data %>%

    # merge in the definitions of the different question types (will automatically drop e.g. motions)
    left_join(event_subtypes_df, by=c("EventTypeID", "EventSubTypeID")) %>%
    select(-c("EventTypeID", "EventSubTypeID")) %>%
    
    # merge in Region info
    left_join(region_df, by=c("RegionID" = "ID")) %>%
    select(-c("StartDate", "EndDate")) %>%
    rename(RegionName = Name) %>%
    
    # merge in Constituency info
    left_join(constituency_df, by=c("ConstituencyID" = "ID")) %>%
    select(-c("ShortName", "ValidFromDate", "ValidUntilDate")) %>%
    rename(ConstituencyName = Name) %>%
    
    # merge in MSP info
    left_join(MSP_df, df, by="MSPID") %>%
    
    arrange(ApprovedDate)
  
  if(save==TRUE){
    saveRDS(df, "opendata_archive.rds")}
  
  return(df)
}

compare_opendata <- function(){
  url <- paste0("https://data.parliament.scot/api/motionsquestionsanswersquestions?year=", as.integer(format(Sys.time(), "%Y")))
  currentPQ_IDs <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(url), simplifyDataFrame = TRUE))$EventID
  
  recentPQ_IDs <- read_rds("recentPQ_IDs.rds")
  
  new_PQ_IDs <- currentPQ_IDs[which(!(currentPQ_IDs %in% recentPQ_IDs))]
  
  print(paste("New PQs are:", toString(new_PQ_IDs)))
  # newPQ_IDs <- currentPQ_IDs %>%
  #   filter()  #  can't do a==b comparison as a will be longer than b
}

train <- function(save=TRUE){
  # read in list of PQs completed by team (drop rows that are blank or NA)
  completed_PQs <- readr::read_delim(file="QI_PQs.txt", delim="\r", col_names="PQ", col_type="c")  %>%
    filter(PQ != "" & !is.na(PQ))
  completed_PQs$team <- "QI"  #  append team name
  
  if(save==TRUE){
    saveRDS(completed_PQs, "train.rds")}
  
  return(completed_PQs)
}

generate_archive_scrape <- function(num_results=1000, save=TRUE, ...){
  # NOTE: this method does not seem to return all data from past PQs!
  # e.g. setting num_results = 10000 shows that previous years drop sharply in the return data....
  # Hence, better to use this for recent comparison purposes, and use opendata API to generate long-term
  # archive, keeping in mind that it is not completley up to date.
  
  # OR, there may be another url/search scheme that reliably returns long term data to scrape?
  
  current_date <- format(today(), "%d/%m/%Y")
  
  # url <- paste0("http://www.parliament.scot/parliamentarybusiness/28877.aspx?SearchType=Advance&DateTo=04/02/2019%2023:59:59&SortBy=DateSubmitted&Answers=OnlyQuestionAwaitingAnswer&SearchFor=AllQuestions&ResultsPerPage=", as.character(num_results))
  # url <- paste0("http://www.parliament.scot/parliamentarybusiness/28877.aspx?SearchType=Advance&DateChoice=3&SortBy=DateSubmitted&Answers=All&SearchFor=AllQuestions&ResultsPerPage=", as.character(num_results))
  
  url <- paste0("http://www.parliament.scot/parliamentarybusiness/28877.aspx?SearchType=Advance&DateTo=",
                current_date,
                "%2023:59:59&SortBy=DateSubmitted&Answers=OnlyQuestionAwaitingAnswer&SearchFor=AllQuestions&ResultsPerPage=", 
                as.character(num_results))
  
  
  regex_ID_selector <- "(?<=\\_ctl00\\_ctl)(.*?)(?=\\_\\_)" # to pull out a unique ID for each search result
  
  tempFileName <- paste0(".tmp", paste0(sample(1e10, 1)))
  download.file(url, destfile = tempFileName, quiet=TRUE) # necessary to avoid proxy issues
  webpage <- read_html(tempFileName)
  file.remove(tempFileName)
  
  #### CURRENT STATUS DATA ####
  current_status <- html_nodes(webpage, "#MAQA_Search_gvResults_ctl00 div div span") # expected dates and search result IDs
  current_status_IDs <- str_extract(html_attr(current_status, name="id")[c(TRUE, FALSE)], regex_ID_selector)
  current_status_text <- paste0(html_text(current_status)[c(TRUE, FALSE)], html_text(current_status)[c(FALSE, TRUE)])
  
  current_status_df <- tibble(searchID = current_status_IDs, current_status_text = current_status_text)
  
  # if "expected answer date" string detected, extract expected answer date and convert to date
  current_status_df$expected_answer_date <- if_else(str_detect(str_to_upper(current_status_df$current_status_text), "EXPECTED ANSWER DATE"),
                                                    str_extract(str_to_upper(current_status_df$current_status_text), "(?<=EXPECTED ANSWER DATE ).*$"),
                                                    "")
  current_status_df$expected_answer_date <- dmy(current_status_df$expected_answer_date)
  
  
  #### QUESTION DETAILS ####
  
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
  
  
  #### QUESTION TEXT ####
  
  question_text_searchID <- question_details_df$searchID
  css_selector <- str_flatten(paste0("#MAQA_Search_gvResults_ctl00_ctl", question_text_searchID, "__lblQuestionTitle"), collapse=", ")
  question_text_text <- html_text(html_nodes(webpage, css_selector))
  
  question_text_df <- tibble(searchID = question_text_searchID,
                             question_text = question_text_text)
  
  
  #### MERGE ####
  
  df <- current_status_df %>%
    inner_join(question_details_df, by="searchID") %>%
    inner_join(question_text_df, by="searchID") %>%
    select(-c(current_status_text, question_details_text)) %>%
    filter(str_detect(question_text, "Question to be taken in chamber")==FALSE)
  
  if(save==TRUE){
    saveRDS(df, "scrape_archive.rds")}
  
  return(df)
  
}

compare_scrape <- function(num_results = 100, update_recent = FALSE, ...){
  
  currentPQs_df <- generate_archive_scrape(num_results=num_results, save=FALSE)

  currentPQ_IDs <- currentPQs_df$PQID
  
  recentPQ_IDs <- read_rds("scrape_archive.rds")$PQID
  
  new_PQ_IDs <- currentPQ_IDs[which(!(currentPQ_IDs %in% recentPQ_IDs))]
  
  if(length(new_PQ_IDs) == 0){
    print(paste(now(), ": no new PQs since last refresh."))
  }
  else{
    print(paste(now(), ": new PQs are:", toString(new_PQ_IDs)))
  }
  
  
  if(update_recent==TRUE){
    generate_archive_scrape(save=TRUE)
  }
  
  newPQs_df <- currentPQs_df %>%
    filter(PQID %in% new_PQ_IDs)
  
  
  return(newPQs_df)

}

send_email <- function(message, from=default_email_from, to=default_email_to, subject="",...){
  
  use_secret_file(file.path(getwd(), "pq-ews.json"))
  
  # clunky hack to send emails to multiple recipients. Must be a better gmailr method...
  for(address in default_email_to){
    email <- mime(From = from,
                  To = address,
                  Subject = subject) %>%
      html_body(message)
    
    send_message(email)
  }
}

send_PQ_email <- function(newPQs, ...){
  text_date_format <- "%A %d %B %Y"
  
  message <- "<style>
  body{background-color:#fefefe;
font-family: Helvetica, Arial, sans-serif;
color:#333}
h3{font-size:1.2em;
color:#333333;
font-family: Helvetica, Arial, sans-serif;
line-height:1em;}
div.dates{line-height:1.3;}
div.MSPinfo{line-height:1.3;}
div.PQ{margin-bottom: 2em;
margin-right: 0.5em;
margin-left: 0.5em;
padding-top: 0.5em;
padding-bottom: 0.5em;
padding-right: 1em;
padding-left: 1em;
border-style:solid;
border-width:3px;
border-color: #999;
border-radius: 10px;}
span.MSPdetails{font-size: 0.7em}
</style>" # inline css
  
  for(PQref in newPQs$PQID){
    PQ <- filter(newPQs, newPQs$PQID==PQref)
    
    text_PQID <- paste0("<h3>", PQref,"</h3>")
    text_expected_date <- paste0("Answer expected: ", "<strong>", format(PQ$expected_answer_date, text_date_format), "</strong>")
    text_date <- paste0("Submitted: ", "<strong>", format(PQ$date, text_date_format), "</strong>")
    text_MSP_details <- paste0("<strong>",
                               PQ$MSPname, "</strong> <br><span class=\"MSPdetails\">",
                               PQ$area, ", ",
                               PQ$party, "</span>")
    text_question <- paste0("<em>", PQ$question_text, "</em>")
    
    message <- paste0(message,
                      "<div class=\"PQ\">", text_PQID,
                      "<div class=\"dates\">", text_expected_date, "<br>", text_date, "<br></div>",
                      "<div class=\"MSPinfo\"><br>", text_MSP_details, "<br><br></div>",
                      text_question, "<br><br></div>")
    
  }
  
  send_email(message = message, subject=paste("New PQs |", format(now(), "%h %d %Y %H:%M")))
  
  
}


#### CODE ####
# generate_archive_opendata(start_year = 2011, save = TRUE) # use open API to generate historical archive
# df <- generate_archive_scrape(num_results = 1000, save=TRUE)
# currentPQ_IDs <- generate_archive_webscrape(num_results=10, save=FALSE)$PQID

# test_df <- generate_archive_scrape(num_results = 5, save = FALSE)
# send_PQ_email(test_df)

newPQs <- compare_scrape(update_recent = TRUE)
if(dim(newPQs)[1] > 0){
  send_PQ_email(newPQs)
}


# cmd <- cronR::cron_rscript("/home/bob/pq-ews/PQ_LIB.R", log_append = FALSE)
# cronR::cron_add(command = cmd, frequency = "hourly", id="PQjob", days_of_week = c(1, 2, 3, 4, 5))
# # Possible fixes for cron issues...
# # Run "sudo service cron restart", and "chmod u+x whatever.R" as well...
# # Add root and username to /etc/cron.allow
