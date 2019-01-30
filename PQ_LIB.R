library(tidyverse)
library(jsonlite)
library(lubridate)

generate_archive <- function(start_year=2017, save=TRUE, ...){
  
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
    saveRDS(df, "archive.rds")}
  
  return(df)
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

compare <- function(){
  url <- paste0("https://data.parliament.scot/api/motionsquestionsanswersquestions?year=", as.integer(format(Sys.time(), "%Y")))
  currentPQ_IDs <- dplyr::as_tibble(jsonlite::fromJSON(txt=url(url), simplifyDataFrame = TRUE))$EventID
  
  recentPQ_IDs <- read_rds("recentPQ_IDs.rds")
  
  newPQ_IDs <- currentPQ_IDs %>%
    filter()  #  can't do a==b comparison as a will be longer than b
}
  

df <- generate_archive()
train()

