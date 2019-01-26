library(tidyverse)
library(jsonlite)
library(lubridate)

#### URLS ####
url <- "https://data.parliament.scot/api/motionsquestionsanswersquestions?year=2018"  #  goes back to 1999
event_subtypes_url <- "https://data.parliament.scot/api/motionsquestionsanswerseventsubtypes/json"
MSP_url <- "https://data.parliament.scot/api/members/json"
constituency_url <- "https://data.parliament.scot/api/constituencies/json"
region_url <- "https://data.parliament.scot/api/regions/json"


#### QUESTION / MOTION DATA ####

data <- as_tibble(jsonlite::fromJSON(txt=url(url), simplifyDataFrame = TRUE))

# Drop columns with non-varying data, as is no use for classifying etc

# cols_num_uniques <- sapply(data, function(x) length(unique(x))) # number of unique entries in each column
# cols_to_keep <- names(cols_num_uniques[cols_num_uniques != 1]) # keep columns with >1 unique entry

# will hardcode cols_to_keep as below to keep consistency 
cols_to_keep <- c("UniqueID", "EventID", "EventTypeID", "EventSubTypeID", "MSPID", "Party", "RegionID", "ConstituencyID", 
                  "ApprovedDate", "SubmissionDateTime", "ItemText", "AnswerText", "AnswerDate", 
                  "ExpectedAnswerDate", "MeetingDate", "AnsweredByMSP", "RegisteredInterest")


#### EVENT CLASSIFICATION ####

event_subtypes_data <- as_tibble(jsonlite::fromJSON(txt=url(event_subtypes_url), simplifyDataFrame = TRUE))

event_subtypes_df <- event_subtypes_data %>%
  select(-IntroText) %>%
  filter(str_detect(EventSubType, fixed("question", ignore_case=TRUE)))



#### REGION CLASSIFICATION ####
# RegionID variable already takes care of region names changing over time etc, so don't require extra logic
region_data <- as_tibble(jsonlite::fromJSON(txt=url(region_url), simplifyDataFrame = TRUE))

region_df <- region_data %>%
  mutate_at(vars(contains("Date")), ~as_datetime(.))  #  convert date/time columns from char to datetime


#### CONSTITUENCY CLASSIFICATION ####
# Likewise, constituency IDs are also unique over time so shouldn't have to take account of
constituency_data <- as_tibble(jsonlite::fromJSON(txt=url(constituency_url), simplifyDataFrame = TRUE))

constituency_df <- constituency_data %>%
  mutate_at(vars(contains("Date")), ~as_datetime(.))  #  convert date/time columns from char to datetime


#### MSP DATA ####  
MSP_data <- as_tibble(jsonlite::fromJSON(txt=url(MSP_url), simplifyDataFrame = TRUE))

MSP_df <- MSP_data %>%
  select(-BirthDateIsProtected) %>%
  mutate_at(vars(contains("Date")), ~as_datetime(.)) %>%  #  convert date/time columns from char to datetime
  separate(ParliamentaryName, into=c("Surname", "FirstName"), sep=", ", extra="merge", remove=TRUE) %>%
  mutate(FullPreferredName = paste(PreferredName, Surname),
         age_years = year(as.period(interval(BirthDate, today()), unit = "years")),
         refresh_date = today()) %>%
  rename(MSPID = PersonID)


#### DATA MERGE ####
df <- data %>%
  select(cols_to_keep) %>%
  mutate_at(vars(contains("Date")), ~as_datetime(.)) %>%
  
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


# tidyup







# email tests
library(gmailr)
test_email <- mime(
  To = "bobajob45@hotmail.com",
  From = "j.boaby@gmail.com",
  Subject = "this is just a gmailr test",
  body = "Can you hear me now?")

send_message(test_email)  # fails auth step due to curl/HTTPS proxy faff


# installing from beta repo
# install.packages("RDCOMClient", repos = "http://www.omegahat.net/R") 
library(RDCOMClient)
## init com api
OutApp <- COMCreate("Outlook.Application")
## create an email 
outMail = OutApp$CreateItem(0)
## configure  email parameter 
outMail[["To"]] = "j.boaby@gmail.com"
outMail[["subject"]] = "some subject"
outMail[["body"]] = "some body"
## send it                     
outMail$Send()  
# works, but will it work from e.g. server environment?




# https://stackoverflow.com/questions/26811679/sending-email-in-r-via-outlook
library(sendmailR)

#set working directory
setwd("C:/workingdirectorypath")

#####send plain email

from <- "bob.taylor@nhs.net"
to <- "j.boaby@gmail.com"
subject <- "Email Subject"
body <- "Email body."                     
mailControl=list(smtpServer="17e818c0-1feb-4a81-96cd-ac2c48ca0db8@nhs.net")

sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)






