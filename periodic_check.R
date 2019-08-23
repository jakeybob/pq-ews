setwd("/home/bob/pq-ews")
source("PQ_LIB.R")

#### SETTINGS ####
email_addresses <- read_json("email_addresses.json") # see email_addresses_example.json for example format for this file
# default_email_from <- "'PQ Update' <j.boaby@gmail.com>"
# default_email_to <- c("bob.taylor@nhs.net", "bobajob45@hotmail.com")
default_email_from <- email_addresses$from
default_email_to <- email_addresses$to


#### CODE ####
newPQs <- compare_scrape(update_recent = TRUE)
if(dim(newPQs)[1] > 0){
  send_PQ_email(newPQs, log=TRUE)
}

# cmd <- cronR::cron_rscript("/home/bob/pq-ews/periodic_check.R", log_append = FALSE)
# cronR::cron_add(command = cmd, frequency = "hourly", id="PQjob", days_of_week = c(1, 2, 3, 4, 5))
# # Possible fixes for cron issues...
# # Run "sudo service cron restart", and "chmod u+x whatever.R" as well...
# # Add root and username to /etc/cron.allow