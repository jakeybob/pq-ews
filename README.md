# PQ EWS

## Notes

- a cron job runs the "periodic_check.R" script hourly on weekdays
- Parliament website is queried and questions extracted based on a regular expression identifying the appropriate HTML elements
- if new questions are present, the details are formatted into an HTML list, flagged, and an email sent
- a log file (the R console output from the most recent check) is stored in `/logs/periodic_check.log`