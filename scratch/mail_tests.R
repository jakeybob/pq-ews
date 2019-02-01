# note RStudioServer runs on Linux version 3.10.0, x86_64, Red Hat 4.8.5-28;
# stripped down distro but has sendmail and postfix

library(sendmailR)

from <- "<bob.taylor@nhs.net>"
to <- "<j.boaby@gmail.com>"
subject <- "Test"
body <- list("Basically zero chance this'll work.")
sendmail(from, to, subject, body,
         control=list(smtpServer="17e818c0-1feb-4a81-96cd-ac2c48ca0db8@nhs.net", smtpPort=25))


library(mailR)



library(gmailr)
