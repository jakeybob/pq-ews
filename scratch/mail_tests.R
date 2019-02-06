# note RStudioServer runs on Linux version 3.10.0, x86_64, Red Hat 4.8.5-28;
# stripped down distro but has sendmail and postfix

# library(sendmailR)
# 
# from <- "<bob.taylor@nhs.net>"
# to <- "<j.boaby@gmail.com>"
# subject <- "Test"
# body <- list("Basically zero chance this'll work.")
# sendmail(from, to, subject, body,
#          control=list(smtpServer="17e818c0-1feb-4a81-96cd-ac2c48ca0db8@nhs.net", smtpPort=25))

# library(mailR)
# sender <- "j.boaby@gmail.com"
# recipients <- c("bob.taylor@nhs.net")
# send.mail(from = sender,
#           to = recipients,
#           subject = "Test mail from Rstudio",
#           body = "Test email body",
#           smtp = list(host.name = "smtp.gmail.com", port = 465, 
#                       user.name = "j.boaby@gmail.com",            
#                       passwd = pass, ssl = TRUE),
#           authenticate = TRUE,
#           send = TRUE)

library(gmailr)
use_secret_file("pq-ews.json")

email <- mime(From="j.boaby@gmail.com",
           To="bob.taylor@nhs.net",
           Subject="HTML test") %>%
  html_body("<b>Test</b> <em>Message</em> <h3>again</h3>")


send_message(email)
