library(urltools)
library(jsonlite)
library(stringr)
library(dplyr)

#set srv = to the link
srv <- 'https://irma.nps.gov/Stats/Reports/ParkReportItems'

#set id = to the park acronym you want
id <- 'CATO'

#modify the url you are calling to include the park acronym (id)
url <- param_set(srv, 'id', id)

#call the NPS data (JSON type of data) from your url
docs <- fromJSON(url)

#report items listed in the url
docs_df <- docs[['ReportItems']]
report_url <- docs_df %>%
  filter(str_detect(Name, 'Annual Park Recreation Visitation \\([0-9]{4}')) %>%
  pull(Location)

#what is the url where the data are stored?
report_url

# session <- html_session(paste(
#   'https://irma.nps.gov/Stats/Reports/Park',
#   park,
#   sep = '/'))
# page <- read_html(session)
# a <- html_node(page, css = 'a')
