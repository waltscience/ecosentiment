# Has the sentiment among academic job seekers in ecology grown more negative?
# Chris Walter - chriswalter.info/ecosentiment
#
# Data sources - Ecology job wiki discussion, venting, and positive affirmation tabs from 2009-2019.
#                Collected and cleaned by the author and available at https://github.com/waltscience/ecosentiment
#
#                Original data sources accessed on July 18, 2020 and were available at the following locations:
#                2019-20 ecoevojobs	https://docs.google.com/spreadsheets/d/1yLm9LhXNKL0YTV6M1lHsNRvypnWI0RRi613s6z494FE/
#                2018-19 ecoevojobs	https://docs.google.com/spreadsheets/d/1z64-PTCydZIB_afaYXYUf4fVniFckHmGZwVgbeg4nNY/
#                2017-18 ecoevojobs	https://docs.google.com/spreadsheets/d/1XcEuPa7YPSHaw11OB9sPKZfQ8ZCYrdiMKzQZC8QSN7s/
#                2016-17 Eco/Evo Jobs	"https://docs.google.com/spreadsheets/d/1rOjzt71IBfaz5gYuiVyJxzoCAD8WqM39eUjUhgBEFuU/edit#gid=1240849863
#                2015-16 Eco/Evo Faculty Jobs	https://docs.google.com/spreadsheets/d/1ub9cA2jOR3QX_qn_K9g0zgoBVC2A7RxiqaZcdDmVn4U/edit#gid=169647778
#                2014-15 Eco_Evo Faculty Jobs	https://docs.google.com/spreadsheets/d/1CRev4oZ63tdsxNaURX6uYCSWLxTQbZ8Q-I7KVJpM-4c/edit?usp=sharing
#                2013-14 Wiki Ecology Postings	https://docs.google.com/spreadsheets/d/11lzZVqkME_dFN0NJpiXDSutz789cqSLilt_OZWIcxS8/edit?usp=sharing
#                2012-13 Wiki Ecology Postings	https://docs.google.com/spreadsheets/d/1fPDPAT_Pc2rhOb-EYLJILuXAAH7DEAdhYH9U_gp2BHg/edit?usp=sharing
#                2009-10 Wiki Ecology Postings	https://docs.google.com/spreadsheets/d/1zORxvfOLMo9a8JIEKbWZXydWlCgRuQGkIpWRVs1wHq0/edit#gid=64

packages <- c("readr", "tidytext", "textdata", "dplyr", "tidyr", "ggplot2") # packages to load
lapply(packages, library, character.only = TRUE)                         # load packages
afinn <- as.data.frame(get_sentiments(lexicon = "afinn"))                # load sentiment dictionary
dat <- as.data.frame(read_csv(url("https://raw.githubusercontent.com/waltscience/ecosentiment/master/threads.csv"))) # load data
sentdata <- data.frame(                                                  # initialize the final data frame
                      "Thread"    = as.numeric(),
                      "Sentiment" = as.numeric(),
                      "Type"      = as.character(),
                      "Year"      = as.numeric()
)
for (i in 1:ncol(dat)) {                                                 # loop to compute sentiment by column
  d <- na.omit(dat[, i])                                                 # select data column
  d <- tibble(thread = 1:length(d), text = as.character(d))              # create a character tibble
  d %>% unnest_tokens(word, text) -> d                                   # convert strings to words
  d <- as.data.frame(d)                                                  # convert to data frame for merge  
  words <- merge(d, afinn, by = "word")                                  # merge words with dictionary
  words %>% count(thread, value) -> words                                # count words by sentiment
  words <- as.data.frame(words)                                          # convert to data frame
  words$sum <- words$value * words$n                                     # sum sentiment by sentiment value
  words <- aggregate(words$sum, by = list(Category = words$thread), FUN = sum)  # sum net sentiment by thread
  words$Type <- substr(colnames(dat)[i], 1, 4)                           # add thread type to data frame
  words$Year <- as.numeric(paste("20", substr(colnames(dat)[i], 5, 6), sep = ""))  # add year to data frame
  colnames(words) <- c("Thread", "Sentiment", "Type", "Year")            # add column names to new data frame
  sentdata <- rbind(sentdata, words)
}
write.csv(sentdata, "sentdata.csv", row.names = FALSE)
sentdata <- read.csv("sentdata.csv", header = TRUE)

# Note on data flow - I learned R in Base R, before tidyverse existed. I understand this workflow could be 
# much more efficient. I am currently working to learn tidyverse to make my data processing more efficient.

