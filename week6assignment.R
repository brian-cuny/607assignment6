library(tidyverse)
library(magrittr)
library(jsonlite)
library(yaml)
library(XML)
library(httr)


# nyt api -----------------------------------------------------------------
url <- 'https://api.nytimes.com/svc/movies/v2/reviews/search.json'
api.key <- read_table('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\assignments\\week6assignmentKey.txt', col_names=FALSE) %>% 
  unlist() %>%
  as.vector()

API.Query <- function(params){
  Sys.sleep(1)
  temp <- GET(url, query=c('api-key'=api.key, params)) %>%
    content(as='text') %>%
    fromJSON(flatten=TRUE) %>%
    .[[5]] %>%
    as.tibble()
  print(temp)
  return(temp)
}

# boxofficemojo -----------------------------------------------------------

movie.data.frame <- getURL('http://www.boxofficemojo.com/yearly/chart/?yr=2018&p=.htm') %>% 
  htmlParse() %>%
  xpathSApply('//*[@id="body"]/table[3]//tr/td[2]', xmlValue) %>%
  str_c("'", ., "'") %>%
  .[3:102]

data <- movie.data.frame %>%
  map_df(~API.Query(list('query'=as.character(.))))

current <- data %>% 
  filter(publication_date %>% startsWith('2018'))



