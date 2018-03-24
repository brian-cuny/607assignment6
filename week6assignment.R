library(tidyverse)
library(magrittr)
library(jsonlite)
library(yaml)
library(XML)
library(httr)
library(RCurl)

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

movie.data.html <- getURL('http://www.boxofficemojo.com/yearly/chart/?yr=2017&p=.htm') %>% 
  htmlParse()

movie.data.headers <- movie.data.html %>%
  xpathSApply('//*[@id="body"]/table[3]//tr//td', xmlValue) %>%
  .[c(7:9, 11:14)] %>%
  str_split(' / ') %>% 
  unlist() %>%
  str_extract('\\w+') %>%
  unique()

movie.data.frame <- movie.data.html %>%
  xpathSApply('//*[@id="body"]/table[3]//tr//td', xmlValue) %>%
  .[15:914] %>%
  matrix(ncol=9, byrow=T) %>%
  as.data.frame() %>%
  select(-7) %>%
  setNames(movie.data.headers) %>%
  mutate(Movie=str_c("'", Movie, "'"))


review.data.frame <- movie.data.frame %>%
  map_df(~API.Query(list('query'=as.character(.)))) %>% 
  filter(publication_date %>% startsWith('2017'))


# join them here ----------------------------------------------------------



