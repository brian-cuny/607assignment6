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
  GET(url, query=c('api-key'=api.key, params)) %>%
    content(as='text') %>%
    fromJSON(flatten=TRUE) %>%
    .[[5]] %>%
    as.tibble()
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
  mutate(Movie=str_replace(Movie, '(.*?)( \\(2017\\))$', '\\1')) %>%
  select(1:4)

# process -----------------------------------------------------------------

review.data.frame <- movie.data.frame$Movie %>%
  map_df(~API.Query(list('query'=as.character(.)))) %>% 
  filter(publication_date %>% startsWith('2017')) %>%
  filter(display_title %in% movie.data.frame$Movie) %>%
  select(1:3) %>%
  unique()

combined.frame <- movie.data.frame %>%
  inner_join(review.data.frame, by=c('Movie'='display_title')) %>%
  mutate(Rank = as.numeric(Rank),
         Total = Total %>% parse_number(),
         Opening = Opening %>% parse_number())


# graphs ------------------------------------------------------------------
require(scales)

ggplot(combined.frame) +
  geom_bar(aes(x=mpaa_rating, fill=Studio))

ggplot(combined.frame) + 
  geom_point(aes(x=mpaa_rating %>% as.factor(), y=Total)) +
  geom_hline(yintercept=combined.frame %>% filter(mpaa_rating == 'G') %>% .$Total %>% max(), col='pink') +
  geom_hline(yintercept=combined.frame %>% filter(mpaa_rating == 'PG') %>% .$Total %>% max(), col='red') +
  geom_hline(yintercept=combined.frame %>% filter(mpaa_rating == 'PG-13') %>% .$Total %>% max(), col='red4') +
  geom_hline(yintercept=combined.frame %>% filter(mpaa_rating == 'R') %>% .$Total %>% max(), col='darksalmon') +
  scale_y_continuous(limits=c(combined.frame$Total %>% min(), combined.frame$Total %>% max()),
                     labels=comma
  ) + 
  labs(x='MPAA Rating',
       title='PG-13 Movies Are Highest Earners')

best.studios <- combined.frame %>%
  group_by(Studio) %>%
  summarize(count=n()) %>%
  arrange(count %>% desc()) %>%
  top_n(5)

combined.frame %>%
  filter(Studio %in% best.studios$Studio) %>%
  ggplot() +
  stat_density(aes(x=Total, color=Studio), size=2, geom='line') + 
  scale_x_continuous(labels=comma) +
  scale_color_brewer(palette='Set1') + 
  theme(axis.ticks.y=element_blank(),
        axis.text.y=element_blank()
        ) +
  labs(y='Density')




