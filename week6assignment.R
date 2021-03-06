library(tidyverse)
library(magrittr)
library(jsonlite)
library(yaml)
library(XML)
library(httr)
library(RCurl)
library(scales)
library(ggrepel)

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
  mutate(Rank = Rank %>% as.numeric(),
         Total = Total %>% parse_number(),
         critics_pick = critics_pick %>% as.factor()
         ) %>%
  distinct()


# graphs ------------------------------------------------------------------
ggplot(combined.frame) +
  geom_bar(aes(x=mpaa_rating, fill=Studio)) +
  labs(x='Mpaa Rating',
        y='Count',
        title='PG-13 and R Movies are Biggest Sellers'
        )

ggplot(combined.frame) + 
  geom_point(aes(x=mpaa_rating %>% as.factor(), y=Total)) +
  geom_hline(yintercept=combined.frame %>% 
                          group_by(mpaa_rating) %>% 
                          arrange(Total %>% 
                                    desc()
                                  ) %>% 
                          top_n(1, Total) %>% 
                          .$Total
             ) +
  geom_label_repel(data=. %>% 
                          group_by(mpaa_rating) %>% 
                          top_n(3, Total),
                   aes(x=mpaa_rating, y=Total, label=Movie, color=mpaa_rating)
                  ) +
  scale_y_continuous(limits=c(combined.frame$Total %>% 
                                min(), 
                              combined.frame$Total %>% 
                                max()
                             ),
                     labels=comma
  ) + 
  labs(x='MPAA Rating',
       title='PG-13 Movies Are Highest Earners'
       ) +
  theme(legend.position='none')

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

combined.frame %>%
  filter(Studio %in% best.studios$Studio) %>%
  ggplot() +
  geom_histogram(aes(x=critics_pick), stat='count') + 
  facet_wrap(~Studio) +
  theme(axis.title.y=element_blank()) +
  labs(x='Critics Picks', 
       title='Top 100 Grossing Movies of 2017 by Studio and Recommendations') +
  scale_x_discrete(labels=c('No', 'Yes')) +
  scale_y_continuous(labels=0:10, breaks=0:10)


