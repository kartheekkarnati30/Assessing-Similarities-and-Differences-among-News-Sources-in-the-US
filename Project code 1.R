#DM Project
library(ggplot2)
library(dplyr)
library(dbplyr)
library(tokenizers)
library(tidytext)
library(stringr)
library(tidyr)
library(textdata)
library(readr)
library(data.table)
#Reading the csv file:
articles <- fread("D:/NEU/Fall 2019/DS5110 Data Management/Project/DS5110-Project-master (3)/DS5110-Project-master/ArticleData2.csv")

#View(articles)

#Separating the article data into day, month and year:
articles <- articles %>% separate(article_date, into = c("year","month","date"),sep = "-")

#Number of articles published by month:
articles %>% count(month, sort = TRUE)

#Number of articles per month per site:
as.data.frame(articles%>% count(news_site, month, sort = TRUE))

#Number of articles classified as right, left or centre leaning:
articles%>% count(political_lean, sort = TRUE)

#Number of articles grouped by political lean and news site:
articles%>% group_by(news_site, political_lean)%>% summarise(no_of_articles = n())

#Basic visualization:
#No of articles per news site per month:
articles %>% ggplot(aes(x=news_site, fill= month))+ geom_bar(position = "dodge")+
  labs(title = "No. of articles per site", x="news site", y="count")

#Number of articles per political lean per news site:
articles %>% ggplot(aes(news_site, fill= month))+
  geom_bar(position = "dodge")+facet_wrap(~political_lean, scales = "free")+
  labs(title = "No of articles per news site per month", x="news site", y="count")+
  coord_flip()


#Converting the article data into words:
unreq_words <- tibble(word = c("â", "itâ", "trumpâ"))

tidy_articles <- articles%>%
  group_by(news_site, political_lean)%>%
  unnest_tokens(word, article_text)%>%
  anti_join(stop_words)%>%
  anti_join(unreq_words)%>%
  select(year:date, political_lean:word)

##Top 5 words per each news site:
abcde <- tibble(word = c('fox','cnn','news'))
tidy_articles %>%
  anti_join(abcde)%>%
  count(news_site, word, sort = TRUE)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(news_site) %>%
  top_n(5) %>%
  ungroup() %>%
  ggplot(aes(reorder(word,n), n, fill = news_site)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~news_site, ncol = 3, scales = "free") +
  #scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',title='Top 5 most appearing words in news articles faceted by news site')+
  coord_flip()+
  ggsave("Top5wordspernewssite1.jpeg",device="jpeg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


##Top 5 words each month:
unrequired_words <- tibble(word=c("width", "typeof","jpg","containerid",0,"fox","cnn"))
tidy_articles %>%
  anti_join(unrequired_words)%>%
  filter(political_lean=="left" | political_lean=="right")%>%
  count(month, word, sort = TRUE)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(month) %>%
  top_n(20) %>%
  ungroup() %>%
  ggplot(aes(word, n,fill=political_lean)) +
  geom_col(position = "stack", show.legend = FALSE) +
  labs(x = "words", y = "count", title = 'Top 20 most appearing words in news articles\n faceted by month') +
  facet_wrap(~month, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("darkblue","darkred"))+
  coord_flip()+
  ggsave("Top5wordseachmonthstack1.jpeg",device="jpeg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


##Most popular person on each news site running for president in the next election:
names <- tibble(word = c("trump","warren","biden","sanders","buttigieg",
                         "booker","gabbard","klobuchar","harris","steyer",
                         "yang"))

tidy_articles %>%
  semi_join(names)%>%
  count(news_site, word, sort = TRUE)%>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(news_site) %>%
  top_n(5) %>%
  ungroup() %>%
  ggplot(aes(word, n, fill = political_lean)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "n") +
  facet_wrap(~news_site, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("darkgreen","darkblue","darkred"))+
  xlab("Popular figures")+ylab("count")+labs(title = "5 most popular people on each news site running for\n president in the next election")+
  coord_flip()+
  ggsave("Most_Popular_candidates1.jpeg", plot=last_plot(),device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")

##Calculating the term frequency per each news site:
articles_words <- tidy_articles%>%
  count(news_site, word, sort = TRUE)

articles_totals <- articles_words%>%
  group_by(news_site)%>%
  summarise(total = sum(n))

articles_words <- articles_words %>%
  left_join(articles_totals)%>%
  mutate(tf = n/total)

articles_words%>%arrange(desc(tf))
#We can clearly see that 'trump', 'president' and the most common words in each news site
#apart from one occurance where 'u.s' is also one of the most common word in Reuters.

#Calculating the tf_idf of the words in the article text:
articles_words_tf <- tidy_articles %>%
  count(news_site, word, sort=TRUE) %>%
  bind_tf_idf(word, news_site, n)

articles_words_tf %>% arrange(desc(tf_idf))  

#Visualizing the highest tf_idfs per news site i.e the words that are specific to
#each news site:
articles_words_tf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = political_lean)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~news_site, ncol = 3, scales = "free") +
  coord_flip()

##Visualizing the least popular candidate running for office in the next election in each
#news site:
counts <- tidy_articles %>%
  semi_join(names)%>%
  count(news_site, word, sort = TRUE) 

least_counts <- tidy_articles %>%
  semi_join(names)%>%
  count(news_site, word, sort = TRUE) %>%
  group_by(news_site)%>%
  summarise(least_popular_candidate = min(n))%>%
  ungroup()

least_popular_candidates <- counts%>%semi_join(least_counts,by=c("n"="least_popular_candidate"))%>%
  rename(number_of_mentions = n)
least_popular_candidates

least_popular_candidates %>% ggplot(aes(x=word, y=number_of_mentions, fill=news_site))+
  geom_col(position = "dodge")+
  facet_wrap(~political_lean)+coord_flip()+
  labs(x="Candidate name",
       y="Number of times mentioned in the news site",
       title="Least popular candidate per each news site running for office \nin the next election")+
  ggsave("Least_Popular_candidates1.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")




##Splitting article data into bigrams:
article_bigrams <- articles%>%
  group_by(news_site, political_lean)%>%
  unnest_tokens(bigram, article_text, token = "ngrams", n=2)%>%
  select(year:date, political_lean:bigram)

##Checking most common bigrams in the article text:
article_bigrams %>% count(bigram, sort = TRUE)

##Removing stop words from the bigrams:
article_bigrams <- article_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")
urw <- tibble(word=c('â','âo','fox','breitbart','app','getty'))
article_bigrams <- article_bigrams %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  anti_join(urw, by = c('word1'='word'))%>%
  anti_join(urw, by = c('word2'='word'))
  

##Counting the most common bigrams now:
article_bigrams %>% count(word1, word2, sort = TRUE)
##We can see that fox news uses 'fox news' alot. CNN articles had the bigram 'white house' the most
#which is not surprising, also 'donald trump' is a very common bigram in cnn's articles too.
#We can see that cnn has many articles with the bigram impeachment inquiry 

#Visualizing most common bigrams across news sites:
article_bigrams1 <- article_bigrams

article_bigrams1 <- article_bigrams1 %>%
  unite(bigram, word1, word2, sep = " ")

article_bigrams1 %>%
  filter(political_lean=="right")%>%
  count(news_site, bigram, sort = TRUE) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))%>% 
  group_by(news_site) %>%
  top_n(8) %>%
  ungroup() %>%
  ggplot(aes(reorder(bigram,-n), n, fill=political_lean)) +
  geom_col(show.legend = FALSE) +
  labs(x = "bigrams", y = "counts",title = "8 most common bigrams in right leaning news sites") +
  facet_wrap(~news_site, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("darkred"))+
  coord_flip()+
  ggsave("Most_Popular_bigrams_in_right1.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


article_bigrams1 %>%
  filter(political_lean=="left")%>%
  count(news_site, bigram, sort = TRUE) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))%>% 
  group_by(news_site) %>%
  top_n(8) %>%
  ungroup() %>%
  ggplot(aes(reorder(bigram,-n), n, fill=political_lean)) +
  geom_col(show.legend = FALSE) +
  labs(x = "bigrams", y = "counts",title = "8 most common bigrams in left leaning news sites") +
  facet_wrap(~news_site, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("darkblue"))+
  coord_flip()+
  ggsave("Most_Popular_bigrams_in_left1.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


article_bigrams1 %>%
  filter(political_lean=="centre")%>%
  count(news_site, bigram, sort = TRUE) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))%>% 
  group_by(news_site) %>%
  top_n(8) %>%
  ungroup() %>%
  ggplot(aes(reorder(bigram,-n), n, fill=political_lean)) +
  geom_col(show.legend = FALSE) +
  labs(x = "bigrams", y = "counts",title = "8 most common bigrams in centre leaning news sites") +
  facet_wrap(~news_site, ncol = 3, scales = "free") +
  scale_fill_manual(values = c("purple"))+
  coord_flip()+
  ggsave("Most_Popular_bigrams_in_centre1.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


##Most commonly associated word with "trump" on cnn:
article_bigrams %>%
  filter(news_site=="cnn" & word1 == "trump") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "trump's" on cnn:
article_bigrams %>%
  filter(news_site=="cnn" & word1 == "trump's") %>%
  count(word1, word2, sort=TRUE)

##Most commonly associated word with "trump" on fox:
article_bigrams %>%
  filter(news_site=="fox" & word1 == "trump") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "trump's" on fox:
article_bigrams %>%
  filter(news_site=="fox" & word1 == "trump's") %>%
  count(word1, word2, sort=TRUE)

##Most commonly associated word with "trump" on bbc:
article_bigrams %>%
  filter(news_site=="bbc" & word1 == "trump") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "trump's" on bbc:
article_bigrams %>%
  filter(news_site=="bbc" & word1 == "trump's") %>%
  count(word1, word2, sort=TRUE)



##Most commonly associated words associated with trump's competition:
##Most commonly associated word with "warren" on cnn:
article_bigrams %>%
  filter(news_site=="cnn" & word1 == "warren") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "warren's" on cnn:
article_bigrams %>%
  filter(news_site=="cnn" & word1 == "warren's") %>%
  count(word1, word2, sort=TRUE)

##Most commonly associated word with "warren" on fox:
article_bigrams %>%
  filter(news_site=="fox" & word1 == "warren") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "warren's" on fox:
article_bigrams %>%
  filter(news_site=="fox" & word1 == "warren's") %>%
  count(word1, word2, sort=TRUE)

##Most commonly associated word with "warren" on bbc:
article_bigrams %>%
  filter(news_site=="bbc" & word1 == "warren") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "warren's" on bbc:
article_bigrams %>%
  filter(news_site=="bbc" & word1 == "warren's") %>%
  count(word1, word2, sort=TRUE)


##Most commonly associated word with "biden" on cnn:
article_bigrams %>%
  filter(news_site=="cnn" & word1 == "biden") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "biden's" on cnn:
article_bigrams %>%
  filter(news_site=="cnn" & word1 == "biden's") %>%
  count(word1, word2, sort=TRUE)
#Biden's son comes up alot

##Most commonly associated word with "biden" on fox:
article_bigrams %>%
  filter(news_site=="fox" & word1 == "biden") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "biden's" on fox:
article_bigrams %>%
  filter(news_site=="fox" & word1 == "biden's") %>%
  count(word1, word2, sort=TRUE)
#Biden's son comes up alot

##Most commonly associated word with "biden" on bbc:
article_bigrams %>%
  filter(news_site=="bbc" & word1 == "biden") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "biden's" on bbc:
article_bigrams %>%
  filter(news_site=="bbc" & word1 == "biden's") %>%
  count(word1, word2, sort=TRUE)
#Biden's son comes up alot



##Most commonly associated word with "sanders" on cnn:
article_bigrams %>%
  filter(news_site=="cnn" & word1 == "sanders") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "sanders's" on cnn:
article_bigrams %>%
  filter(news_site=="cnn" & word1 == "sanders's") %>%
  count(word1, word2, sort=TRUE)

##Most commonly associated word with "sanders" on fox:
article_bigrams %>%
  filter(news_site=="fox" & word1 == "sanders") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "sanders'" on fox:
article_bigrams %>%
  filter(news_site=="fox" & word1 == "sanders's") %>%
  count(word1, word2, sort=TRUE)

##Most commonly associated word with "sanders" on bbc:
article_bigrams %>%
  filter(news_site=="bbc" & word1 == "sanders") %>%
  count(word1, word2, sort=TRUE)
##Most commonly associated word with "sanders's" on bbc:
article_bigrams %>%
  filter(news_site=="bbc" & word1 == "sanders's") %>%
  count(word1, word2, sort=TRUE)



##Authors of the articles:
authorsart <- articles %>% select(article_author)
#Single names of the authors:
#Taking only authors who wrote more than 9 articles:
authorsart <- authorsart %>% count(article_author, sort = TRUE)
authorsart <-authorsart %>% 
  separate(article_author, c('fname','lname'), sep = " ")%>% 
  select(lname)%>%
  rename(word=lname)


