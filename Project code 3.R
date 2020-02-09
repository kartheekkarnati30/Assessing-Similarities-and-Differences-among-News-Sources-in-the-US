library(tidytext)

##Splitting article data into bigrams:
article_bigramss <- articles%>%
  group_by(news_site, political_lean)%>%
  unnest_tokens(bigram, article_text, token = "ngrams", n=2)%>%
  select(year:date, political_lean:bigram, -article_text2)%>%
  separate(bigram, into = c("word1","word2"), sep = " ")%>%
  anti_join(stop_words, by = c('word2'='word'))

##Analyzing commonly negated words:
negation_words <- c("not", "no", "never", "without")
article_neg <- article_bigramss %>%
  filter(word1 %in% negation_words) %>%
  inner_join(get_sentiments("afinn"), by = c("word2" = "word")) %>%
  count(word1, word2, value, sort = TRUE) %>%
  ungroup()

article_neg %>%
  arrange(desc(n)) %>%
  mutate(word2 = reorder(word2, n)) %>%
  group_by(word1) %>%
  top_n(5) %>%
  ggplot(aes(word2, n)) +
  geom_col(aes(fill=political_lean), position = "dodge") +
  facet_wrap(~word1, scales="free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x="words", y="count", 
       title="Top 5 most commonly negated words in news articles")
  

##Analyzing sentiments using nrc:
nrc <- get_sentiments("nrc")

#Different sentiments in nrc:
nrc %>% count(sentiment, sort = TRUE)

##FEAR:
namestbf <- tibble(word=c('hunter','trump','john'))
fear <- nrc %>% filter(sentiment=="fear")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf, by = c('word1'='word'))%>%
  inner_join(fear, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with fear in articles\n
       red -> right, blue -> left and purple -> centre lean')+
  ggsave("Wordsassocfear3.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")

  
#POSITIVE:
positive <- nrc %>% filter(sentiment=="positive")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf, by = c('word1'='word'))%>%
  inner_join(positive, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with positive sentiment in articles\n
       red -> right, blue -> left, purple -> centre lean')

##NEGATIVE:
negative<- nrc %>% filter(sentiment=="negative")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf, by = c('word1'='word'))%>%
  inner_join(negative, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with negative sentiment in articles\n
       red -> right, blue -> left, purple -> centre lean')

##ANGER:
anger<- nrc %>% filter(sentiment=="anger")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf, by = c('word1'='word'))%>%
  inner_join(anger, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with anger in articles\n
       red -> right, blue -> left, purple -> centre lean')

#TRUST:
trust<- nrc %>% filter(sentiment=="trust")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf, by = c('word1'='word'))%>%
  inner_join(trust, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with trust in articles\n
       red -> right, blue -> left, purple -> centre lean')

#JOY:
namestbf1 <- tibble(word=c('hunter','trump','john','white'))
joy<- nrc %>% filter(sentiment=="joy")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf1, by = c('word1'='word'))%>%
  inner_join(joy, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with joy in articles\n
       red -> right, blue -> left, purple -> centre lean')

##SADNESS:
sadness<- nrc %>% filter(sentiment=="sadness")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf, by = c('word1'='word'))%>%
  inner_join(sadness, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with sadness in articles\n
       red -> right, blue -> left, purple -> centre lean')

##DISGUST:
namestbf2 <- tibble(word=c('hunter','trump','john','white','abortion'))
disgust<- nrc %>% filter(sentiment=="disgust")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf2, by = c('word1'='word'))%>%
  inner_join(disgust, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with disgust in articles\n
       red -> right, blue -> left, purple -> centre lean')

#ANTICIPATION:
anticipation<- nrc %>% filter(sentiment=="anticipation")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf2, by = c('word1'='word'))%>%
  inner_join(anticipation, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with anticipation in articles\n
       red -> right, blue -> left, purple -> centre lean')

#SURPRISE:
surprise<- nrc %>% filter(sentiment=="surprise")
article_bigramss %>% 
  anti_join(stop_words, by = c('word1'='word'))%>%
  anti_join(namestbf, by = c('word1'='word'))%>%
  inner_join(surprise, by=c('word1'='word'))%>%
  count(news_site, word1, sort = TRUE)%>%
  mutate(word1 = reorder(word1, n)) %>%
  group_by(news_site) %>%
  top_n(10) %>%
  ggplot(aes(x=word1, y=n)) +
  geom_col(show.legend=FALSE,aes(fill=political_lean)) +
  facet_wrap(~news_site, scales = "free") +
  coord_flip()+
  scale_fill_manual(values = c("purple","darkblue","darkred"))+
  labs(x='word',y='count',
       title='Top 10 most associated words with surprise in articles\n
       red -> right, blue -> left, purple -> centre lean')


