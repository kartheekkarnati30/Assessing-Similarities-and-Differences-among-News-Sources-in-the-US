library(igraph)
library(ggraph)

##Word association:
##Most common bigrams in RIGHT WING news sites:
urw1 <- tibble(word = c('pam','trumpâ','adam','pamkeynen',25,'ianhanchett',
                        'https','weâ'))
article_bigrams2<-article_bigrams
article_graph_right <- article_bigrams2 %>%
  anti_join(urw1, by = c('word1'='word'))%>%
  anti_join(urw1, by = c('word2'='word'))%>%
  filter(political_lean=="right")%>%
  count(word1, word2, sort=TRUE) %>%
  ungroup()%>%
  select(-news_site,-political_lean)%>%
  filter(n > 155) %>%
  graph_from_data_frame()
article_graph_right

ggraph(article_graph_right,
       layout="igraph",
       algorithm="kk") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.85)+
  labs(title = "Most common bigrams in right leaning news sites")+
  ggsave("Wordassocright2.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")

#Ocasio Cortez, quid pro quo, Volodymyr Zelensky, Hunter Biden, 
#Whistleblower complaint, impeachment inquiry, illegal aliens, military
#aid, climate change, american people

##Most common bigrams in LEFT WING news sites:
urw2 <- tibble(word = c('rupar','youtube','imageobject','schema.org','http',
                        'rameswaram','theyâ','weâ','iâ','url',
                        25,'cnnnext','spotify','podcasts','cillizza',
                        'cdn.cnn.com','jpg','click','dam'))
article_graph_left <- article_bigrams2 %>%
  anti_join(urw2, by = c('word1'='word'))%>%
  anti_join(urw2, by = c('word2'='word'))%>%
  filter(political_lean=="left")%>%
  count(word1, word2, sort=TRUE) %>%
  ungroup()%>%
  select(-news_site,-political_lean)%>%
  filter(n > 220) %>%
  graph_from_data_frame()
article_graph_left

ggraph(article_graph_left,
       layout="igraph",
       algorithm="kk") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.7)+
  labs(title = "Most common bigrams in left leaning news sites")+
  ggsave("Wordassocleft2.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


#quid pro quo, Hunter Biden, whistleblower complaint, impeachment inquiry,
#mass shootings, gun violence, El Paso, Volodymyr Zelensky, Mike Pompeo,
#climate change

##Most common bigrams in CENTRE LEAN news sites:
urw3 <- tibble(word = c('trumpâ',25))
article_graph_centre <- article_bigrams2 %>%
  anti_join(urw3, by = c('word1'='word'))%>%
  anti_join(urw3, by = c('word2'='word'))%>%
  filter(political_lean=="centre")%>%
  count(word1, word2, sort=TRUE) %>%
  ungroup()%>%
  select(-news_site,-political_lean)%>%
  filter(n > 40) %>%
  graph_from_data_frame()
article_graph_centre

ggraph(article_graph_centre,
       layout="igraph",
       algorithm="kk") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  labs(title = "Most common bigrams in centre leaning news sites")+
  ggsave("Wordassoccentre2.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


#tax returns, impeachment inquiry, al bhagdadi, middle class, gun control,
#Hunter Biden, Volodymyr Zelensky.



##Most common bigrams in news sites in the month of AUGUST:
urw4 <- tibble(word = c('theyâ','iâ','pam','pamkeynen','weâ','rupar'))
article_graph_august <- article_bigrams2 %>%
  anti_join(urw4, by = c('word1'='word'))%>%
  anti_join(urw4, by = c('word2'='word'))%>%
  filter(month == '08')%>%
  count(word1, word2, sort=TRUE) %>%
  ungroup()%>%
  select(-news_site,-political_lean)%>%
  filter(n > 65) %>%
  graph_from_data_frame()
article_graph_august

ggraph(article_graph_august,
       layout="igraph",
       algorithm="kk") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.7)+
  labs(title = "Most common bigrams in news sites in the month of August")+
  ggsave("Wordassocaugust2.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


#No mention of impeachment or impeachment inquiry in the most commom 
#bigrams in the news sites. Hong Kong comes up alot in the articles of 
#august. The trade war between US and China is also mentioned alot.
#Gun control, gun violence and gun control are common terms in the articles
#of August and so is mass shootings. The term neo nazis features alot in
#the articles of August. El Paso, Texas comes up alot in the news articles
#on account of the shooting that took place in a Walmart store in El Paso.


##Most common bigrams in news sites in the month of SEPTEMBER:
urw5 <- tibble(word = c('jpg','rameswaram','cdn.cnn.com','http',
                        'schema.org','cnnnext','imageobject',
                        'dam'))
article_graph_september <- article_bigrams2 %>%
  anti_join(urw5, by = c('word1'='word'))%>%
  anti_join(urw5, by = c('word2'='word'))%>%
  filter(month == '09')%>%
  count(word1, word2, sort=TRUE) %>%
  ungroup()%>%
  select(-news_site,-political_lean)%>%
  filter(n > 100) %>%
  graph_from_data_frame()
article_graph_september

ggraph(article_graph_september,
       layout="igraph",
       algorithm="kk") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 0.5)+
  labs(title = "Most common bigrams in news sites in the month of September")+
  ggsave("Wordassocsept2.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


#Month of September saw the beginning of impeachment of the president 
#being a possibility because of the whistleblower compplaint. The names 
#Joe Biden, Hunter Biden and Volodymyr Zelensky come up alot in light of the
#allegations that President Trump asked the Ukranian President to look 
#into the affiars of Hunter Biden and Joe Biden. The name Ocasio Cortez 
#also comes up alot and so does Bill de Blasio. 


##Most common bigrams in news sites in the month of OCTOBER:
urw6 <- tibble(word = c('schema.org','dam','cnnnext','rameswaram',
                        'schiff','podcasts','jpg','http','imageobject',
                        'trumpâ',25,'url','illing','iâ'))
article_graph_october <- article_bigrams2 %>%
  anti_join(urw6, by = c('word1'='word'))%>%
  anti_join(urw6, by = c('word2'='word'))%>%
  filter(month == '10')%>%
  count(word1, word2, sort=TRUE) %>%
  ungroup()%>%
  select(-news_site,-political_lean)%>%
  filter(n > 100) %>%
  graph_from_data_frame()
article_graph_october

ggraph(article_graph_october,
       layout="igraph",
       algorithm="kk") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  labs(title = "Most common bigrams in news sites in the month of October")+
  ggsave("Wordassococtober2.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")


#The impeachment phenomenon carried on in to the month of October and so 
#did the common terms. We see the new term quid pro quo featuring alot in
#the articles of October because it was alleged that President Trump was
#involved in a quid pro quo situation with President Zelensky. Mike Pompeo
#and Rudy Giuliani were popular figures too, in news in October. Northern
#Syria is also a common term and this is because Trump ordered American
#Troops to withdraw from Northern Syria.

##Most common bigrams in new sites in the month of NOVEMBER:
article_graph_november <- article_bigrams2 %>%
  anti_join(urw6, by = c('word1'='word'))%>%
  anti_join(urw6, by = c('word2'='word'))%>%
  filter(month == '11')%>%
  count(word1, word2, sort=TRUE) %>%
  ungroup()%>%
  select(-news_site,-political_lean)%>%
  filter(n > 40) %>%
  graph_from_data_frame()
article_graph_november

ggraph(article_graph_november,
       layout="igraph",
       algorithm="kk") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)+
  labs(title = "Most common bigrams in news sites in the month of November")+
  ggsave("Wordassocnovem2.jpeg",device="jpg",
         path="D:/NEU/Fall 2019/DS5110 Data Management/Project/Plots/")
#Gordon Sondland's testimony.




