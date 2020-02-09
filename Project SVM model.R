library(topicmodels)
library(tm)
library(textdata)
library(caret)
#BUILDING AN SVM CLASSIFICATION MODEL:
svm_articles <- articles1 %>%
  select(political_lean, article_text)%>%
  filter(political_lean=="left" | political_lean=="right")

tiar <- svm_articles %>%
  unnest_tokens(word, article_text)%>%
  anti_join(stop_words)%>%
  anti_join(fw)%>%
  anti_join(authorsart)%>%
  count(word, sort = TRUE)%>%
  filter(n>20)

#Filtering out the words that appear less than 10 times and stop words article wise:
tilear <- svm_articles%>%
  mutate(article = 1:length(article_text))%>%
  unnest_tokens(word, article_text)%>%
  anti_join(stop_words)%>%
  anti_join(fw)%>%
  anti_join(authorsart)%>%
  inner_join(tiar, by=c('word'='word'))%>%
  mutate(label = ifelse(political_lean=="left",0,1))%>%
  select(article, word, label)%>%
  group_by(article)%>%
  count(word)%>%
  cast_dtm(article, word, n)

tilearla <- svm_articles%>%
  mutate(article = 1:length(article_text))%>%
  unnest_tokens(word, article_text)%>%
  anti_join(stop_words)%>%
  anti_join(fw)%>%
  anti_join(authorsart)%>%
  inner_join(tiar, by=c('word'='word'))%>%
  mutate(label = ifelse(political_lean=="left",0,1))%>%
  group_by(article)%>%
  summarise(label=mean(label))%>%
  select(label)

#Coercing tilear to a matrix:
learti <- as.matrix(tilear)
learlati <- cbind(learti, tilearla)
learlati <- learlati[, !duplicated(colnames(learlati), fromLast = TRUE)]


#Fitting an SVM model on the DTM:
set.seed(321)
partitionsvm <- createDataPartition(learlati$label, p=0.8, list = FALSE)

#Training and test sets:
data_df_train <- learlati[partitionsvm,]%>% as.matrix()%>%as.data.frame()
data_df_test <- learlati[-partitionsvm,]%>% as.matrix()%>%as.data.frame()

#Training data:
response_train <- learlati$label[partitionsvm]

#Without using a resampling method:
trctrl <- trainControl(method = "none")

#Classification using SVM:
data_df_train <- data_df_train[, !duplicated(colnames(data_df_train))]
data_df_train1 <- data_df_train %>% select(-label)

svm_mod <- train(x= data_df_train1, 
                 y= as.factor(response_train),
                 method = "svmLinearWeights2",
                 trControl = trctrl,
                 tuneGrid = data.frame(cost =1,
                                       Loss =0,
                                       weight= 1))

#Removing the class label while predicting on the test set:
data_df_test1 <- data_df_test %>% select(-label)

#Prediction on the test set:
svm_pred <- predict(svm_mod, 
                    newdata = data_df_test1)

#Confusion matrix to find out the accuracy:
svm_cm <- confusionMatrix(svm_pred, as.factor(learlati[-partitionsvm, ]$label))
svm_cm

#The accuracy of the model is 92.72%.



