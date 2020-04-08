library(randomForest)
library(dplyr)
library(scorecard)
library(caret)

#################################################
#using IV and random forest for imp feature
#################################################
rm_model1 <- randomForest(bounced_1 ~., data = sample_train, ntree = 501 ,mtry = 40 ,importance = TRUE)#default trees uses = 500, splits=26
importance(rm_model2)
varImpPlot(rm_model2)

#Sample data
data('germancredit')
iv = iv(data, y = 'creditability') %>%
  as_tibble() %>%
  mutate( info_value = round(info_value, 3) ) %>%
  arrange( desc(info_value) )

iv
#############
#WOE Bins
#############
bins = woebin(data, y = 'creditability')
bins$duration_in_month 
woebin_plot(bins$duration_in_month)

###########
#Apply bins
###########
data_woe = woebin_ply( data, bins ) %>%
  as_tibble()

####################################################
#Build model using new woe values
#########################################################
model_woe <- glm(bounced_1~.,family = binomial(link='logit'),data=data_woe)
summary(model_woe)

predict <- predict(model_woe, newdata = data_woe, type = "response")
predict <- ifelse(predict > 0.5 , 1,0)
predict <- as.factor(predict)
confusionMatrix(data = predict, reference = data_woe$bounced_1)

predict_woe <- predict(model_woe)

data_woe$predict_woe <- predict_woe

points0 = 800
odds0 = 50
pdo = 20

card = scorecard(bins, model_woe
                 , points0 = points0 
                 , odds0 = 1/odds0 # scorecard wants the inverse
                 , pdo = pdo)

sc = scorecard_ply(sample, card )

card[[1]]
data_all$score <- sc[[1]]
sample_test$score <- sc[[1]]
sample$score <- sc[[1]]

table(data_all$score)
