# 1. INTRODUCTION
## 1.1. Project goal

library(tidyverse)
library(stringr)
library(purrr)
library(caret)
library(ggplot2)
library(corrplot)
library(forcats)
library(rattle)
library(xgboost)
library(klaR)

url_main <- "https://raw.githubusercontent.com/beatrizeg/Wish-Units-Solds/main/summer-products-with-rating-and-performance_2020-08.csv"
dest_file <- "data/main.csv"
download.file(url_main, destfile = dest_file)
main <- read_csv("data/main.csv")
save(main, file = "rdas/main.rda")

url_cat <- "https://raw.githubusercontent.com/beatrizeg/Wish-Units-Solds/main/unique-categories.sorted-by-count.csv"
dest_file_cat <- "data/cat.csv"
download.file(url_cat, destfile = dest_file_cat)
cat <- read_csv("data/cat.csv")
save(cat, file = "rdas/cat.rda")

load("rdas/main.rda")
load("rdas/cat.rda")

main <- as.data.frame(main)
cat <- as.data.frame(cat)

## 1.2. Inspecting the dataset

dim(main)
summary(main)

#check for NAs
apply(main, 2, function(x) any(is.na(x)))

#NAs in * ratings and has_urgency_banner are substituted by 0
main <- main %>% mutate(rating_five_count=ifelse(is.na(rating_five_count),0,rating_five_count),
                        rating_four_count=ifelse(is.na(rating_four_count),0,rating_four_count),
                        rating_three_count=ifelse(is.na(rating_three_count),0,rating_three_count),
                        rating_two_count=ifelse(is.na(rating_two_count),0,rating_two_count),
                        rating_one_count=ifelse(is.na(rating_one_count),0,rating_one_count),
                        has_urgency_banner=ifelse(is.na(has_urgency_banner),0,has_urgency_banner))

apply(main, 2, function(x) any(is.na(x))) #check again that substitution was made correctly

#check variability for product_color and make adjustments
table(main$product_color) %>% sort(decreasing = TRUE)

main <- main %>% mutate(product_color=
                          as.factor(case_when(
                            str_detect(product_color, "&") ~ "two colors",
                            str_detect(product_color, "blue") ~ "blue",
                            str_detect(product_color, "navy") ~ "blue",
                            str_detect(product_color, "green") ~ "green",
                            str_detect(product_color, "red") ~ "red",
                            str_detect(product_color, "gray") ~ "grey",
                            str_detect(product_color, "grey") ~ "grey",
                            str_detect(product_color, "coffee") ~ "brown",
                            str_detect(product_color, "brown") ~ "brown",
                            str_detect(product_color, "pink") ~ "pink",
                            str_detect(product_color, "rose") ~ "pink",
                            str_detect(product_color, "black") ~ "black",
                            str_detect(product_color, "white") ~ "white",
                            str_detect(product_color, "purple") ~ "purple",
                            str_detect(product_color, "orange") ~ "orange",
                            str_detect(product_color, "multicolor") ~ "multicolor",
                            str_detect(product_color, "yellow") ~ "yellow",
                            TRUE ~ "other")))

main %>% ggplot(aes(product_color))+geom_bar()

#check variability for product_variation_size_id and make adjustments
table(main$product_variation_size_id) %>% sort(decreasing = TRUE)

main <- main %>% mutate(product_variation_size_id=
                          as.factor(case_when(product_variation_size_id=="XXXS" ~ "XXXS",
                                   product_variation_size_id=="XXS" ~ "XXS",
                                   product_variation_size_id=="XS" | 
                                     product_variation_size_id=="XS." |
                                     product_variation_size_id=="SIZE XS" |
                                     product_variation_size_id=="Size-XS" |
                                     product_variation_size_id=="Size-XS" ~ "XS",
                                   product_variation_size_id=="S" | 
                                     product_variation_size_id=="S." |
                                     product_variation_size_id=="s" |
                                     product_variation_size_id=="Size S" |
                                     product_variation_size_id=="Size-S" |
                                     product_variation_size_id=="size S" |
                                     product_variation_size_id=="Size S." |
                                     product_variation_size_id=="S Pink" |
                                     product_variation_size_id=="Suit-S"~ "XS",
                                   product_variation_size_id=="M" | 
                                     product_variation_size_id=="M."~ "M",
                                   product_variation_size_id=="L" | 
                                     product_variation_size_id=="SizeL" ~ "L",
                                   product_variation_size_id=="XL"   ~ "XL",
                                   product_variation_size_id=="XXL" | 
                                     product_variation_size_id=="2XL" ~ "XXL",
                                   product_variation_size_id=="XXXL" ~ "XXXL",
                                   product_variation_size_id=="4XL" ~ "4XL",
                                   TRUE ~ "other")))
                         
table(main$product_variation_size_id) %>% sort(decreasing = TRUE)

main %>% ggplot(aes(product_variation_size_id))+geom_bar()

#check variability for origin_country and make adjustments
table(main$origin_country) %>% sort(decreasing = TRUE)

main <- main %>% mutate(
  origin_country=as.factor(case_when(
    origin_country == "CN" | origin_country == "US" ~ origin_country,
    TRUE ~ "other"
  )))

main %>% ggplot(aes(origin_country))+geom_bar()

#check variability for shipping_option_name and make adjustments
#this is ommited in the report as this feature variability is 0
table(main$origin_country) %>% sort(decreasing = TRUE)

main <- main %>% mutate(
  shipping_option_name=as.factor(case_when(
    shipping_option_name == "Livraison standard" | shipping_option_name == "Standard Shipping" ~ "Standard Shipping",
    TRUE ~ "Standard Shipping"
  )))

#Check only one currency in dataset (EUR)
n_distinct(main$currency_buyer)

#check variability for units_sold and make adjustments
table(main$units_sold) %>% sort(decreasing = TRUE)

main <- main %>% mutate(units_sold = ifelse(units_sold<10, 10, units_sold))
main %>% ggplot(aes(units_sold))+geom_bar()

#delete duplicated rows via product_id
n_distinct(main$product_id)
#when examining duplicates as this, we see that one of the rows should be removed as data is the same except in any categorical value such as has_urgency_banner, which was probably updated during the month
main %>% group_by(product_id) %>% summarize(n=n()) %>% arrange(desc(n))
main %>% filter(product_id=="5577faf03cef83230c39d0c3")
main <- distinct(main, product_id, .keep_all = TRUE) #delete duplicated rows

# 2.1.2.
#convert variables to logical or factor classes
main <- main %>% mutate(currency_buyer=as.factor(currency_buyer),
                        badges_count=as.factor(badges_count),
                        uses_ad_boosts=as.logical(uses_ad_boosts),
                        badge_local_product=as.logical(badge_local_product),
                        badge_product_quality=as.logical(badge_product_quality),
                        badge_fast_shipping=as.logical(badge_fast_shipping),
                        shipping_option_price=as.factor(shipping_option_price),
                        shipping_is_express=as.logical(shipping_is_express),
                        has_urgency_banner=as.logical(has_urgency_banner),
                        merchant_has_profile_picture=as.logical(merchant_has_profile_picture),
                        inventory_total=as.factor(inventory_total))
              
#2.1.3 Introducing tags model
#tags model
cat <- cat %>% mutate(cat_n =
                        case_when(count>=1000 ~ 4,
                                  count<1000 & count>=500 ~ 3,
                                  count<500 & count>=200 ~ 2,
                                  count < 200 ~ 1,
                                  TRUE ~ 0))
main_tags <- str_split(main$tags, ",", simplify = TRUE)

for (i in 1:41){
main_tags[,i] <- with(cat, cat_n[match(main_tags[,i], keyword)])
} #next step change to numeric values 

main_tags <- as.data.frame(main_tags)
main_tags[] <- lapply(main_tags, function(x) as.numeric(as.character(x))) #convert to numeric values
main_tags <- as.data.frame(main_tags)
main_tags <- main_tags %>% mutate(n_tags = rowSums(main_tags, na.rm=TRUE)) %>% #we sum the number of tags for each row
  dplyr::select(n_tags) 

main_m <- bind_cols(main, main_tags) #we bind the main table with the main_tags to include new column with number of tags

main_m <- main_m %>% dplyr::select(price, retail_price, units_sold, uses_ad_boosts, rating, rating_count, 
                 rating_five_count, rating_four_count, rating_three_count, rating_two_count, rating_one_count,
                 badges_count, badge_local_product, badge_product_quality, badge_fast_shipping,
                 product_color, product_variation_size_id, product_variation_inventory,
                 shipping_option_price, shipping_is_express, countries_shipped_to, inventory_total,
                 has_urgency_banner, origin_country, merchant_rating_count, merchant_rating,
                 merchant_has_profile_picture, product_id, n_tags)

#check for predictors that do not vary across sample
no_var <- nearZeroVar(main_m, saveMetrics = TRUE)
no_var[no_var[,"zeroVar"] + no_var[,"nzv"] > 0, ] 
#as percentUnique is not high in any predictor except for inventory_total,
#no deletions as not having inventory might have a big impact across sales.

#we add column with difference % between price and retail_price
main_m <- main_m %>% mutate(perc_price=(price-retail_price)/retail_price)

main_m.cor <- main_m %>% mutate(units_sold=as.numeric(units_sold)) %>%
  select_if(is.numeric) %>%
  cor(.)

corrplot(main_m.cor)

main_m.chisq <- main_m %>%
  select_if(function(col) is.character(col) | 
              is.factor(col) | is.logical(col) |
              all(col == .$units_sold)) %>% select(-product_id)

columns <- 1:ncol(main_m.chisq)
vars <- names(main_m.chisq)[columns]
out <-  apply( combn(columns,2),2,function(x){
  chisq.test(table(main_m.chisq[,x[1]],main_m.chisq[,x[2]]),correct=F)$p.value
})

out <- cbind(as.data.frame(t(combn(vars,2))),out)
out <- out %>% filter(V1=="units_sold") %>% filter(out<0.05) %>%arrange(out)


#units_sold vs product_color
main_m %>% 
ggplot(aes(fct_infreq(product_color), units_sold)) + geom_bar(stat = "identity")

#units_sold vs product_size_id
main_m %>% 
  ggplot(aes(fct_infreq(product_variation_size_id), units_sold)) + geom_bar(stat = "identity")

#units_sold vs price
main_m %>% 
  ggplot(aes(price, units_sold)) + geom_smooth()

#units_sold vs perc_price
main_m %>% 
  ggplot(aes(perc_price, units_sold)) + geom_smooth()

#CHECK!!
#units_sold vs uses_ad_boost
main_m %>% 
  ggplot(aes(uses_ad_boosts, units_sold)) + geom_violin()

levels <- c("X10", "X50", "X100", "X1000", "X5000", "X10000", "X20000", "X50000", "X1e05")
main_p <- main_m %>% mutate(units_sold = factor(units_sold, levels=levels))


#MACHINE LEARNING
#split into train and test set
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(main_m$units_sold, times=1, p=0.15, list=FALSE) #Test set is 15% of our data
train_set <- main_p[-test_index,] %>% select(-product_id)
test_set <- main_p[test_index,] %>% select(-product_id) 


#2 gam loess - #check optimization
control <- trainControl(method = "repeatedcv", number = 3, repeats = 4, savePredictions = "all")
grid_loess <- expand.grid(span=seq(0.2,0.9,0.2), degree=seq(1,3,1))
train_loess <- train(units_sold ~ ., data=train_set, method="gamLoess", trControl=control, tuneGrid=grid_loess)
span <- train_loess$bestTune$span
varImp(train_loess) #error
ggplot(train_loess, highlight = TRUE) #no tuning parameters
y_loess <- predict(train_loess, test_set, type="raw")
acc_loess <- confusionMatrix(y_loess, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="GamLoess",
                                    Accuracy = acc_loess))


#4 k-nearest neighbor
set.seed(2007, sample.kind = "Rounding")
control <- trainControl(method = "repeatedcv", number=4, repeats=4)
train_knn <- train(units_sold ~ ., data=train_set, method="knn", tuneGrid = data.frame(k=seq(3, 71, 2)), trControl=control)
ggplot(train_knn, highlight = TRUE)
train_knn$results
train_knn$finalModel
k <- train_knn$bestTune
y_knn <- predict(train_knn, test_set, type="raw")
acc_knn <- confusionMatrix(y_knn, test_set$units_sold)$overall[['Accuracy']]
acc_results <- tibble(method = "KNN", Accuracy = acc_knn)


#5 Neural Network
set.seed(2007, sample.kind = "Rounding")
control <- trainControl(method = "repeatedcv", number=4, repeats=2)
grid_nnet1 <- expand.grid(size=seq(4,20,4), decay=seq(0.05, 0.5, 0.02))
train_nnet1 <- train(units_sold ~ ., data=train_set, method="nnet", trControl=control, tuneGrid=grid_nnet)
ggplot(train_nnet1, highlight = TRUE)
train_nnet1
train_nnet1$bestTune

grid_nnet2 <- expand.grid(size=seq(4,7,1), decay=seq(0.3, 0.5, 0.01))
train_nnet2 <- train(units_sold ~ ., data=train_set, method="nnet", trControl=control, tuneGrid=grid_nnet2)
ggplot(train_nnet2, highlight = TRUE)
train_nnet2
train_nnet2$bestTune

#Model chosen is train_nnet1 as it gets better accuracy (size=4 and decay=0.47)
y_nnet <- predict(train_nnet1, test_set, type="raw")
acc_nnet <- confusionMatrix(y_nnet, test_set$units_sold)$overall[['Accuracy']]
acc_results <- tibble(method = "NNET", Accuracy = acc_nnet)


#7.1 Classification trees regular values
set.seed(2007, sample.kind = "Rounding")
control <- trainControl(method = "cv", number=4, classProbs = TRUE)
train_rpart1 <- train(units_sold ~ ., data=train_set, method="rpart", trControl=control)
ggplot(train_rpart1, highlight = TRUE)
fancyRpartPlot(train_rpart1$finalModel, sub = NULL)
plot(train_rpart1$finalModel, margin = 0.3) 
text(train_rpart1$finalModel, cex = 0.4)
train_rpart1$finalModel$variable.importance
y_rpart1 <- predict(train_rpart1, test_set, type="raw")
acc_rpart1 <- confusionMatrix(y_rpart1, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Regression Trees not optimised",
                                    Accuracy = acc_rpart1))

#7 Regression trees optimising cp and minsplit
train_rpart <- train(units_sold ~ ., data=train_set, method="rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), control=rpart::rpart.control(minsplit=15))
ggplot(train_rpart, highlight = TRUE)
cp <- train_rpart$bestTune$cp
minsplit <- seq(10, 40, len=5)
acc <- sapply(minsplit, function(ms){
  train(units_sold ~ ., method = "rpart", data = train_set, tuneGrid = data.frame(cp=cp),
        control=rpart::rpart.control(minsplit=ms))$results$Accuracy })
qplot(minsplit, acc)
minsplit <- minsplit[which.max(acc)]
train_rpart2 <- train(units_sold ~ ., data=train_set, method="rpart", tuneGrid = data.frame(cp = cp), control=rpart::rpart.control(minsplit=minsplit))
fancyRpartPlot(train_rpart2$finalModel, sub = NULL)
plot(train_rpart2$finalModel, margin = 0.3) 
text(train_rpart2$finalModel, cex = 0.4)
train_rpart2$finalModel$variable.importance
y_rpart2 <- predict(train_rpart2, test_set, type="raw")
acc_rpart2 <- confusionMatrix(y_rpart2, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Regression Trees Optimized",
                                    Accuracy = acc_rpart2))

#8.1 Random Forest default values
train_rf <- train(units_sold ~ ., data=train_set, method="rf")
ggplot(train_rf, highlight = TRUE)
mtry <- train_rf$bestTune #ERROR!!

y_rf0 <- predict(train_rf, test_set, type="raw")
acc_rf0 <- confusionMatrix(y_rf0, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Random Forest not optimized",
                                    Accuracy = acc_rf0))
#Optimize mtry
set.seed(1234, sample.kind = "Rounding")
control_rf <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
grid_rf <- expand.grid(mtry=seq(4,28,2))
train_rf1 <- train(units_sold ~ ., data=train_set, method="rf", tuneGrid=grid_rf, trControl=control_rf)
ggplot(train_rf1, highlight = TRUE)
mtry <- train_rf1$bestTune$mtry

#8.2 Random Forest optimising minimum node size
grid_mtry <- expand.grid(mtry=mtry)
nodesize <- seq(1, 5, 2)
acc <- sapply(nodesize, function(ns){
  train(units_sold ~ ., method = "rf", data = train_set, tuneGrid = grid_mtry, trControl=control_rf,
        nodesize = ns)$results$Accuracy })
qplot(nodesize, acc)
nodesize <- nodesize[which.max(acc)]

train_rf2 <- train(units_sold ~ ., method = "rf", data = train_set, tuneGrid = grid_mtry, nodesize = nodesize)

#chosen train_rf2 as it provides highter accuracy on train_set than train_rf1
y_rf <- predict(train_rf2, test_set, type="raw")
acc_rf <- confusionMatrix(y_rf, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Random Forest optimized",
                                    Accuracy = acc_rf))

#9 XGBoost 
#optimize eta and max_depth 
grid_xgbm <- expand.grid(min_child_weight=c(nodesize), eta=c(0.02,0.01,0.005), nrounds=c(500), max_depth=c(6,8,10), gamma=0,
                         colsample_bytree=c(0.8), subsample=1)
set.seed(62, sample.kind = "Rounding")
control_xgbm <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm <- train(units_sold ~ ., method="xgbTree", data=train_set, trControl=control_xgbm, tuneGrid=grid_xgbm, verbose=TRUE)
eta <- train_xgbm$bestTune$eta
max_depth <- train_xgbm$bestTune$max_depth
ggplot(train_xgbm, highlight = TRUE)

#optimize nrounds
grid_xgbm <- expand.grid(min_child_weight=c(nodesize), eta=c(eta), nrounds=c(500,1000,1500,2000), max_depth=c(max_depth), gamma=0,
                         colsample_bytree=c(0.8), subsample=1)
set.seed(62, sample.kind = "Rounding")
control_xgbm <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm <- train(units_sold ~ ., method="xgbTree", data=train_set, trControl=control_xgbm, tuneGrid=grid_xgbm, verbose=TRUE)
nrounds <- train_xgbm$bestTune$nrounds
ggplot(train_xgbm, highlight = TRUE)

#optimize nodesize
grid_xgbm <- expand.grid(min_child_weight=c(1,3,5), eta=c(eta), nrounds=c(nrounds), max_depth=c(max_depth), gamma=0,
                         colsample_bytree=c(0.8), subsample=1)
set.seed(62, sample.kind = "Rounding")
control_xgbm <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm <- train(units_sold ~ ., method="xgbTree", data=train_set, trControl=control_xgbm, tuneGrid=grid_xgbm, verbose=TRUE)
ggplot(train_xgbm, highlight = TRUE)
nodesize <- train_xgbm$bestTune$min_child_weight

#run with optimized values
grid_xgbm <- expand.grid(min_child_weight=c(nodesize), eta=c(eta), nrounds=c(nrounds), max_depth=c(max_depth), gamma=0,
                         colsample_bytree=c(0.8), subsample=1)
control_xgbm <- trainControl(method = "cv", number=4, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm <- train(units_sold ~ ., method="xgbTree", data=train_set, tuneGrid=grid_xgbm, trControl=control_xgbm, verbose=TRUE)
train_xgbm$bestTune
xgbm_imp <- varImp(train_xgbm)
plot(xgbm_imp, top = 10)
y_xgbm <- predict(train_xgbm, test_set, type="raw")
acc_xgbm <- confusionMatrix(y_xgbm, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="XGBoost",
                                    Accuracy = acc_xgbm))

#10 H20
library(h2o)
main_h2o <- main_p %>% select(-product_id)
h2o.init()
data_h2o <- as.h2o(train_set)
test_h2o <- as.h2o(test_set)
automl_all <- h2o.automl(y=3, training_frame=data_h2o, max_runtime_secs=500,validation_frame = test_h2o,
                            seed=1, keep_cross_validation_predictions=TRUE)

automl_all_lb <- automl_all@leaderboard
print(automl_all_lb, n=nrow(automl_all_lb))
lb <- h2o.get_leaderboard(object = automl_all, extra_columns = 'ALL')
automl_all@leader

h2o_xgb <- h2o.performance(model = automl_all@leader, newdata = test_h2o)

pred <- h2o.predict(automl_all@leader, test_h2o)
summary(pred)
h2o.confusionMatrix(automl_all@leader, newdata = test_h2o)
h2o.varimp(automl_all@leader)
dev.off()

plot <- h2o.varimp_plot(automl_all@leader, num_of_features = 10)