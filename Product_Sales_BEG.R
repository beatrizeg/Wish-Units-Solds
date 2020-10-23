# 1. INTRODUCTION
## 1.1. Project goal

#libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(purrr)) install.packages("purrr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(forcats)) install.packages("forcats", repos = "http://cran.us.r-project.org")
if(!require(rattle)) install.packages("rattle", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("xgboost", repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")

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

#loading databases
url_main <- "https://raw.githubusercontent.com/beatrizeg/Wish-Units-Solds/main/summer-products-with-rating-and-performance_2020-08.csv"
dest_file <- "data/main.csv"
download.file(url_main, destfile = dest_file)
main <- read_csv("data/main.csv")

url_cat <- "https://raw.githubusercontent.com/beatrizeg/Wish-Units-Solds/main/unique-categories.sorted-by-count.csv"
dest_file_cat <- "data/cat.csv"
download.file(url_cat, destfile = dest_file_cat)
cat <- read_csv("data/cat.csv")

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

#2.1.1. Checking features variability and adjusting

#product_color
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

#product_variation_size_id
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

#origin_country
table(main$origin_country) %>% sort(decreasing = TRUE)

main <- main %>% mutate(
  origin_country=as.factor(case_when(
    origin_country == "CN" | origin_country == "US" ~ origin_country,
    TRUE ~ "other"
  )))

table(main$origin_country) %>% sort(decreasing = TRUE)
main %>% ggplot(aes(origin_country))+geom_bar()


#check variability for shipping_option_name and make adjustments
#this is ommited in the report as this feature's variability is 0
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
main %>% ggplot(aes(factor(units_sold)))+geom_bar()

#delete duplicated rows via product_id
n_distinct(main$product_id)
#example of product_id duplicated
main %>% filter(product_id=="5577faf03cef83230c39d0c3")
main <- distinct(main, product_id, .keep_all = TRUE) #delete duplicated rows

# 2.1.2. Assigning classes to features and calculating % stars rating instead of total count
#change rating star counts per percentage over totals
main <- main %>% mutate(rating_five_count=rating_five_count/rating_count,
                        rating_four_count=rating_four_count/rating_count,
                        rating_three_count=rating_three_count/rating_count,
                        rating_two_count=rating_two_count/rating_count,
                        rating_one_count=rating_one_count/rating_count)

main <- main %>% mutate(rating_five_count=ifelse(is.na(rating_five_count),0,rating_five_count),
                        rating_four_count=ifelse(is.na(rating_four_count),0,rating_four_count),
                        rating_three_count=ifelse(is.na(rating_three_count),0,rating_three_count),
                        rating_two_count=ifelse(is.na(rating_two_count),0,rating_two_count),
                        rating_one_count=ifelse(is.na(rating_one_count),0,rating_one_count))

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

#assign a weight to each keyword depending on the counts
cat <- cat %>% mutate(cat_n =
                        case_when(count>=1000 ~ 4,
                                  count<1000 & count>=500 ~ 3,
                                  count<500 & count>=200 ~ 2,
                                  count < 200 ~ 1,
                                  TRUE ~ 0))
main_tags <- str_split(main$tags, ",", simplify = TRUE) #split tags into different columns

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
                 merchant_has_profile_picture, product_id, n_tags) #select only features we want to keep

### 2.1.4. Predictors that do not vary across sample
#check for predictors that do not vary across sample
no_var <- nearZeroVar(main_m, saveMetrics = TRUE)
no_var[no_var[,"zeroVar"] + no_var[,"nzv"] > 0, ] 
#as percentUnique is not high in any predictor except for inventory_total,
#no deletions as not having inventory might have a big impact across sales.

### 2.1.5. Adding *perc_price* column
#we add column with difference % between price and retail_price
main_m <- main_m %>% mutate(perc_price=(price-retail_price)/retail_price)

## 2.2. Studying correlation between variables
#correlation matrix for numeric variables
main_m.cor <- main_m %>% mutate(units_sold=as.numeric(units_sold)) %>%
  dplyr::select_if(is.numeric) %>%
  cor(.)

corrplot(main_m.cor)

main_m.chisq <- main_m %>%
  dplyr::select_if(function(col) is.character(col) | 
              is.factor(col) | is.logical(col) |
              all(col == .$units_sold)) %>% dplyr::select(-product_id)

columns <- 1:ncol(main_m.chisq)
vars <- names(main_m.chisq)[columns]
out <-  apply( combn(columns,2),2,function(x){
  chisq.test(table(main_m.chisq[,x[1]],main_m.chisq[,x[2]]),correct=F)$p.value
})

out <- cbind(as.data.frame(t(combn(vars,2))),out)
out_dep <- out %>% filter(V1=="units_sold") %>% filter(out<0.05) %>% arrange(out) #these we can assume as dependent
out_ind <- out %>% filter(V1=="units_sold") %>% filter(out>=0.05) %>% arrange(out) #independent var

## 2.3. Checking predictors effect - graphs
#units_sold vs product_color
main_m %>% 
ggplot(aes(fct_infreq(product_color), units_sold)) + geom_bar(stat = "identity") +
  ggtitle("Product Color") + xlab("product_color")

#units_sold vs product_size_id
main_m %>% 
  ggplot(aes(fct_infreq(product_variation_size_id), units_sold)) + geom_bar(stat = "identity") +
   ggtitle("Product Size") + xlab("product_size_id")

#units_sold vs price
main_m %>% 
  ggplot(aes(price, units_sold)) + geom_smooth() +
  ggtitle("Price") + xlab("price")

#units_sold vs perc_price
main_m %>% 
  ggplot(aes(perc_price, units_sold)) + geom_smooth() +
  ggtitle("Percentage price") + xlab("perc_price")

#units_sold vs uses_ad_boost
main_m %>% 
  ggplot(aes(uses_ad_boosts, as.numeric(units_sold))) + geom_bar(stat="identity") +
  ggtitle("Uses Ad boosts") + ylab("units_sold")

#units_sold vs % five star
main_m %>% 
  ggplot(aes(rating_five_count, units_sold)) + geom_smooth() +
  ggtitle("Percentage of 5 stars") + xlab("perc 5*")

#units_sold vs % one star
main_m %>% 
  ggplot(aes(rating_one_count, units_sold)) + geom_smooth() +
  ggtitle("Percentage of 1 star") + xlab("perc 1*")


levels <- c("10", "50", "100", "1000", "5000", "10000", "20000", "50000", "1e+05")
main_p <- main_m %>% mutate(units_sold = factor(units_sold, levels=levels)) 



#MACHINE LEARNING

## 2.4. Creation of train and test set
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(main_m$units_sold, times=1, p=0.15, list=FALSE) #Test set is 15% of our data
train_set <- main_p[-test_index,] %>% dplyr::select(-product_id)
test_set <- main_p[test_index,] %>% dplyr::select(-product_id) 

#3. RESULTS

## 3.1. GAM Loess
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "repeatedcv", number = 3, repeats = 4, savePredictions = "all")
grid_loess <- expand.grid(span=seq(0.2,0.9,0.2), degree=1)
train_loess <- train(units_sold ~ ., data=train_set, method="gamLoess", trControl=control, tuneGrid=grid_loess, na.action=na.exclude)
ggplot(train_loess, highlight = TRUE)

y_loess <- predict(train_loess, test_set, type="raw")
acc_loess <- confusionMatrix(y_loess, test_set$units_sold)$overall[['Accuracy']]
acc_results <- tibble(method = "Gam Loess", Accuracy_Train = max(train_loess$results$Accuracy), Accuracy_Test = acc_loess)


## 3.2. K nearest neighbors
set.seed(2007, sample.kind = "Rounding")
control <- trainControl(method = "repeatedcv", number=3, repeats=4)
train_knn <- train(units_sold ~ ., data=train_set, method="knn", tuneGrid = data.frame(k=seq(3, 40, 2)), trControl=control)
ggplot(train_knn, highlight = TRUE)

y_knn <- predict(train_knn, test_set, type="raw")
acc_knn <- confusionMatrix(y_knn, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="KNN", Accuracy_Train = max(train_knn$results$Accuracy),
                                    Accuracy_Test = acc_knn))


## 3.3. Neural networks
set.seed(2007, sample.kind = "Rounding")
control <- trainControl(method = "repeatedcv", number=3, repeats=4)
grid_nnet1 <- expand.grid(size=seq(4,20,4), decay=seq(0.05, 0.5, 0.02)) #optimize size and decay
train_nnet1 <- train(units_sold ~ ., data=train_set, method="nnet", trControl=control, tuneGrid=grid_nnet1)
ggplot(train_nnet1, highlight = TRUE)

set.seed(2007, sample.kind = "Rounding")
control <- trainControl(method = "repeatedcv", number=3, repeats=4)
grid_nnet2 <- expand.grid(size=seq(4,8,2), decay=seq(0.4, 0.6, 0.02)) #try with more values
train_nnet2 <- train(units_sold ~ ., data=train_set, method="nnet", trControl=control, tuneGrid=grid_nnet2)
ggplot(train_nnet2, highlight = TRUE)

#Model chosen is train_nnet2 as it gets better accuracy (size=6 and decay=0.6)
y_nnet <- predict(train_nnet2, test_set, type="raw")
acc_nnet <- confusionMatrix(y_nnet, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Neural Network", Accuracy_Train = max(train_nnet2$results$Accuracy),
                                    Accuracy_Test = acc_nnet))


## 3.4. Classification Trees
#to avoid errors we change names of levels according to make.names()
levels(train_set$units_sold) <- c("X10", "X50", "X100", "X1000", "X5000", "X10000", "X20000", "X50000", "X05") 
levels(test_set$units_sold) <- c("X10", "X50", "X100", "X1000", "X5000", "X10000", "X20000", "X50000", "X05")

#default
set.seed(2007, sample.kind = "Rounding")
control <- trainControl(method = "cv", number=4, classProbs = TRUE)
train_rpart0 <- train(units_sold ~ ., data=train_set, method="rpart", trControl=control)
ggplot(train_rpart0, highlight = TRUE)
fancyRpartPlot(train_rpart0$finalModel, sub = NULL)

train_rpart0$finalModel$variable.importance
y_rpart0 <- predict(train_rpart0, test_set, type="raw")
acc_rpart0 <- confusionMatrix(y_rpart0, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Classification Trees not optimised", Accuracy_Train = max(train_rpart0$results$Accuracy),
                                    Accuracy_Test = acc_rpart0))

#optimizing cp
set.seed(2007, sample.kind = "Rounding")
control1 <- trainControl(method = "cv", number=4, classProbs = TRUE)
train_rpart1 <- train(units_sold ~ ., data=train_set, method="rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), control=rpart::rpart.control(minsplit=15), trControl=control1)
ggplot(train_rpart1, highlight = TRUE)
cp <- train_rpart1$bestTune$cp
minsplit <- seq(10, 40, len=8)
acc <- sapply(minsplit, function(ms){
  train(units_sold ~ ., method = "rpart", data = train_set, tuneGrid = data.frame(cp=cp),
        control=rpart::rpart.control(minsplit=ms), trControl=control1)$results$Accuracy })
qplot(minsplit, acc)
minsplit <- minsplit[which.max(acc)]
train_rpart2 <- train(units_sold ~ ., data=train_set, method="rpart", tuneGrid = data.frame(cp = cp), control=rpart::rpart.control(minsplit=minsplit), trControl=control1)
fancyRpartPlot(train_rpart2$finalModel, sub = NULL)
train_rpart2$finalModel$variable.importance

y_rpart2 <- predict(train_rpart2, test_set, type="raw")
acc_rpart2 <- confusionMatrix(y_rpart2, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Classification Trees Optimized", Accuracy_Train = max(train_rpart2$results$Accuracy),
                                    Accuracy_Test = acc_rpart2))


## 3.5. Random Forest
train_rf0 <- train(units_sold ~ ., data=train_set, method="rf")
ggplot(train_rf0, highlight = TRUE)

y_rf0 <- predict(train_rf0, test_set, type="raw")
acc_rf0 <- confusionMatrix(y_rf0, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Random Forest not optimized", Accuracy_Train = max(train_rf0$results$Accuracy),
                                    Accuracy_Test = acc_rf0))
#Optimize mtry
set.seed(1234, sample.kind = "Rounding")
control_rf <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
grid_rf <- expand.grid(mtry=seq(16,40,2))
train_rf1 <- train(units_sold ~ ., data=train_set, method="rf", tuneGrid=grid_rf, trControl=control_rf)
ggplot(train_rf1, highlight = TRUE)
mtry <- train_rf1$bestTune$mtry

#optimising minimum node size
grid_mtry <- expand.grid(mtry=mtry)
nodesize <- seq(1, 5, 2)
acc <- sapply(nodesize, function(ns){
  train(units_sold ~ ., method = "rf", data = train_set, tuneGrid = grid_mtry, trControl=control_rf,
        nodesize = ns)$results$Accuracy })
qplot(nodesize, acc)
nodesize <- nodesize[which.max(acc)]
max(acc)

train_rf2 <- train(units_sold ~ ., method = "rf", data = train_set, tuneGrid = grid_mtry, nodesize = nodesize, trControl=control_rf)

#chosen train_rf2 as it provides highter accuracy on train_set than train_rf1
y_rf <- predict(train_rf2, test_set, type="raw")
acc_rf <- confusionMatrix(y_rf, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="Random Forest optimized", Accuracy_Train = max(train_rf2$results$Accuracy),
                                    Accuracy_Test = acc_rf))


## 3.6. XGBoost
#optimize eta and max_depth 
grid_xgbm1 <- expand.grid(min_child_weight=c(nodesize), eta=seq(0.01, 0.3, 0.05), nrounds=c(500), max_depth=seq(4,10,2), gamma=0,
                         colsample_bytree=c(0.8), subsample=1)
set.seed(62, sample.kind = "Rounding")
control_xgbm <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm1 <- train(units_sold ~ ., method="xgbTree", data=train_set, trControl=control_xgbm, tuneGrid=grid_xgbm1, verbose=TRUE)
ggplot(train_xgbm1, highlight = TRUE)
eta <- train_xgbm1$bestTune$eta
max_depth <- train_xgbm1$bestTune$max_depth

#optimize nrounds
grid_xgbm2 <- expand.grid(min_child_weight=c(nodesize), eta=c(eta), nrounds=c(500,1000,1500,2000), max_depth=c(max_depth), gamma=0,
                         colsample_bytree=c(0.8), subsample=1)
set.seed(62, sample.kind = "Rounding")
control_xgbm <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm2 <- train(units_sold ~ ., method="xgbTree", data=train_set, trControl=control_xgbm, tuneGrid=grid_xgbm2, verbose=TRUE)
ggplot(train_xgbm2, highlight = TRUE)
nrounds <- train_xgbm2$bestTune$nrounds

#optimize nodesize
grid_xgbm3 <- expand.grid(min_child_weight=c(1,3,5), eta=c(eta), nrounds=c(nrounds), max_depth=c(max_depth), gamma=0,
                         colsample_bytree=c(0.8), subsample=1)
set.seed(62, sample.kind = "Rounding")
control_xgbm <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm3 <- train(units_sold ~ ., method="xgbTree", data=train_set, trControl=control_xgbm, tuneGrid=grid_xgbm3, verbose=TRUE)
ggplot(train_xgbm3, highlight = TRUE)
nodesize <- train_xgbm3$bestTune$min_child_weight

#optimize gamma
grid_xgbm4 <- expand.grid(min_child_weight=c(nodesize), eta=c(eta), nrounds=c(nrounds), max_depth=c(max_depth), gamma=seq(0,5,2),
                          colsample_bytree=c(0.8), subsample=1)
set.seed(62, sample.kind = "Rounding")
control_xgbm <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm4 <- train(units_sold ~ ., method="xgbTree", data=train_set, trControl=control_xgbm, tuneGrid=grid_xgbm4, verbose=TRUE)
ggplot(train_xgbm4, highlight = TRUE)
gamma <- train_xgbm4$bestTune$gamma

#run with optimized values
grid_xgbm_op <- expand.grid(min_child_weight=c(nodesize), eta=c(eta), nrounds=c(nrounds), max_depth=c(max_depth), gamma=gamma,
                         colsample_bytree=c(0.8), subsample=1)
set.seed(62, sample.kind = "Rounding")
control_xgbm <- trainControl(method = "cv", number=3, savePredictions = FALSE, verboseIter = FALSE)
train_xgbm_op <- train(units_sold ~ ., method="xgbTree", data=train_set, tuneGrid=grid_xgbm_op, trControl=control_xgbm, verbose=TRUE)

xgbm_imp <- varImp(train_xgbm_op)
plot(xgbm_imp, top = 10)

#test model
y_xgbm <- predict(train_xgbm_op, test_set, type="raw")
acc_xgbm <- confusionMatrix(y_xgbm, test_set$units_sold)$overall[['Accuracy']]
acc_results <- bind_rows(acc_results,
                         data_frame(method="XGBoost", Accuracy_Train = max(train_xgbm_op$results$Accuracy),
                                    Accuracy_Test = acc_xgbm))

## 3.7. H2O AutoML
if(!require(h2o)) install.packages("h2o", repos = "http://cran.us.r-project.org")
library(h2o)
h2o.init() #inizialize - error might occur if JAVA 11 is not installed in your computer

#change from data.frame to as.h2o
data_h2o <- as.h2o(train_set)
test_h2o <- as.h2o(test_set)

#run automl
automl_all <- h2o.automl(y=3, training_frame=data_h2o, max_runtime_secs=500,validation_frame = test_h2o,
                            seed=1, keep_cross_validation_predictions=TRUE)

automl_all_lb <- automl_all@leaderboard
print(automl_all_lb, n=nrow(automl_all_lb))

#plot variable importance in the best model
h2o.varimp(automl_all@leader)
#dev.off() #activate if you get error message when running code
plot <- h2o.varimp_plot(automl_all@leader, num_of_features = 10)

#test best performer model
h2o_xgb <- h2o.performance(model = automl_all@leader, newdata = test_h2o)
pred <- h2o.predict(automl_all@leader, test_h2o)
h2o.confusionMatrix(automl_all@leader, newdata = test_h2o)

acc_results <- bind_rows(acc_results,
                         data_frame(method="h2oAutoML", Accuracy_Train = 0.697,
                                    Accuracy_Test = 0.7389))

acc_results
