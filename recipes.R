#### Рецепты

library(tidyverse)
library(data.table)
library(caret)
library(stringr)

### загрузка данных
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00462/drugsCom_raw.zip"
td <- tempdir()
print(td)
tf <- tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)

### импорт параметров набора данных
big_test_set <- read_tsv(unz(tf, "drugsComTest_raw.tsv"))
big_train_set <- read_tsv(unz(tf, "drugsComTrain_raw.tsv"))

### снижение объема расчетов в связи с недостаточной мощностью ср-в ВТ
set.seed(2019)
big_test_reduce_index <- createDataPartition(y = big_test_set$rating, times = 1, p = 0.1, list = FALSE)
big_train_reduce_index <- createDataPartition(y = big_train_set$rating, times = 1, p = 0.1, list = FALSE)
test_set <- big_test_set[big_test_reduce_index, ]
train_set <- big_train_set[big_train_reduce_index, ]
#big_test_set <- NULL
#big_train_set <- NULL

## данные поля review не участвуют анализе, фильтрация NA
test_set <- select(test_set, -review) %>% filter(!is.na(condition))
train_set <- select(train_set, -review) %>% filter(!is.na(condition))

## превращение базы обзоров лекрств в базу обзоров кулинарных рецептов
tbl_columns <- c("id", "recipeName", "condition", "rating", "date", "usefulCount")
colnames(test_set) <- tbl_columns
colnames(train_set) <- tbl_columns

## исключение данных, не вошедших в трейнинговый набор
test_set <- test_set %>% 
  semi_join(train_set, by = "recipeName") %>%
  semi_join(train_set, by = "condition")

## переход к работе с идентификатором кулинарного рецепта
train_recipes <- train_set %>% select(recipeName) %>% group_by(recipeName) %>% summarize() %>% as.data.table()
train_recipes[ , recipeId := as.factor(rownames(train_recipes))]
train_set <- merge(train_set, train_recipes, by.x = "recipeName", by.y = "recipeName", all.x = TRUE, sort = FALSE)
test_set <- merge(test_set, train_recipes, by.x = "recipeName", by.y = "recipeName", all.x = TRUE, sort = FALSE)
test_set <- select(test_set, -recipeName)
train_set <- select(train_set, -recipeName)

## переход к работе с идентификатором условия применения рецепта
train_conds <- train_set %>% select(condition) %>% group_by(condition) %>% summarize() %>% as.data.table()
train_conds[ , conditnId := as.factor(rownames(train_conds))]
train_set <- merge(train_set, train_conds, by.x = "condition", by.y = "condition", all.x = TRUE, sort = FALSE)
test_set <- merge(test_set, train_conds, by.x = "condition", by.y = "condition", all.x = TRUE, sort = FALSE)
test_set <- select(test_set, -condition)
train_set <- select(train_set, -condition)

train_set %>% 
  count(recipeId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "blue", fill = "blue") + 
  scale_x_log10() + 
  ggtitle("Кулинарные рецепты")

train_set %>%
  count(conditnId) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "blue", fill = "blue") + 
  scale_x_log10() + 
  ggtitle("Предпочтения пользователей")

### Постановка задачи
## Критерий оценки:
##   - снижение остаточной среднеквадратичной ошибки (RMSE) 

### функция рассчета RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

### Модель "Простое угадывание"
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

rmse_results <- tibble(method = "Только среднее", RMSE = naive_rmse)

fit <- lm(rating ~ as.factor(recipeId), data = train_set)

mu <- mean(train_set$rating) 
recipe_avgs <- train_set %>% 
  group_by(recipeId) %>% 
  summarize(b_r = mean(rating - mu))  %>% as.data.table()

qplot(b_r, data = recipe_avgs, bins = 10, color = I("blue"), fill = I("blue"))

predicted_ratings <- mu + test_set %>% 
  left_join(recipe_avgs, by='recipeId') %>%
  pull(b_r)
model_1_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                tibble(method="Учет популярности рецептов",  
                       RMSE = model_1_rmse))
rmse_results 

train_set %>% 
  group_by(conditnId) %>% 
  summarize(b_c = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_c)) + 
  geom_histogram(bins = 30, color = "blue", fill = "blue")

conditn_avgs <- train_set %>% 
  left_join(conditn_avgs, by="recipeId") %>%
  group_by(condithId) %>%
  summarize(b_c = mean(rating - mu - b_r))

predicted_ratings <- test_set %>% 
  left_join(conditn_avgs, by="recipeId") %>%
  left_join(conditn_avgs, by="conditnId") %>%
  mutate(pred = mu + b_r + b_c) %>%
  pull(pred)
model_2_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Учет рецепта и условий применения",  
                                 RMSE = model_2_rmse))
rmse_results

##############################################################
## Regularization
## Penalized Least Squares

lambda <- 3
mu <- mean(train_set$rating)
recipe_reg_avgs <- train_set %>% 
  group_by(recipeId) %>% 
  summarize(b_r = sum(rating - mu)/(n()+lambda), n_r = n()) 

tibble(original = recipe_avgs$b_r, 
       regularlized = recipe_reg_avgs$b_r, 
       n = recipe_reg_avgs$n_r) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)

predicted_ratings <- test_set %>% 
  left_join(recipe_reg_avgs, by = "recipeId") %>%
  mutate(pred = mu + b_r) %>%
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, test_set$rating)
rmse_results <- bind_rows(rmse_results,
                          tibble(method="Регулярная модель (рецепты)",  
                          RMSE = model_3_rmse))
rmse_results 

############################################################
lambdas <- seq(0, 10, 0.25)
mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(recipeId) %>% 
  summarize(s = sum(rating - mu), n_r = n())
rmses <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='recipeId') %>% 
    mutate(b_r = s/(n_r+l)) %>%
    mutate(pred = mu + b_r) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
qplot(lambdas, rmses)  
lambdas[which.min(rmses)]

lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$rating)
  
  b_r <- train_set %>% 
    group_by(recipeId) %>%
    summarize(b_r = sum(rating - mu)/(n()+l))
  
  b_c <- train_set %>% 
    left_join(b_r, by="recipeId") %>%
    group_by(conditnId) %>%
    summarize(b_c = sum(rating - b_r - mu)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_r, by = "recipeId") %>%
    left_join(b_c, by = "conditnId") %>%
    mutate(pred = mu + b_r + b_c) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
#qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(
  rmse_results,
  tibble(method="регулярная модель (рецепты и применение)",  
         RMSE = min(rmses)))

rmse_results %>% knitr::kable()





