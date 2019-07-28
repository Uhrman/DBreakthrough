require(tidyverse)
require(data.table)
require(caret)
require(stringr)

## загрузка данных
url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/00296/dataset_diabetes.zip"
td <- tempdir()
print(td)
tf <- tempfile(tmpdir=td, fileext=".zip")
download.file(url, tf)

## импорт параметров набора данных
tI <- read_csv(unz(tf, "dataset_diabetes/IDs_mapping.csv"))
admissions <- tI[1:8, ]
discharge_dispositions <- tI[11:40, ]
colnames(discharge_dispositions) <- tI[10, ]
admission_sources <- tI[43:67, ]
colnames(admission_sources) <- tI[42, ]
tI <- NULL

diseases <- data.table(dcode = c(seq(390,459), 785), dgroup = "circulatory")
diseases <- rbind(diseases, data.table(dcode = c(seq(460,519), 786), dgroup = "respiratory"))
diseases <- rbind(diseases, data.table(dcode = c(seq(390,459), 785), dgroup = "circulatory"))
diseases <- rbind(diseases, data.table(dcode = c(seq(460,519), 786), dgroup = "respiratory"))
diseases <- rbind(diseases, data.table(dcode = c(seq(520,579), 787), dgroup = "digestive"))
diseases <- rbind(diseases, data.table(dcode = 250, dgroup = "diabetes"))
diseases <- rbind(diseases, data.table(dcode = seq(800,999), dgroup = "injury"))
diseases <- rbind(diseases, data.table(dcode = seq(710,739), dgroup = "musculoskeletal"))
diseases <- rbind(diseases, data.table(dcode = sort(c(seq(580,629), 788, 789, seq(140,239), 780, 781, 784, seq(790,799), seq(240,249), seq(251,279), 
                            seq(680,709), 782, seq(001,139), seq(290,319), seq(280,289), seq(320,359), seq(630,679), 
                            seq(360,389), seq(740,759), seq(1000,1091), seq(2000,2030), seq(2800,2999))), dgroup = "other")
                  )

## импорт параметров набора данных
diabets_dataset <- read_csv(unz(tf, "dataset_diabetes/diabetic_data.csv"))

## подготовка набора данных
case_age <- function (age) {
  case_when (
    age == "[0-10)"   ~ 0,
    age == "[10-20)"  ~ 10,
    age == "[20-30)"  ~ 20,
    age == "[30-40)"  ~ 30,
    age == "[40-50)"  ~ 40,
    age == "[50-60)"  ~ 50,
    age == "[60-70)"  ~ 60,
    age == "[70-80)"  ~ 70,
    age == "[80-90)"  ~ 80,
    age == "[90-100)" ~ 90,
    age == "?"          ~ NA_real_,
    TRUE              ~ NA_real_
  )
}
diabets_dataset <- diabets_dataset %>% mutate(age = case_age(age))

case_weight <- function (weight) {
  case_when (
    weight == "[0-25)"     ~ 0,
    weight == "[25-50)"    ~ 25,
    weight == "[50-75)"    ~ 50,
    weight == "[75-100)"   ~ 75,
    weight == "[100-125)"  ~ 100,
    weight == "[125-150)"  ~ 125,
    weight == "[150-175)"  ~ 150,
    weight == "[175-200)"  ~ 175,
    weight == "[>200)"     ~ 200,
    weight == "?"          ~ NA_real_,
    TRUE                   ~ NA_real_
  )
}
diabets_dataset <- diabets_dataset %>% mutate(weight = case_weight(weight))

diag_round <- function(diag) {
  case_when (
    diag == "?"           ~ NA_real_,
    str_detect(diag, "V") ~ as.numeric(substr(diag, 2, 3)) + 1000,
    str_detect(diag, "E") ~ as.numeric(substr(diag, 2, 4)) + 2000,
    is.na(diag)           ~ NA_real_, 
    TRUE                  ~ as.numeric(diag)
  )
}
diabets_dataset <- diabets_dataset %>% mutate(diag_1r = round(diag_round(diag_1),0), diag_2r = round(diag_round(diag_2),0), diag_3r = round(diag_round(diag_3),0))
m <- diseases %>% transmute(diag_1r = dcode, dgroup1 = dgroup)
diabets_dataset <- merge(diabets_dataset, m, by.x = "diag_1r", by.y = "diag_1r", all.x = TRUE, sort = FALSE)
m <- diseases %>% transmute(diag_2r = dcode, dgroup2 = dgroup)
diabets_dataset <- merge(diabets_dataset, m, by.x = "diag_2r", by.y = "diag_2r", all.x = TRUE, sort = FALSE)
m <- diseases %>% transmute(diag_3r = dcode, dgroup3 = dgroup)
diabets_dataset <- merge(diabets_dataset, m, by.x = "diag_3r", by.y = "diag_3r", all.x = TRUE, sort = FALSE)
m <- NULL

case_A1C <- function (A1Cresult) {
  case_when (
    A1Cresult == "None" ~ -100,
    A1Cresult == "Norm" ~ 0,
    A1Cresult == ">7"   ~ 1,
    A1Cresult == ">8"   ~ 2,
    A1Cresult == "?"    ~ NA_real_,
    TRUE                ~ NA_real_
  )
}
diabets_dataset <- diabets_dataset %>% mutate(A1Cresult = case_A1C(A1Cresult))

case_drugs <- function (drug) {
  case_when (
    drug == "No"      ~ -100,
    drug == "Down"    ~ -1,
    drug == "Steady"  ~ 0,
    drug == "Up"      ~ 1,
    drug == "?"       ~ NA_real_,
    TRUE              ~ NA_real_
  )
}
diabets_dataset <- diabets_dataset %>% mutate(
  metformin = case_drugs(metformin),
  repaglinide = case_drugs(repaglinide),
  nateglinide = case_drugs(nateglinide),
  chlorpropamide = case_drugs(chlorpropamide),
  glimepiride = case_drugs(  glimepiride),
  acetohexamide = case_drugs(acetohexamide),
  glipizide = case_drugs(glipizide),
  glyburide = case_drugs(glyburide),
  tolbutamide = case_drugs(tolbutamide),
  pioglitazone = case_drugs(pioglitazone),
  rosiglitazone = case_drugs(rosiglitazone),
  acarbose = case_drugs(rosiglitazone),
  miglitol = case_drugs(miglitol),
  troglitazone = case_drugs(troglitazone),
  tolazamide = case_drugs(tolazamide),
  examide = case_drugs(tolazamide),
  citoglipton = case_drugs(tolazamide),
  insulin = case_drugs(tolazamide),
  "glyburide-metformin" = case_drugs("glyburide-metformin"),
  "glipizide-metformin" = case_drugs("glipizide-metformin"),
  "glimepiride-pioglitazone" = case_drugs("glimepiride-pioglitazone"),
  "metformin-rosiglitazone" = case_drugs("metformin-rosiglitazone"),
  "metformin-pioglitazone" = case_drugs("metformin-pioglitazone")
)

diabets_dataset <- diabets_dataset %>% 
  mutate(readmitted = ifelse(readmitted == "NO", 0, 1),
         change = ifelse(change == "No", 0, 1),
         diabetesMed = ifelse(diabetesMed == "No", 0, 1)
  )

## создание выборки из генеральной совокупности - слишком слабый компьютер
set.seed(2019)                    # только для целей тестирования
p <- 0.025
sample_index <- createDataPartition(diabets_dataset$readmitted, times = 1, p = p, list = FALSE)
sample_set <- diabets_dataset[sample_index, ] # только для целей тестирования на слабом компьютере

## CART
sample_set %>% select(age, dgroup1, time_in_hospital) %>% filter(!is.na(dgroup1)) %>% 
  ggplot(aes(dgroup1, time_in_hospital, color = age)) + 
  geom_point()

m <- sample_set %>% select(age, A1Cresult, readmitted) %>% filter(!is.na(A1Cresult), A1Cresult != -100) 
m %>% 
  ggplot(aes(A1Cresult, readmitted, color = age)) + 
  geom_point()

sum(is.na(m))

train_lm <- train(readmitted ~ A1Cresult, method = "lm", data = m)

train_glm <- train(readmitted ~ ., method = "glm", data = m)


library(rpart)

train_rpart <- train(readmitted ~ ., method = "rpart", data = m)
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)



## выделение сетов для тренировки и тестирования
set.seed(2019)                    # только для целей тестирования
p <- 0.25
test_index <- createDataPartition(ffull_set$readmitted, times = 1, p = p, list = FALSE)

## формирование сетов данных
#sel.n <- c("readmitted", "change", "diabetesMed",
#          "gender", "age", "weight", "diag_1r", 
#          "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_emergency", "number_diagnoses", 
#          "A1Cresult","metformin","repaglinide", "nateglinide", "chlorpropamide", 
#          "glimepiride", "acetohexamide", "glipizide","glyburide","tolbutamide", 
#          "pioglitazone", "rosiglitazone", "acarbose", "miglitol", "troglitazone", 
#          "tolazamide", "examide", "citoglipton", "insulin", "glyburide-metformin", 
#          "glipizide-metformin", "glimepiride-pioglitazone", "metformin-rosiglitazone", "metformin-pioglitazone"
#)

sel_n <- c("readmitted", "change", "diabetesMed",
          "gender", "age", "diag_1r", 
          "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_diagnoses", 
          "A1Cresult"
)

full_set <- ffull_set %>% select(sel_n)
full_set <- na.omit(full_set)

train_set <- full_set[-test_index, ]
test_set <- full_set[test_index, ]

## 
train.formula <- function (form, data, ..., weights, subset, na.action = na.fail, contrasts = NULL)  {
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval.parent(m$data)))  m$data <- as.data.frame(data)
  m$... <- m$contrasts <- NULL
  
  ## Look for missing `na.action` in call. To make the default (`na.fail`) 
  ## recognizable by `eval.parent(m)`, we need to add it to the call
  ## object `m`
  
  if(!("na.action" %in% names(m))) m$na.action <- quote(na.fail)
  
  m[[1]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  if(nrow(m) < 1) stop("Every row has at least one missing value were found")
  Terms <- attr(m, "terms")
  x <- model.matrix(Terms, m, contrasts)
  cons <- attr(x, "contrast")
  int_flag <- grepl("(Intercept)", colnames(x))
  if (any(int_flag)) x <- x[, !int_flag, drop = FALSE]
  w <- as.vector(model.weights(m))
  y <- model.response(m)  
}

train_glm <- train(train_set$readmitted ~ ., method = "glm", data = train_set)
train_knn <- train(train_set$readmitted ~ ., method = "knn", data = train_set)
