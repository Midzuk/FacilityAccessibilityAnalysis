library(tidyverse)
library(dplyr)
library(magrittr)

# ’²¸Œ‹‰Ê

data0 <- read_csv("ignore/input/data.csv")

data <- data0 %>%
  rename(sample_id = "SAMPLEID",
         facility_use = "Q2",
         income = "Q23",
         sex = "SEX",
         age = "AGE",
         child = "CHILD",
         one_line_distance = "DISTANCE1") %>%
  select(sample_id,
         facility_use,
         income,
         sex,
         age,
         child,
         one_line_distance) %>%
  mutate(facility_use = case_when(facility_use == 2 ~ 0,
                                  facility_use == 1 ~ 1),
         income = case_when(income >= 1 & income <= 10 ~ income * 1000000 - 500000,
                            income == 11 ~ 12500000,
                            income == 12 ~ 17500000),
         sex = sex - 1,
         child = child - 1,
         one_line_distance = one_line_distance * 1000)



# ‹——£î•ñ

dist_data0 <- read_csv("ignore/input/rawdata_distance.csv")

dist_data <- dist_data0 %>%
  mutate(distance = dist_link + dist_org *0 + dist_dest *0)



# 1kmƒƒbƒVƒ…lŒû





data %<>%
  left_join(dist_data, by = "sample_id") %>%
  filter(!is.na(income) & !is.na(distance))

result <- glm(facility_use ~ distance + income + sex, data, family = binomial(link = "logit"))
summary(result)
predict(result, type = "response")

