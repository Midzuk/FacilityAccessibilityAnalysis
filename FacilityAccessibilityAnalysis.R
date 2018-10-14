library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)

# questionnaire

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



# distance

dist0 <- read_csv("ignore/input/rawdata_distance.csv")

dist <- dist0 %>%
  mutate(distance = dist_link + dist_org * 1 + dist_dest * 1) %>%
  select(sample_id, distance)



# 1km mesh population

mesh0 <- read_csv("ignore/input/mesh.csv")

mesh <- mesh0 %>%
  select(sample_id, third_order_mesh_code) %>%
  rename(mesh_code = "third_order_mesh_code")

pop <- read_csv("ignore/input/pop2015_1km.csv",
                col_types = cols(population0_14 = col_integer())) %>%
  select(mesh_code, population)

mesh_pop <- mesh %>%
  left_join(pop, by = "mesh_code")


data %<>%
  left_join(dist, by = "sample_id")

data %<>%
  left_join(mesh_pop, by = "sample_id") %>%
  filter(!is.na(income) & !is.na(distance) & !is.na(population))



#DID (assuming >=4000people/km2)

data_did <- data %>%
  filter(population >= 4000)



#notDID (assuming <4000people/km2)

data_notdid <- data %>%
  filter(population < 4000)



result <- glm(facility_use ~ distance + sex, data_notdid, family = binomial(link = "logit"))
summary(result)

# predict(result, type = "response")

data1 <- data %>%
  mutate(dist_ratio = distance / one_line_distance)
  # filter(one_line_distance >= 500)
  

g <- ggplot()
g <- g +
  geom_point(data = data1, aes(x = data1$one_line_distance, y = data1$dist_ratio)) +
  geom_hline(yintercept = 1)

