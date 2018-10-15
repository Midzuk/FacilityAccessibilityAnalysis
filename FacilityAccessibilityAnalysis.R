library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)

# grtCirDist <- function(lat1, lon1, lat2, lon2) {
#  6378137 * acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos(lon1 * pi / 180 - lon2 * pi /180))
#}



# questionnaire

ques0 <- read_csv("ignore/input/data.csv")

ques <- data0 %>%
  rename(sample_id = "SAMPLEID",
         facility_use = "Q2",
         income = "Q23",
         sex = "SEX",
         age = "AGE",
         child = "CHILD",
         # one_line_distance = "DISTANCE1",
         lat1 = "LAT1",
         lon1 = "LON1",
         lat2 = "LAT2",
         lon2 = "LON2") %>%
  select(sample_id,
         facility_use,
         income,
         sex,
         age,
         child,
         lat1,
         lon1,
         lat2,
         lon2) %>%
  mutate(facility_use = case_when(facility_use == 2 ~ 0,
                                  facility_use == 1 ~ 1),
         income = case_when(income >= 1 & income <= 10 ~ income * 1000000 - 500000,
                            income == 11 ~ 12500000,
                            income == 12 ~ 17500000),
         sex = sex - 1,
         child = child - 1,
         one_line_distance = 6378137 * acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos(lon1 * pi / 180 - lon2 * pi /180)))



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


ques %<>%
  left_join(dist, by = "sample_id")

ques %<>%
  left_join(mesh_pop, by = "sample_id") %>%
  filter(!is.na(income) & !is.na(distance) & !is.na(population) & !is.na(one_line_distance))



#DID (assuming >=4000people/km2)

ques_did <- data %>%
  filter(population >= 4000)



#notDID (assuming <4000people/km2)

ques_notdid <- data %>%
  filter(population < 4000)



result <- glm(facility_use ~ distance + sex, ques_notdid, family = binomial(link = "logit"))
summary(result)

# predict(result, type = "response")

ques1 <- ques %>%
  mutate(dist_ratio = 1 / ((distance - one_line_distance) / one_line_distance))
  # filter(one_line_distance >= 500)

g <- ggplot(data = ques1, aes(x = ques1$one_line_distance, y = ques1$dist_ratio, colour = ques1$population)) +
  labs(x = "one_line_distance", y = "ratio") +
  geom_point(size = 1) +
  scale_color_gradientn(colours=c("forestgreen", "yellow", "red", "red1", "red2"))
  # scale_color_gradient2(midpoint=mean(ques1$population), low="blue", mid="violet",
  #                          high="red", space ="Lab" )
  # scale_colour_gradientn(low="blue", high="red")
  # geom_hline(yintercept = 0)

