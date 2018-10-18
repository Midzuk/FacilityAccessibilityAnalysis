library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)

# grtCirDist <- function(lat1, lon1, lat2, lon2) {
#  6378137 * acos(sin(lat1 * pi / 180) * sin(lat2 * pi / 180) + cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * cos(lon1 * pi / 180 - lon2 * pi /180))
#}



# questionnaire

ques0 <- read_csv("ignore/input/data.csv")

ques <- ques0 %>%
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

ques_pop <- ques %<>%
  left_join(mesh_pop, by = "sample_id") %>%
  filter(!is.na(population)) %>% # !is.na(income) & !is.na(distance) & !is.na(population) & !is.na(one_line_distance)) %>%
  mutate(DID = if_else(population >= 4000, TRUE, FALSE))

ques_did_mean <- ques %>% 
  group_by(DID) %>%
  summarise(mean(facility_use), n())


#DID (assuming >=4000people/km2)
ques_did <- ques %>%
  filter(population >= 4000)
#notDID (assuming <4000people/km2)
ques_notdid <- ques %>%
  filter(population < 4000)

t.test(ques_did$facility_use, ques_notdid$facility_use, var.equal = F)

#DID

  
#facility_use_did <- ggplot(data = ques_did_mean, aes(x = DID, y = mean(facility_use))) +
#  geom_bar(stat = "identity") +
#  ylim(0,0.30)
  

# welch t
# t.test(ques_did$facility_use, ques_did$facility_use, var.equal = F, paired = F)

# result <- glm(facility_use ~ distance + sex, ques_notdid, family = binomial(link = "logit"))
# summary(result)

# predict(result, type = "response")




ques %<>%
  filter(!is.na(income) & !is.na(distance) & !is.na(one_line_distance))
ques_did %<>%
  filter(!is.na(income) & !is.na(distance) & !is.na(one_line_distance))
ques_notdid %<>%
  filter(!is.na(income) & !is.na(distance) & !is.na(one_line_distance))

result <- glm(facility_use ~ distance + sex + income, ques, family = binomial(link = "logit"))
summary(result)

result_did <- glm(facility_use ~ one_line_distance + sex + income, ques_did, family = binomial(link = "logit"))
summary(result_did)

result_notdid <- glm(facility_use ~ one_line_distance + sex + income, ques_notdid, family = binomial(link = "logit"))
summary(result_notdid)


ques1000 <- ques %>%
  filter(one_line_distance <= 1000)

result <- glm(facility_use ~ distance + sex + income, ques1000, family = binomial(link = "logit"))
summary(result)

grubbs







ques_col <- ques %>%
  select(one_line_distance, sex, income)
cor(ques_col, method="spearman")


ques1 <- ques %>%
   #mutate(dist_ratio = - log((distance - one_line_distance) / one_line_distance))
  mutate(dist_ratio = (distance - one_line_distance) / one_line_distance)
  # filter(one_line_distance >= 500)

# result2 <- lm(log(dist_ratio) ~ 1 * log(one_line_distance) + log(population), ques1)
# summary(result2)

#g0 <- ggplot(data = ques1, aes(x = ques1$one_line_distance, y = ques1$distance, colour = population)) +
#  labs(x = "直線距離 (m)", y = "移動距離 (m)") +
#  xlim(0, 5000) +
#  ylim(0, 5000) +
#  geom_point(size = 1.5) +
#  scale_color_gradientn(colours=c("forestgreen", "yellow", "red", "red1", "red2"))

ques2_1 <- ques %>%
  mutate(dist_id = round(one_line_distance/100)) %>%
  group_by(dist_id) %>%
  summarise(mean(facility_use), n(), sum(facility_use)) %>%
  rename(use = "sum(facility_use)",
         num = "n()") %>%
  mutate(dont_use = num - use) %>%
  gather(key = "use", value = "num1",
         "use", "dont_use")



g_ques2 <- ggplot(data = ques2_1, aes(x = ques2_1$dist_id, y = ques2_1$num1, fill = use)) +
  xlim(0,20) +
  ylim(0, 60) +
  geom_bar(stat = "identity") +
  # geom_bar(stat = "identity", position = "fill") +
  labs(x = "直線距離 (100m)", y = "回答者数 (人)")


ques2_income <- ques %>%
  mutate(income_id = round(income/1000000)) %>%
  group_by(income_id) %>%
  summarise(mean(facility_use), n(), sum(facility_use)) %>%
  rename(use = "sum(facility_use)",
         num = "n()") %>%
  mutate(dont_use = num - use) %>%
  gather(key = "use", value = "num1",
         "use", "dont_use")

g_ques_in <- ggplot(data = ques2_income, aes(x = ques2_income$income_id, y = ques2_income$num1, fill = use)) +
  xlim(0,20) +
  # ylim(0, 60) +
  geom_bar(stat = "identity", position = "fill") +
  # geom_bar(stat = "identity", position = "fill") +
  labs(x = "所得 (百万円)", y = "学校開放利用率")






gg <- ggplot(data = ques1, aes(x = ques1$one_line_distance, y = ques1$dist_ratio, colour = population)) +
    labs(x = "直線距離 (m)", y = "直線距離に対する移動距離と直線距離の差の比") +
  xlim(0, 5000) +
  ylim(0, 10) +
  geom_point(size = 1.5) +
  scale_color_gradientn(colours=c("forestgreen", "yellow", "red", "red1", "red2"))
  # geom_abline(intercept = 1.619e+00, slope = 9.250e-04)
  # scale_color_gradient2(midpoint=mean(ques1$population), low="blue", mid="violet",
  #                          high="red", space ="Lab" )
  # scale_colour_gradientn(low="blue", high="red")
  # geom_hline(yintercept = 0)

ques_sex <- ques %>%
  group_by(sex) %>%
  summarise(mean(facility_use), n(), sum(facility_use)) %>%
  rename(use = "sum(facility_use)",
         num = "n()") %>%
  mutate(dont_use = num - use) %>%
  gather(key = "use", value = "num1",
         "use", "dont_use")

g_ques_sex <- ggplot(data = ques_sex, aes(x = ques_sex$sex, y = ques_sex$num1, fill = use)) +
  # xlim(0,20) +
  # ylim(0, 60) +
  geom_bar(stat = "identity", position = "fill") +
  # geom_bar(stat = "identity", position = "fill") +
  labs(x = "性別", y = "回答者数 (人)")






ques2 <- ques1 %>%
  mutate(dist_ratio2 = 1 / dist_ratio)

g1 <- ggplot(data = ques2, aes(x = ques2$one_line_distance, y = ques2$dist_ratio2, colour = population)) +
  labs(x = "直線距離", y = "直線距離に対する移動距離と直線距離の差の比の逆数") +
  xlim(0, 5000) +
  ylim(0, 10) +
  geom_point(size = 1.5) +
  scale_color_gradientn(colours=c("forestgreen", "yellow", "red", "red1", "red2"))
  # geom_abline(intercept = 1.619e+00, slope = 9.250e-04)

ques2 %<>%
  filter(distance > 0, one_line_distance > 0, population > 0)

result3 <- lm(distance ~ one_line_distance + population, ques2)
summary(result3)







g_welch <- ggplot(data = ques2, aes(x = ques2$one_line_distance, y = ques2$dist_ratio2, colour = ques2$population)) 