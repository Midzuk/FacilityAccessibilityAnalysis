sample <- read_csv("ignore/input/sample.csv")

result <- glm(facility_use_sample ~ distance_sample, data = sample, family = binomial("logit"))
summary(result)
