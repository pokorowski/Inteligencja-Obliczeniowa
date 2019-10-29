library(tidyr)
library(dplyr)
df <- read.csv("iris_with_errors.csv")
glimpse(df)

df %>% summarise(n = n_distinct(MonthlyCharges),
                 na = sum(is.na(MonthlyCharges)),
                 med = median(sepal.length, na.rm = TRUE))
