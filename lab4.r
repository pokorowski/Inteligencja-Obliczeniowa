library(tidyr)
library(dplyr)

df <- read.csv("iris_with_errors.csv")
flower <- c("Setosa", "Versicolor","Virginica")

check <- function(x) {
  x <= 0 | x >=15 | is.na(x)
}


#1
df <- df %>%
mutate(sepal.length = replace(sepal.length, sepal.length ==  "-", NA)) %>%
mutate(sepal.width = replace(sepal.width, sepal.width ==  "-", NA)) %>%
mutate(petal.length = replace(petal.length, petal.length ==  "-", NA)) %>%
mutate(petal.width = replace(petal.width, petal.width ==  "-", NA))

df$sepal.length <- as.numeric(as.character(df$sepal.length))
df$sepal.width <- as.numeric(as.character(df$sepal.width))
df$petal.length <- as.numeric(as.character(df$petal.length))
df$petal.width <- as.numeric(as.character(df$petal.width))

df %>%
  summarise(sepal.length = sum(is.na(sepal.length)),
            sepal.width = sum(is.na(sepal.width)),
            petal.length = sum(is.na(petal.length)),
            petal.width = sum(is.na(petal.width)),
            variety = sum(is.na(variety)))

#2
df %>%
  summarise(sepal.length = sum(check(sepal.length) == TRUE),
            sepal.width = sum(check(sepal.width) == TRUE),
            petal.length = sum(check(petal.length) == TRUE),
            petal.width = sum(check(petal.width) == TRUE))


df <- df %>%
  mutate(sepal.length = replace(sepal.length,
        check(sepal.length),
        median(sepal.length, na.rm = TRUE)))%>%
  mutate(sepal.width = replace(sepal.width,
        check(sepal.width),
        median(sepal.width, na.rm = TRUE)))%>%
  mutate(petal.length  = replace(petal.length ,
        check(petal.length ),
        median(petal.length , na.rm = TRUE)))%>%
  mutate(petal.width = replace(petal.width,
        check(petal.width),
        median(petal.width, na.rm = TRUE)))
  

df %>%
  summarise(sepal.length = sum(check(sepal.length) == TRUE),
            sepal.width = sum(check(sepal.width) == TRUE),
            petal.length = sum(check(petal.length) == TRUE),
            petal.width = sum(check(petal.width) == TRUE))
#3
df <- df %>%
mutate(variety = replace(variety, is.element(variety, flower) == FALSE, sample(flower,1))) 

df
