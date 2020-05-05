sample_birthdays <- function(num) {
  birth_days <- sample(1:365, num, replace = TRUE)
  any(duplicated(birth_days))
}

same_birthday_with_montecarlo <- function(num, b=10000) {
  mean(replicate(b, sample_birthdays(num)))
}

num <- seq(1, 60)
prob <- sapply(num, same_birthday_with_montecarlo)

same_birthday_with_math <- function(num) {
  1 - prod((seq(365, 365 - num + 1) / 365))
}

eprob <- sapply(num, same_birthday_with_math)

library(tidyverse)
png("plot.png")
qplot(num, prob) + geom_line(aes(num, eprob), col = "red")
