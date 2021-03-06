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
  # `prod` means product all elements
  prob_unique <- prod((seq(365, 365 - num + 1) / 365))
  1 - prob_unique
}

eprob <- sapply(num, same_birthday_with_math)

library(tidyverse)
png("mc_and_math.png")
qplot(num, prob) + geom_line(aes(num, eprob), col = "red")

# Check the stability

num <- 10 ^ seq(1, 5, len = 100)
prob <- sapply(num, function(b, num=25) {
  # Replace `num` with `b` for sapply
  same_birthday_with_montecarlo(num, b)
})

png("stability.png")
qplot(log10(num), prob, geom = "line")
