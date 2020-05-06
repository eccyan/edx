library(gtools)
library(tidyverse)

# 1.

ball_colors <- c("cyan", "magenta", "yello")
ball_counts <- c(3, 5, 7)
balls <- rep(ball_colors, ball_counts)
cyan <- c("cyan")
p_1 <- length(balls[balls %in% cyan]) / length(balls)

print(p_1)

# 2.

not_cyan <- balls[!balls %in% cyan]
p_2 <- length(not_cyan) / length(balls)

print(p_2)

# 3.

ball_counts_3 <- c(3 - 1, 5, 7)
balls_3 <- rep(ball_colors, ball_counts_3)
not_cyan_3 <- balls_3[!balls_3 %in% cyan]

p_3 <- p_1 * length(not_cyan_3) / length(balls_3)

print(p_3)

# 4.

p_4 <- p_1 * p_2
print(p_4)

# 5.

# -> b.

# 6.


ball_colors_6 <- c("cyan", "magenta", "yello")
ball_counts_6 <- c(3, 5, 7 - 5)
balls_6 <- rep(ball_colors_6, ball_counts_6)
yello <- c("yello")

p_6 <- length(balls_6[balls_6 %in% yello]) / length(balls_6)
print(p_6)

# 7.

library(gtools)
dice <- c(1:6)
roll_count <- 2
comb <- permutations(length(dice), roll_count, v = dice, repeats = TRUE)
excludes <- c(6)
see_excludes <- apply(comb, 1, function(row) {
  any(row %in% excludes)
})
p_7 <- mean(!see_excludes)

print(p_7) # Can calculate with: (5/6)^6

# 8.

games <- c(1:7)
cavs_winning_rate <- 0.6
all_lose_prob <- prod(rep(1 - cavs_winning_rate, times = length(games)))
p_8 <- 1 - all_lose_prob # Each games probability are independent

print(p_8)

# 9.

b_9 <- 100000

cavs_wins_least_one <- function() {
  celtic_wins <- sample(c(0, 1), length(games), replace = TRUE,
    prob = c(cavs_winning_rate, 1 - cavs_winning_rate))
  any(!celtic_wins)
}
tab <- table(replicate(b_9, cavs_wins_least_one()))
p_9 <- prop.table(tab)["TRUE"]

print(p_9)

# 10.

games <- c(1:7)
cavs_winning_rate <- 0.5
rate <- function(count, inverse= FALSE) {
  rate <- ifelse(inverse, 1 - cavs_winning_rate, cavs_winning_rate)
  prod(rep(rate, times = count))
}

comb <- function(count, win) {
  nrow(combinations(count, win)) *
    rate(win) * rate(count - win, inverse = TRUE)
}

# win 4:0 => never
win4_0 <- 0

# win 4:1 => 4 win cavs for next 4 games
win4_1 <- rate(4)
print(win4_1)

# win 4:2 => 3 win for next 4 games, win 5th game
# -++++-
# +-+++-
# ++-++-
# +++-+-
win4_2 <- comb(4, 3) * rate(1)
print(win4_2)

# win 4:3 => 3 win for next 5 games, win 5th game
# --++++
# +--+++
# ++--++
# +++--+
# -+-+++
# -++-++
# -+++-+
# +-+-++
# +-++-+
# ++-+-+
win4_3 <- comb(5, 3) * rate(1)

# those are binomial probability: nCx⋅p^x⋅(1−p)^(n−x)
p_10 <- win4_0 + win4_1 + win4_2 + win4_3
print(p_10)

# 11.

games <- c(2:7)
cavs_winning_rate <- 0.5
wins <- function() {
  sample(c(0, 1), length(games), replace = TRUE,
    prob = c(cavs_winning_rate, 1 - cavs_winning_rate))
}

check_wins <- function(wins) {
  win4_1 <- sum(wins[1:4]) >= 4
  win4_2 <- sum(wins[1:4]) >= 3 & wins[5] == 1
  win4_3 <- sum(wins[1:5]) >= 3 & wins[6] == 1
  win4_1 | win4_2 | win4_3
}

p_11 <- mean(replicate(10000, check_wins(wins())))

print(p_11)

# 12.

prob_win <- function(p) {
  b <- 10000
  result <- replicate(b, {
    b_win <- sample(c(1, 0), 7, replace = TRUE, prob = c(1 - p, p))
    sum(b_win) >= 4
  })
  mean(result)
}
print(prob_win(1))

p <- seq(0.5, 0.95, 0.025)
pr <- sapply(p, prob_win)
png("exercise12.png")
qplot(p, pr)

# 13.

p <- 0.75
game_count <- seq(1, 25, 2)
prob_win <- function(num, p) {
  b <- 10000
  result <- replicate(b, {
    b_win <- sample(c(1, 0), num, replace = TRUE, prob = c(1 - p, p))
    sum(b_win) >= (num + 1) / 2
  })
  mean(result)
}

pr <- sapply(game_count, function(num) {
  prob_win(num, p)
})
png("exercise13.png")
qplot(game_count, pr)
