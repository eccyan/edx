B <- 10000

monty_hall <- function(switch = FALSE) {
  doors <- as.character(1:3)
  prizes <- sample(c("car", "goat", "goat")) # Shuffle prizes with sample()
  car_door <- doors[prizes == "car"] # a[con] Returns vector satisfied condition
  picked_door <- sample(doors, 1)
  showable_doors <- doors[!doors %in% c(car_door, picked_door)]
  shown_door <- sample(showable_doors, 1)
  sticked <- picked_door
  switchable_doors <- doors[!doors %in% c(picked_door, shown_door)]
  switched <- sample(switchable_doors, 1)
  chose_door = ifelse(switch, switched, sticked)
  chose_door == car_door
}

stick <- replicate(B, monty_hall())
switch <- replicate(B, monty_hall(TRUE))

mean(stick)
mean(switch)
