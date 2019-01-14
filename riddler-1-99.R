# Riddler 1-99 
# 530,131,801,762,787,739,802,889,792,754,109,70_,139,358,547,710,066,257,652,050,346,294,484,433,323,974,747,960,297,803,292,989,236,183,040,000,000,000
# base::prod()

# because of course
library(tidyverse)
library(gmp)

# target in character form
target_c <- "530,131,801,762,787,739,802,889,792,754,109,700,139,358,547,710,066,257,652,050,346,294,484,433,323,974,747,960,297,803,292,989,236,183,040,000,000,000"
targ_str <- stringr::str_remove_all(target_c, ",")

# numeric target
target <- as.bigz(targ_str)

# underscore for replacement
target_us <- "530,131,801,762,787,739,802,889,792,754,109,70_,139,358,547,710,066,257,652,050,346,294,484,433,323,974,747,960,297,803,292,989,236,183,040,000,000,000"

# without comma
targ_sus <- stringr::str_remove_all(target_us, ",")

# vector of 10 possible character strings
targ_vect <- stringr::str_replace(targ_sus, "_", as.character(0:9))

# vector of 10 possible big integers
targ_bigz <- as.bigz(targ_vect)

targ_bigz / 91

################################ 6 ################################

# characters
nchar(stringr::str_remove_all(target_c, ","))

# too small
prod(1:77)

# too big
prod(1:78)

# too small
prod(38:99)

# too big
prod(37:99)

# at most 77 elements
length(1:77)

# at least 63 elements
length(37:99)

# there's so so so many possibilities
choose(100, 70)

#check the help
?runif

# create a list
vect1 <- (runif(99) < 0.7) * 1:99
vect1a <- replace(vect1, vect1 == 0, 1)
prod(vect1a)

# create a function
prod_x <- function(ch = 70) {
  ch <- ch / 100
  vect1 <- (runif(99) < ch) * 1:99
  vect1a <- replace(vect1, vect1 == 0, 1)
}

# test function
prod_x(77)

# random list of numbers
possbl <- tibble(x = round(runif(10000, min = 62, max = 78)))

# create vector of which numbers to include and their product
possbl_v <- possbl %>% 
  mutate(vect = map(x, prod_x), 
         prodr = map_dbl(vect, prod), 
         diff_l = targ_l / log10(prodr), 
         subt_l = targ_l - log10(prodr), 
         close1 = abs(diff_l - 1), 
         close2 = abs(subt_l - 1), 
         sumr = map_dbl(vect, sum), 
         obs = 1:10000) %>% 
  arrange(close1)

# what's the best way to compare the results with the target? 
# subtract, divide, number of digits
# don't know how to get number of digits! 

# plot them on log 
# do the calculations with log!!!

ggplot(possbl_v, aes(x = x, y = diff_l)) + 
  geom_point(colour = "blue", alpha = 0.1) + 
  geom_hline(yintercept = 1e0)

# how do i reduce this search space further? 
# take the sum of the vector

ggplot(possbl_v, aes(x = sumr, y = diff_l)) + 
  geom_point(alpha = 0.1)

# see how common all the numbers are

vect_exp <- possbl_v %>% 
  unnest(vect) %>% 
  select(obs, vect) %>% 
  count(vect)

# 1 is most common, otherwise ~70 is average
# this is good, between 63 and 77
# not good for investigation
vect_exp %>% 
  filter(vect != 1) %>% 
ggplot(aes(x = vect, y = n)) + 
  geom_point()
  

# maybe have a look at the final digits
last <- c(rep(1:10, 9), 1:9)
last_tib <- tibble(lst = last)

# create a function
vect_single <- function(ch = 70) {
  ch <- ch / 100
  vect1 <- (runif(99) < ch) 
}

last_tib_2 <- last_tib %>% 
  mutate(vect = vect_single(70), 
         prod_use = if_else(vect, lst, 1L))

prod(last_tib_2$prod_use)

# still produces accuracy for the first 15 digits, not the last 15 digits
