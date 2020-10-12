library(tidyverse)

url_main <- "https://raw.githubusercontent.com/beatrizeg/Wish-Units-Solds/main/summer-products-with-rating-and-performance_2020-08.csv"
dest_file <- "data/main.csv"
download.file(url_main, destfile = dest_file)
main <- read_csv("data/main.csv")
save(main, file = "rdas/main.rda")

url_cat <- "https://raw.githubusercontent.com/beatrizeg/Wish-Units-Solds/main/unique-categories.sorted-by-count.csv"
dest_file_cat <- "data/cat.csv"
download.file(url_cat, destfile = dest_file_cat)
cat <- read_csv("data/cat.csv")
save(cat, file = "rdas/cat.rda")