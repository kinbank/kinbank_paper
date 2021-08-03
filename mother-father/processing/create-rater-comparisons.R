# inter rater reliability
library(readxl)
library(dplyr)

# data split

sam = read_xlsx('mother-father/data/coded-data.xlsx', sheet = 1)

# remove uncoded data
sam = sam %>% filter(!is.na(consonant) & !is.na(vowel))

# pick 100 words at random
set.seed(994433)
idx = sample(1:nrow(sam), size = 100, replace = FALSE)

# my ansers
sam_coded = write.csv(sam[idx,], 'mother-father/data/iir/sam-coded.csv')

# rater answers
new_rater = sam[idx,]
new_rater$consonant = NA
new_rater$vowel = NA
new_rater$reduplication = NA
new_rater$...10 = NA

write.csv(new_rater, 'mother-father/data/iir/empty_sheet.csv')
