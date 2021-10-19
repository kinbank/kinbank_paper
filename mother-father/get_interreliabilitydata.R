library(readxl)
library(dplyr)
library(assertthat)
library(openxlsx)
library(stringr)

set.seed(2021)

data = read_xlsx('mother-father/data/coded-data.xlsx', sheet = "data")
vowels = read_xlsx('mother-father/data/coded-data.xlsx', sheet = "vowels")
consonants = read_xlsx('mother-father/data/coded-data.xlsx', sheet = "consonant")

# SP has already coded all 3,534 rows
# Two additional coders code 120 rows each with 60 kinterms overlapping
# The 120 rows will be equally split between M and F, 
# as will the 60 overlapping rows
# This will check 180 rows total or 5% of the sample

intercode_set = data %>% 
  group_by(Parameter_ID) %>% 
  sample_n(size = 90)

# Coder 1 is a random sample of the set chosen for intercoder reliability
coder_1 = intercode_set %>% 
  group_by(Parameter_ID) %>% 
  sample_n(60)

assert_that(all(table(coder_1$mf) == 60))
assert_that(nrow(coder_1) == 120)

# Coder 2 is all rows not chosen by Coder 1 & 
# 60 that were (30 each for M & F)
coder_2a = intercode_set %>% 
  dplyr::filter(!(ID %in% coder_1$ID))

assert_that(nrow(coder_2a) == 60)

coder_2b = coder_1 %>%
  group_by(Parameter_ID) %>% 
  sample_n(30) 

coder_2 = rbind(coder_2a, coder_2b)

assert_that(all(table(coder_2$mf) == 60))
assert_that(nrow(coder_2) == 120)


# Hide ids
existing_id = unique(c(coder_1$ID, coder_2$ID) )
new_id = data.frame(ID = existing_id, 
                    new_id = 1:length(existing_id))

coder_1 = left_join(coder_1, new_id)
coder_2 = left_join(coder_2, new_id)

write.csv(new_id, "mother-father/processed_data/hidden_ids.csv")

# save coding sheets
coder_1 = coder_1 %>% 
  ungroup() %>% 
  select(new_id, Glottocode, Form)

coder_1$consonant = ""
coder_1$vowel = ""

assert_that(nrow(coder_1) == 120)

c1 = list(kinterms = coder_1, consonants = consonants, vowels = vowels)
write.xlsx(c1, file = "mother-father/processed_data/coder_1.xlsx")

coder_2$ID2 = str_extract(coder_2$ID, ".+?(?=[a-z]{4}[0-9]{4})") %>% 
  str_remove("[vp]_")

coder_2 = coder_2 %>% 
  ungroup() %>% 
  select(new_id, ID2, Glottocode, Form)

coder_2$consonant = ""
coder_2$vowel = ""

assert_that(nrow(coder_2) == 120)
assert_that(!all(coder_2$new_id == coder_1$new_id))

c2 = list(kinterms = coder_2, consonants = consonants, vowels = vowels)
write.xlsx(c2, file = "mother-father/processed_data/coder_2.xlsx")
