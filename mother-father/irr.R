## This script tests the IRR between Angarika Deb and Peter Racz
## Then tests their reliability against Sam Passmore's coding (who coded the dataset)

library(readxl)
library(tidyverse)
library(dplyr)
library(irr)

ad = read_xlsx("data/DebA_coder_AD.xlsx")

# Some words were not coded by AD - remove them
ad = ad[nchar(ad$consonant) == 1 & nchar(ad$vowel) == 1,]

# Some words were not coded by PR - remove them
pr = read.csv('data/PR_coder.tsv', sep = "\t")
pr = pr[nchar(pr$consonant) == 1 & nchar(pr$vowel) == 1,]

sp = read_xlsx('data/coded-data.xlsx', sheet= 1)

# put the hidden ids back
masked_ids = read.csv('processed_data/hidden_ids.csv')

ad = left_join(ad, masked_ids, by = "new_id")
pr = left_join(pr, masked_ids, by = "new_id")

rater_df = left_join(ad, pr, "ID", suffix = c(".ad", ".pr"))

## AD vs PR

## Consonant agreement
consonant_agreement = rater_df[,c("consonant.ad", "consonant.pr")] %>% 
  drop_na()

kappa2(consonant_agreement, weight = "unweighted")


## Vowel agreement
vowel_agreement = rater_df[,c("vowel.ad", "vowel.pr")] %>% 
  drop_na()
  
kappa2(vowel_agreement, weight = "unweighted")

## AD & SP
adsp = left_join(ad, sp, "ID", suffix = c(".ad", ".sp"))

## Consonant agreement
consonant_agreement = 
  adsp[,c("consonant.ad", "consonant.sp")] %>% 
  drop_na()

kappa2(consonant_agreement, weight = "unweighted")

## Vowel agreement
vowel_agreement =   
  adsp[,c("vowel.ad", "vowel.sp")] %>% 
  drop_na()

kappa2(vowel_agreement, weight = "unweighted")


# PR & SP
prsp = left_join(pr, sp, "ID", suffix = c(".pr", ".sp"))

## Consonant agreement
consonant_agreement = 
  prsp[,c("consonant.pr", "consonant.sp")] %>% 
  drop_na()

kappa2(consonant_agreement, weight = "unweighted")

## Vowel agreement
vowel_agreement =   
  prsp[,c("vowel.pr", "vowel.sp")] %>% 
  drop_na()

kappa2(vowel_agreement, weight = "unweighted")

## Multirater agreement (SP AD PR)
multirater_df = left_join(sp, rater_df, "ID")

## Consonant agreement
consonant_agreement = 
  multirater_df[,c("consonant", "consonant.ad", "consonant.pr")] %>% 
  drop_na()

kappam.light(consonant_agreement)

## Vowel agreement
vowel_agreement =   
  multirater_df[,c("vowel", "vowel.ad", "vowel.pr")] %>% 
  drop_na()

kappam.light(vowel_agreement)

