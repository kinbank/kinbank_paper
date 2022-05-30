## This script tests the IRR between Angarika Deb and Peter Racz
## Then tests their reliability against Sam Passmore's coding (who coded the dataset)

library(readxl)
library(tidyverse)
library(dplyr)
library(irr)

# output table
coders = c("AD", "JC", "PR", "SP")
results = data.frame(t(combn(coders, 2)))
colnames(results) = c("Coder1", "Coder2")
results$vowel.agreement = NA
results$consonant.agreement = NA

ad = read_xlsx("data/DebA_coder_AD2.xlsx")
# Some words were not coded by AD - remove them
ad = ad[nchar(ad$consonant) == 1 & nchar(ad$vowel) == 1,]

# Some words were not coded by PR - remove them
pr = read.csv('data/PR_coder.tsv', sep = "\t")
pr = pr[nchar(pr$consonant) == 1 & nchar(pr$vowel) == 1,]
pr = pr %>% select(new_id, Glottocode, Form, consonant, vowel)

jc = read_xlsx('data/JC_coder.xlsx', sheet="Kinterms")
jc = jc[!is.na(jc$consonant) & !is.na(jc$vowel),]

# SP was the primary coder
sp = read_xlsx('data/coded-data.xlsx', sheet= 1)

# put the hidden ids back
masked_ids = read.csv('processed_data/hidden_ids.csv')
masked_ids = masked_ids %>% 
  select(ID, new_id)

ad = left_join(ad, masked_ids, by = "new_id")
pr = left_join(pr, masked_ids, by = "new_id")
jc = left_join(jc, masked_ids, by = "new_id")

# seperate coder cols
colnames(ad)[colnames(ad) %in% c("consonant", "vowel")] = 
  paste0(colnames(ad)[colnames(ad) %in% c("consonant", "vowel")], ".ad")
colnames(pr)[colnames(pr) %in% c("consonant", "vowel")] = 
  paste0(colnames(pr)[colnames(pr) %in% c("consonant", "vowel")], ".pr")
colnames(jc)[colnames(jc) %in% c("consonant", "vowel")] = 
  paste0(colnames(jc)[colnames(jc) %in% c("consonant", "vowel")], ".jc")

rater_df = left_join(ad, pr, c("ID", "new_id", "Glottocode", "Form")) %>% 
  left_join(., jc, c("ID", "new_id"))

## AD vs PR
idx = results$Coder1 == "AD" & results$Coder2 == "PR"
## Consonant agreement
consonant_agreement = rater_df[,c("consonant.ad", "consonant.pr")] %>% 
  drop_na()

results$consonant.agreement[idx] = kappa2(consonant_agreement, weight = "unweighted")$value

## Vowel agreement
vowel_agreement = rater_df[,c("vowel.ad", "vowel.pr")] %>% 
  drop_na()
  
results$vowel.agreement[idx] = kappa2(vowel_agreement, weight = "unweighted")$value


## AD vs JC
idx = results$Coder1 == "AD" & results$Coder2 == "JC"
## Consonant agreement
consonant_agreement = rater_df[,c("consonant.ad", "consonant.jc")] %>% 
  drop_na()

results$consonant.agreement[idx] = kappa2(consonant_agreement, weight = "unweighted")$value

## Vowel agreement
vowel_agreement = rater_df[,c("vowel.ad", "vowel.jc")] %>% 
  drop_na()

results$vowel.agreement[idx] = kappa2(vowel_agreement, weight = "unweighted")$value

## PR vs JC
idx = results$Coder1 == "JC" & results$Coder2 == "PR"
## Consonant agreement
consonant_agreement = rater_df[,c("consonant.pr", "consonant.jc")] %>% 
  drop_na()

results$consonant.agreement[idx] = kappa2(consonant_agreement, weight = "unweighted")$value

## Vowel agreement
vowel_agreement = rater_df[,c("vowel.pr", "vowel.jc")] %>% 
  drop_na()

results$vowel.agreement[idx] = kappa2(vowel_agreement, weight = "unweighted")$value

## AD & SP
adsp = left_join(ad, sp, "ID")

idx = results$Coder1 == "AD" & results$Coder2 == "SP"
## Consonant agreement
consonant_agreement = 
  adsp[,c("consonant.ad", "consonant")] %>% 
  drop_na()

results$consonant.agreement[idx] = kappa2(consonant_agreement, weight = "unweighted")$value

## Vowel agreement
vowel_agreement =   
  adsp[,c("vowel.ad", "vowel")] %>% 
  drop_na()

results$vowel.agreement[idx] = kappa2(vowel_agreement, weight = "unweighted")$value

# PR & SP
prsp = left_join(pr, sp, "ID")
idx = results$Coder1 == "PR" & results$Coder2 == "SP"
## Consonant agreement
consonant_agreement = 
  prsp[,c("consonant.pr", "consonant")] %>% 
  drop_na()

results$consonant.agreement[idx] = kappa2(consonant_agreement, weight = "unweighted")$value

## Vowel agreement
vowel_agreement =   
  prsp[,c("vowel.pr", "vowel")] %>% 
  drop_na()

results$vowel.agreement[idx] = kappa2(vowel_agreement, weight = "unweighted")$value

# JC & SP
jcsp = left_join(jc, sp, "ID")
idx = results$Coder1 == "JC" & results$Coder2 == "SP"
## Consonant agreement
consonant_agreement = 
  jcsp[,c("consonant.jc", "consonant")] %>% 
  drop_na()

results$consonant.agreement[idx] = kappa2(consonant_agreement, weight = "unweighted")$value

## Vowel agreement
vowel_agreement =   
  jcsp[,c("vowel.jc", "vowel")] %>% 
  drop_na()

results$vowel.agreement[idx] = kappa2(vowel_agreement, weight = "unweighted")$value

## Multirater agreement (SP AD PR)
multirater_df = left_join(sp, rater_df, "ID", suffix = c(".sp", ".jc"))

## Consonant agreement
total = c()
consonant_agreement = 
  multirater_df[,c("consonant", "consonant.ad", "consonant.pr", "consonant.jc")] %>% 
  drop_na()

total[2] = kappam.light(consonant_agreement)$value

## Vowel agreement
vowel_agreement =   
  multirater_df[,c("vowel", "vowel.ad", "vowel.pr", "vowel.jc")] %>% 
  drop_na()

total[1] = kappam.light(vowel_agreement)$value

results = rbind(results, c("All", "All", total))

write.csv(results, file = 'results/IRR_results.csv')
