library(dplyr)
library(tidyr)
library(stringr)
library(assertthat)

# Get Grollemund Taxa data
taxa = read.csv('data/phylogeny/taxa.csv')
taxa$ea_id = stringr::str_extract(taxa$soc_ids, "[A-Za-z]{1,2}[0-9]{1,2}")

# Get D-PLACE (for CCM)
dplace = read.csv('data/dplace/societies.csv')

# Which taxa are in D-PLACE
taxa$in_dplace = ifelse(taxa$glottocode %in% dplace$glottocode, 1, 0)
assert_that(sum(taxa$in_dplace) == 154, msg = "There should be 154 Bantu taxa in D-PLACE. Something has changed!")

# Which Taxa have CCM data
dplace_vars = read.csv('data/dplace/data.csv')
var = c("EA023", "EA024", "EA025", "EA026", "EA027")

dv_ss = dplace_vars %>%
  filter(var_id %in% var) %>%
  filter(soc_id %in% taxa$ea_id)

# Make data wide
dv_ss_w = spread(dv_ss[,c("soc_id", "var_id", "code")], key = "var_id", value = "code")  

# Join taxa and DPLACE
taxa = left_join(taxa, dv_ss_w, by = c("ea_id" = "soc_id"))

assert_that(sum(!is.na(taxa$EA023)) == 87, msg = "Data from EA023 does not equal 87. Something has changed!")
assert_that(sum(!is.na(taxa$EA025)) == 87, msg = "Data from EA025 does not equal 87. Something has changed!")

## Kinbank languages
languages = read.csv('../kinbank/kinbank/cldf/languages.csv')

## Which kinbank languages are in the taxa-dplace subset
taxa$in_kinbank = ifelse(taxa$glottocode %in% languages$Glottocode, 1, 0)

# how many data points?
assert_that(sum(taxa$in_kinbank & !is.na(taxa$EA023)) == 74, msg = "Data in tree & in dplace does not equal 74. Something has changed!")

## Get the kintypes for Bi-furcate merging 
kinbank_id = taxa$glottocode[taxa$in_kinbank == 1 & !is.na(taxa$EA023)]
parameters = c("mF", "mFeB", "mFyB", "mMeB", "mMyB", "mWF",
              "mM", "mFeZ", "mFyZ", "mMeZ", "mMyZ", "mWM")
forms = read.csv('../kinbank/kinbank/cldf/forms.csv') %>% 
  left_join(., languages, c("Language_ID" = "ID")) %>% 
  filter(Parameter_ID %in% parameters)

# Concatenate kin terms when languages have multiple forms
forms = forms %>% 
  group_by(Parameter_ID, Glottocode) %>% 
  mutate(Form2 = paste0(Form, collapse = "; ")) %>%
  slice(1) %>% 
  ungroup()

source = unique(forms[,c("Glottocode", "Source")]) %>% 
  dplyr::filter(Source != "" & Glottocode %in% taxa$glottocode)

source$date = str_extract(source$Source, "[0-9]+$") %>% as.numeric()
source$datediff = 1966 - source$date

forms_w = spread(forms[,c("Glottocode", "Parameter_ID", "Form2")], key = "Parameter_ID", value = "Form2") %>%
  dplyr::select(Glottocode, all_of(parameters))

## merge kinbank and dplace data
out = inner_join(taxa, forms_w, by = c("glottocode" = "Glottocode"))

# Save for manual coding
write.csv(out, 'processed_data/bantu_data.csv', na = "", row.names = FALSE)
