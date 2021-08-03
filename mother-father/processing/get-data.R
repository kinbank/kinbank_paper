library(dplyr)

forms = read.csv('../kinbank/kinbank/cldf/forms.csv')
languages = read.csv('../kinbank/kinbank/kinbank/cldf/languages.csv')

forms = left_join(forms, languages, c("Language_ID" = "ID"))

mf = forms %>% 
  filter(Parameter_ID %in% c("mM", "mF", "fM", "fF") & !is.na(Form)) %>%
  dplyr::select(ID, Glottocode, Parameter_ID, Form)


mf$speaker = ifelse(substr(mf$Parameter_ID, 1, 1) == "f", "f", "m")
mf$mf = substr(mf$Parameter_ID, 2, 2) # mother or father

table(mf$Parameter_ID)

length(unique(mf$Glottocode))

# how many languages have sex of speaker distinctions in parents:
gc = unique(mf$Glottocode)
kintypes = unique(mf$mf)
sos = c(NA, NA, NA)
for(i in 1:length(gc)){
  glottocode = gc[i]
  ss = mf %>% 
    filter(Glottocode == glottocode) # subset to one language
  
  language = matrix(NA, ncol = 3, nrow = 2)
  for(j in 1:length(kintypes)){
    k = kintypes[j]
    m = ss %>% # subset to male speakers
      filter(mf == k & speaker == "m")
   
   f = ss %>% # subset to female speakers
     filter(mf == k & speaker == "f")

   language[j,] = c(glottocode, k, all(f$Form == m$Form))
  }
  sos = rbind(sos, language)
}
sos = as.data.frame(sos)
colnames(sos) = c("Glottocode", "Kintype", "SoS")
table((sos[,3]), sos[,2]) # only 15 languages have a sex of speaker distinction

sos_gc = sos %>% 
  filter(SoS == FALSE) %>% 
  select(Glottocode) %>% 
  distinct()


## -- Create data to code -- ## 
# we need three data columns:
## Language, kin type, form
# and three coding columns:
## consonant, vowel, reduplication 


## We have already formatted MF for the columns we need - so we just need to add the coding columns
mf$consonant = NA # factor
mf$vowel = NA # factor
mf$reduplication = NA # logical

# only m speaking
mf = mf %>% 
  filter(substr(Parameter_ID, 1, 1) == "m")

write.csv(mf, 'processed_data/uncoded-data.csv')

  