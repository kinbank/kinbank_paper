library(readxl)
library(ape)
library(bayestraitr)
library(stringr)
library(geiger)
library(dplyr)
library(assertthat)

# Get Data ----

## NOTE: I manually determined whether there was merging in kin types
## within Men and within Women. If both = 1, then they are a complete bifurcate merging system

# EA023: Permittance of cross-cousin marriage 
# All codes > 6 indicate some form of first-cousin, cross-cousin marriage
# 1 	Duolateral cross-cousin marriage permitted, i.e., marriage allowed with either MoBrDa or FaSiDa but forbidden with a parallel cousin 	206
# 2 	Duolateral marriage permitted with paternal cousins only (FaBrDa or FaSiDa) 	1
# 3 	Duolateral marriage permitted with maternal cousins only (MoBrDa or MoSiDa) 	8
# 4 	Duolateral marriage permitted with an uncle's daughter only (FaBrDa or MoBrDa) 	0
# 5 	Duolateral marriage permitted with an aunt's daughter only (FaSiDa or MoSiDa) 	0
# 6 	Unilateral: only matrilateral cross-cousin marriage permitted, i.e., with a MoBrDa 	44
# 7 	Nonlateral marriage, i.e., unions forbidden with any first or second cousin 	282
# 8 	Nonlateral marriage, evidence available only for first cousins 	277
# 9 	Unilateral: only patrilateral cross-cousin marriage permitted i.e., with a FaSiDa 	5
# 10 	Quadrilateral marriage, i.e., marriage allowed with any first cousin 	117
# 11 	Nonlateral marriage in which all first cousins and some but not all second cousins are forbidden as spouses 	13
# 12 	Nonlateral marriage in which unions are forbidden with any first cousin but are permitted with any second cousin (or at least any who is not a lineage mate) 	64
# 13 	Trilateral marriage, i.e., marriage allowed with any first cousin except an orthocousin or lineage mate 	25

# Bring in Structural changes
d = read_xlsx('data/clean_data.xlsx', sheet = 1, skip = 1) %>% 
  as.data.frame()
rownames(d) = d$taxon

# If any first cousin cross-cousin marriage 1 else 0
d$ccm = ifelse(d$EA023 <= 6 | d$EA023 == 9, 1, 0)

# Linguistic variables
## We have three variables: 
### 1. Crossed female relatives
### 2. Crossed male relatvies
### 3. Both male and females are crossed
#### 1 & 2 are created manually in the previous section

# if data is missing for eith M or F then it is missing for the complete cross
d$both_cross = ifelse(d$bifurcate_merging_M < 0 | d$bifurcate_merging_W < 0, NA,
                          ifelse(d$bifurcate_merging_M == 1 & d$bifurcate_merging_W == 1, 1, 0))

d = d %>% 
  dplyr::filter(bifurcate_merging_M != -1 | bifurcate_merging_W != -1)

d$bifurcate_merging_M = ifelse(d$bifurcate_merging_M < 0, NA, 
                               d$bifurcate_merging_M)
d$bifurcate_merging_W = ifelse(d$bifurcate_merging_W < 0, NA, 
                               d$bifurcate_merging_W)
d$both_cross = ifelse(d$both_cross < 0, NA, 
                               d$both_cross)

tree = read.tree('data/phylogeny/posterior_new.trees')

# Save data ----
bt_write(tree, 
         d, 
         variables = c('bifurcate_merging_W', "ccm"), 
         filename = "processed_data/fXpermitted", 
         na.omit = FALSE)
bt_write(tree, 
         d, 
         variables = c('bifurcate_merging_M', "ccm"), 
         filename = "processed_data/mXpermitted",
         na.omit = FALSE)
bt_write(tree, 
         d, 
         variables = c('both_cross', "ccm"), 
         filename = "processed_data/bothXpermitted",
         na.omit = FALSE)
