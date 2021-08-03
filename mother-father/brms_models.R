# new models
library(brms)
library(ape)
library(readxl)
library(dplyr)
library(bayesplot)
library(ggplot2)
library(beepr)
library(stringr)
library(bayestestR)
library(parallel)
library(rstan)

#setwd("mother-father/")
# -- Get Data -- #
# 3068
df = read_xlsx('data/coded-data.xlsx', sheet= 1)
super_tree = read.tree('processed_data/super_tree.nwk')

# get glottocodes 
glottolog = read.csv('data/glottolog.csv')
df = left_join(df, glottolog, c("Glottocode" = "id")) %>%
  dplyr::filter(family_id != "" )

# -- remember to subset to unique language - kin type - kin term combos -- #
# -- since we analyse all sources, this will remove when sources agree -- #
df = df %>% 
  group_by(Glottocode, Parameter_ID, Form) %>% 
  slice(1)


## WHILE STILL CODING
df = df %>% 
  dplyr::filter(!is.na(consonant)) %>% 
  dplyr::filter(!is.na(vowel)) %>% 
  dplyr::filter(vowel != "?")
nrow(df)


# data checks
all(df$vowel %in% c("i", "e", "E", "3", "a", "u", "o", "!"))
all(df$consonant %in% c("p", "b", "m", "f", "v", "8" ,"4", "t", "d","s","z","c","n","S","Z","C","j","T","5","k","g","x","N","q","G","X","7","h","l","L","w","y","r","!"))

# get response
df$mother = ifelse(df$mf == "M", 1, 0)
df$father = ifelse(df$mf == "F", 1, 0)
df$super_tree = df$Glottocode
df$reduplication = ifelse(is.na(df$reduplication), 0, 1)

## - Set up Phylogeny - ##
data_gc = unique(df$Glottocode)
data_gc = data_gc[data_gc %in% super_tree$tip.label]
super_tree = keep.tip(super_tree, data_gc)
# cov phylo matrix
A <- ape::vcv.phylo(super_tree)

# check all languages are included
all(super_tree$tip.label %in% data_gc)
all(data_gc %in% super_tree$tip.label)

# -- Models -- #

# -- parameters -- #
iter = 5000
warmup = 2000
chains = 4
mtd = 15 # max tree depth
## speed ups?
ncores = parallel::detectCores()
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# using the file argument will save this model. 
# running this again will load the file
# to re-run delete the file and run this code. 

# without phylogeny
model_plain <- brm(
  data = df, family = bernoulli,
  mother ~ vowel + consonant, 
  prior = c(
    prior(normal(0,10), "b")
  ),
  sample_prior = TRUE, chains = chains, cores = ncores,
  iter = iter, warmup = warmup, control=list(adapt_delta=0.99, max_treedepth = mtd),
  file = "results/mother_nophylo.rds"
)

model_mother <- brm(
  data = df, family = bernoulli,
  mother ~ vowel + consonant + (1|gr(super_tree, cov = A)) + (1|Glottocode), 
  data2 = list(A = A),
  prior = c(
    prior(normal(0,10), "b"),
    prior(student_t(3,0,20), "sd")
    ),
  sample_prior = TRUE, chains = chains, control=list(adapt_delta=0.99, max_treedepth = mtd),
  iter = iter, warmup = warmup, cores = ncores, file = "results/mother_withphylo.rds"
)
beepr::beep(4)

# model_father <- brm(
#   data = df, family = bernoulli,
#   father ~ vowel + consonant + (1|gr(super_tree, cov = A)) + (1|Glottocode), 
#   data2 = list(A = A),
#   prior = c(
#     prior(normal(0,10), "b"),
#     prior(student_t(3,0,20), "sd")
#   ),
#   sample_prior = TRUE, chains = 1, control=list(adapt_delta=0.99, max_treedepth = mtd),
#   iter = 4000, warmup = 1000, file = "results/father_withphylo.rds"
# )
# beepr::beep(4)

# -- Get posterior -- #
params = parnames(model_mother)[str_detect(parnames(model_mother), "b_")]

# - plain model - #
no_phylo = posterior_samples(model_plain, pars = params)
# get combinations of interest
np_ofinterest = data.frame(
  ma = no_phylo$b_Intercept + no_phylo$b_consonantm + no_phylo$b_vowela,
  nga = no_phylo$b_Intercept + no_phylo$b_consonantN + no_phylo$b_vowela,
  na = no_phylo$b_Intercept + no_phylo$b_consonantn + no_phylo$b_vowela,
  pa = no_phylo$b_Intercept + no_phylo$b_consonantp + no_phylo$b_vowela,
  ta = no_phylo$b_Intercept + no_phylo$b_consonantt + no_phylo$b_vowela
)
np_ofinterest = apply(np_ofinterest, 2, inv_logit_scaled)

# - mother & phylogenetically controlled model - #
mother = posterior_samples(model_mother, pars = params)
# get combinations of interest
m_ofinterest = data.frame(
  ma = mother$b_Intercept + mother$b_consonantm + mother$b_vowela,
  nga = mother$b_Intercept + mother$b_consonantN + mother$b_vowela,
  na = mother$b_Intercept + mother$b_consonantn + mother$b_vowela,
  pa = mother$b_Intercept + mother$b_consonantp + mother$b_vowela,
  ta = mother$b_Intercept + mother$b_consonantt + mother$b_vowela
)
m_ofinterest = apply(m_ofinterest, 2, inv_logit_scaled)

apply(m_ofinterest, 2, summary)

# --  PLOTS -- #
my_scheme <- c("#FBBE4B", "#ED5C4D",
                   "#ED5C4D", "#F4E9DA",
                   "#273253", "#ED8F57")
color_scheme_set(my_scheme)

# mcmc_areas(np_ofinterest,
#            prob = 0.8)
# 
# mcmc_trace(mother, pars = params)

# Summary statistics
plot_hdi = apply(m_ofinterest,2, function(x){
  interval = hdi(x)
  paste0("[", round(interval$CI_low, 2), ", ", round(interval$CI_high, 2), "]")
})

y_points = 5:1 + 0.8
x_points = apply(m_ofinterest, 2, median) + .11

grDevices::cairo_pdf('figures/prob_mother.pdf')
mcmc_areas_ridges(m_ofinterest,
           prob = 0.89) + ggtitle("Probability of sounds referring to mother",
          "Posterior distribution with 89% intervals") +
  vline_at(0.5, col = "#ED5C4D", lty = "dashed") +
  xlim(c(0, 1)) + 
  scale_y_discrete(labels=c("ma" = "ma",
                            "nga" = "ŋa",
                            "na" = "na",
                            "pa" = "pa",
                            "ta" = "ta"
                            ), limits = rev(c("ma", "nga", "na", "pa", "ta"))) +
  geom_text(x=x_points[1], y=y_points[1], label=plot_hdi[1], family = "serif", fontface = "plain") +
  geom_text(x=x_points[2], y=y_points[2], label=plot_hdi[2], family = "serif", fontface = "plain") +
  geom_text(x=x_points[3], y=y_points[3], label=plot_hdi[3], family = "serif", fontface = "plain") +
  geom_text(x=x_points[4], y=y_points[4], label=plot_hdi[4], family = "serif", fontface = "plain") +
  geom_text(x=x_points[5], y=y_points[5], label=plot_hdi[5], family = "serif", fontface = "plain") + 
  annotate(geom = "text", label = "Mother", x = 0.95, y = 0.65, size = 5) +
  annotate(geom = "text", label = "Father", x = 0.05, y = 0.65, size = 5) + 
  theme(text = element_text(size=20))
dev.off()


### SM plots
mcmc_areas(
  posterior, 
  pars = c("cyl", "drat", "am", "sigma"),
  prob = 0.8, # 80% intervals
  prob_outer = 0.99, # 99%
  point_est = "mean"
)