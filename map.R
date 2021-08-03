## -- File to get statistics for DB paper -- #

library(dplyr)
library(stringr)
library(ggplot2)
library(ape)
library(phytools)
library(rphyloxml)

colour_palatte = c('#297AB1', '#57B5ED', '#ED5C4D', '#FBBE4B', 
                   "#FF9438", "#8980D4", "#AB4E68",
                   '#BFD7E8', '#BCE1F8', '#C6DDCC', 
                   '#FDE5B7', '#FFD4AF', "#D0CCEE", 
                   "#F8D2BC", "#17C3B2", "#17C3B2")

# -- Data -- #
languages = read.csv('./kinbank/kinbank/cldf/languages.csv')
forms = read.csv('./kinbank/kinbank/cldf/forms.csv')
parameters = read.csv('./kinbank/kinbank/cldf/parameters.csv')

glottolog = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/csv/glottolog.csv')

## -- Number of languages -- ##
nrow(languages)

## -- Number of Parameters -- ##
nrow(parameters)

## frequency of parameters less than 10
sum(table(forms$Parameter_ID) < 10)

## -- Language families -- ##
language_families = left_join(languages, glottolog, by = c("Glottocode" = "id"))

# number of langauges families 
length(unique(language_families$family_name))
# frequency of languages families 
sort(table(language_families$family_name))

# -- Dialect examples -- #
languages[str_detect(languages$ID, "[a-z]{4}[0-9]{4}[a-z]"),c("Name", "Glottocode", "ID")]

# --- MAP -- #
bantu_taxa = read.csv('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/phylogenies/grollemund_et_al2015/taxa.csv')

languages$family_plot = languages$Family

languages$family_plot = ifelse(
  languages$Glottocode %in% bantu_taxa$glottocode, "Bantu", languages$family_plot
)

language_families = c("Austronesian", "Bantu", "Indo-European",
                      "Pama-Nyungan", "Tupian", "Uto-Aztecan")

languages$family_cols = ifelse(languages$family_plot %in% language_families,
                               languages$family_plot, "other")

languages$family_labels = ifelse(languages$family_plot %in% language_families,
                                 languages$family_plot, "Other")

languages$family_labels = factor(languages$family_labels, 
                                 levels = c("Austronesian", "Pama-Nyungan", "Indo-European",
                                            "Bantu", "Uto-Aztecan", "Tupian", "Other"))

languages$Longitude2 <- ifelse(languages$Longitude < -25, 
                               languages$Longitude + 360, 
                               languages$Longitude) # where d is your df


map_df = left_join(languages, data.frame(family_plot = c(language_families), 
                                         cols = c(colour_palatte[1:length(language_families)])))

map_df$cols = ifelse(is.na(map_df$cols), 
                     rgb(0.5,0.5,0.5, alpha = 0.6, maxColorValue = 1), map_df$cols)



mapWorld <- map_data('world', wrap=c(-25,335), ylim=c(-55,75))


plot_map = 
  ggplot() +
  geom_polygon(data = mapWorld, aes(x=long, y = lat, group = group), 
               fill="#F4E9DA", col="black") +
  geom_point(data = map_df, aes(x = Longitude2, y = Latitude,
                                fill = family_labels), 
             size = 2.5, pch = 21) +
  theme_void() +
  scale_fill_manual(values=
                       c(colour_palatte[2:(length(language_families) + 1)], 
                         rgb(0.5,0.5,0.5, alpha = 0.6, maxColorValue = 1))) + 
  theme(legend.position = 'bottom',
        legend.title = element_blank(), legend.text=element_text(size=13)) 

plot_map
ggsave(filename = "map.png", plot = plot_map)

# -- Unique terms -- #
length(forms$Form[!is.na(forms$Form)])
