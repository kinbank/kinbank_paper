# glottolog distance
library(readxl)
library(stringr)
library(fs)

## -- Functions -- #
get_shortest = function(x){
  idx = nchar(x) == min(nchar(x))
  x[idx]
}

languages = read_xlsx('mother-father/data/coded-data.xlsx', sheet = 1)
glottocodes = unique(languages$Glottocode) %>% 
  sort()


glotto_distance = matrix(NA, 
                         ncol = length(glottocodes), 
                         nrow = length(glottocodes))
dimnames(glotto_distance) = list(glottocodes, glottocodes) # alphabetical

# all locations in glottolog tree
files = fs::dir_ls(path = 'mother-father/raw/glottolog/languoids/tree/', 
           type = "directory", 
           glob = paste(glottocodes, collapse = "|"), recurse = TRUE)

files = str_replace(files, "mother-father/raw/glottolog/languoids/tree/", "")
locations = str_split(files, '/')


max_dist = lapply(locations, length) %>% unlist %>% max() + 1

names(locations) = glottocodes

for(i in 1:length(glottocodes)){
  gc = glottocodes[i]
  gc_location = files[str_detect(files, gc)] # detect locations with GC
  gc_location = get_shortest(gc_location) %>% 
    str_split("/") %>% 
    unlist()

  gc_dist = lapply(locations, function(x) sum(gc_location %in% x)) %>% unlist()
  all(rownames(glotto_distance) == names(gc_dist))
  glotto_distance[names(gc_dist),]
}
