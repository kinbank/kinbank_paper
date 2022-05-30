suppressPackageStartupMessages({
  library(ape)
  library(phytools)
  library(readxl)
  library(stringr)
})

#### Inputs ####
taxa_pairing = read.csv('data/taxa.csv')
jaeger_tree = read.tree('data/world.tre')
languages = read_xlsx('data/coded-data.xlsx', sheet= 1)

#### Tree - Data pairs ####
coverage = sum(languages$Glottocode %in% taxa_pairing$glottocode) / nrow(languages) 
cat("Jaeger tree contains", round(coverage, 2) * 100, "% of the langauges in Kinbank")

#### Relabel taxa in tree ####
current_taxa = jaeger_tree$tip.label
stripped_taxa = str_extract(current_taxa, "[^.]+$")

if(all(stripped_taxa == taxa_pairing$taxon)){
  jaeger_tree$tip.label = taxa_pairing$glottocode  
} else{
  cat("Jager tree taxa change failed.")
  stop()
}

#### Subset to Grambank langauges ####
in_tree = languages$Glottocode[languages$Glottocode %in% taxa_pairing$glottocode]
jaeger_pruned = keep.tip(jaeger_tree, in_tree)

scale = 1
jaeger_pruned$edge.length = jaeger_pruned$edge.length/max(nodeHeights(jaeger_pruned)[,2])*scale

write.tree(jaeger_pruned, "processed_data/jaeger_pruned.tree")

removed = languages$Glottocode[!languages$Glottocode %in% taxa_pairing$glottocode]
write.csv(removed, 'processed_data/jaeger_removed.csv')

cat("Jager tree created with", length(jaeger_pruned$tip.label), "tips\n")
cat(nrow(languages) - length(jaeger_pruned$tip.label), "languages were not paired\n")
cat("Languages that are in Grambank, but not in the Jaeger tree are in processed_data/jaeger_removed.csv\n")
