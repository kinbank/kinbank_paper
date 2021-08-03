library(dplyr)
library(ape)
library(phytools)
library(phangorn)
library(readxl)
library(stringr)
library(geiger)

source('processing/getGlottologTree.R')

## -- Parameters -- ##
years = 6000 # how old the LF trees should be

# -- Data -- #
mofa = read_xlsx('data/coded-data.xlsx', sheet = 1)
glottolog = read.csv('~/OneDrive - University of Bristol/projects/dplace-data/csv/glottolog.csv')

languages = left_join(mofa, glottolog, by = c("Glottocode" = "id"))

unq_languages = unique(languages$Glottocode)

families = unique(languages$family_id)
families = families[families != ""]

isolate = c()
for(f in families){
  print(f)
  tre = getGlottologTree(f)
  
  all_keeps = c(unq_languages[unq_languages %in% tre$node.label],
                unq_languages[unq_languages %in% tre$tip.label])
  
  save = FALSE
  # make nodes that I have into tips
  if(any(tre$node.label %in% unq_languages)){
    nodes = tre$node.label[tre$node.label %in% unq_languages]
    for(nod in nodes){
      print(nod)
      n_node = length(tre$tip.label) + which(tre$node.label == nod)
      tre = add.tips(tre, tips = nod, where = n_node)
      save = TRUE
    }
  }
  
  # remove tips I don't use
  if(any(tre$tip.label %in% unq_languages)){
    keep = tre$tip.label[tre$tip.label %in% unq_languages]
    tre = drop.tip(tre, tip = setdiff(tre$tip.label, keep))
    save = TRUE
  }
  
  # if we have only one tip, save as an isolate a patch onto the supertree
  if(length(all_keeps) == 1){
    isolate = c(isolate, all_keeps)
  }
  
  
  if(save){
    if(length(tre$tip.label) == 1){
      tre$edge.length = years
    } else{
      tre = compute.brlen(tre, method = 'Grafen', power = 1) # Grafen is default, with power = 1
      tre = rescale(tre, "depth", years)  
    }
  }
  ape::write.tree(tre, paste0('processed_data/glottocode_trees/', f, '.nex'))  
}

## - Paste trees together -- ## 

tree_list = list.files('processed_data/glottocode_trees/', full.names = TRUE)

# build a tree from isolates
out = "("
## add families
for(t in tree_list){
  language_tree = readChar(t, file.info(t)$size)
  language_tree = str_replace(language_tree, ";\n", ":54000,")
  out = paste0(out, language_tree)
}

out = str_sub(out, end=-2)

out = paste0(out, ");")

fileConn = file("processed_data/super_tree.nwk")
  writeLines(out, fileConn)
close(fileConn)

tree = read.tree('processed_data/super_tree.nwk')

max(phytools::nodeHeights(tree))
plot(tree$edge.length)
A <- ape::vcv.phylo(tree)
diag(A)
any(is.na(A))
