library(bayestraitr)
library(ape)
library(stringr)

# functions ----
bt_addtag = function (tree, taxa) 
{
  anc = ape::getMRCA(tree, taxa)
  label = paste0("Node", anc)
  desc_idx = phytools::getDescendants(tree, node = anc)
  desc_taxa = tree$tip.label[desc_idx]
  desc_taxa = desc_taxa[!is.na(desc_taxa)]
  desc_taxa = paste(desc_taxa, collapse = " ")
  sprintf("AddTag %s %s", label, desc_taxa)
}

# reconstruct all ancestral nodes ----
tree = read.nexus('data/phylogeny//summary.trees')
d = read.table('processed_data/bothXpermitted.btdata', header = FALSE, sep = "\t")
rownames(d) = d[,1]
tree = keep.tip(phy = tree, tip = d$V1)

all_pairs = t(combn(tree$tip.label,2))

fileConn<-file("processed_data/bantu_tags.txt")
lines = vector(mode = "character", length = nrow(all_pairs))
tags = vector(mode = "character", length = nrow(all_pairs))
for(i in 1:nrow(all_pairs)){
  #label = paste(all_pairs[i,], collapse=".")
  # tag = bt_addtag(tree[[1]], taxa = all_pairs[i,])  
  tag = bt_addtag(tree, taxa = all_pairs[i,])  
  label = str_extract(tag, "Node[0-9]+")
  n_node = str_extract(label, "[0-9]+")
  node = paste0("addNode RecNode", n_node, " ", label)
  lines[i] = paste(tag, "\n", node, collapse = "")
  tags = rbind(tags, str_match(tag, "(Node[0-9]+)(.*)"))
}
node_names = stringr::str_extract(lines, "Node[0-9]+") 
lines = lines[!duplicated(node_names)]
writeLines(lines, fileConn)
close(fileConn)

# save tags for plot
write.csv(tags[!duplicated(tags[,2]),2:3], file = "processed_data/tags.csv")
