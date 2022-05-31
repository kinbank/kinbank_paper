suppressPackageStartupMessages({
  library(bayestraitr)
  library(ape)
  library(stringr)
  library(phytools)
  library(tidyr)
})

# Read in results from the dual model
# Translate NA
# and identify reconstructed nodes
log = bt_read.log('results/bothXpermitted-dep-1.Log.txt')
log[log == "--"] = NA
idx = str_detect(colnames(log), "RecNode")
rec_nodes = log[,idx]
rec_nodes = as.matrix(sapply(rec_nodes, as.numeric))

# Read in consensus tree 
con_tree = read.nexus('https://raw.githubusercontent.com/D-PLACE/dplace-data/master/phylogenies/grollemund_et_al2015/summary.trees')
con_tree = ladderize(con_tree)

# Read in model data
d = read.table('data/bt_data/bothXpermitted.btdata', header = FALSE, sep = "\t")
rownames(d) = d[,1]
con_tree = keep.tip(phy = con_tree, tip = d$V1)

## create node probabilities
values = apply(rec_nodes, 2, mean, na.rm = T)

# pairs nodes with nodes on tree
children = read.csv('data/tags.csv', skip = 1)
children_sets = children$X.1 %>% 
  str_trim() %>% 
  str_split(" ")
new_nodes = lapply(children_sets, function(x){ape::getMRCA(con_tree, tip = x)}) %>% unlist()

paired_nodes = data.frame(old_nodes = paste0("Rec", children$X, sep = ""),
                          new_nodes = new_nodes)
result_nodes = data.frame(tree_nodes = str_match(names(values),
                                                 "RecNode[0-9]+")[,1])
nodes = dplyr::left_join(result_nodes, paired_nodes, 
                         by = c("tree_nodes" = "old_nodes"))[,2]

option = str_match(names(values), "P\\([0-1],[0-1]\\)")

node_prob = data.frame(node = nodes, option = option, value = values)
node_prob_wide = spread(node_prob, key = "option", value = "value")
rownames(node_prob_wide) = node_prob_wide$node

node_prob_matrix = as.matrix(node_prob_wide[,2:5])

# 273253 : blue       : 00      : no Cross & no CCM no CCM & no Cross
# ED5C4D : red        : 01      : No Cross & CCM 
# FBBE4B : Yellow     : 10      : Crossed & no CCM
# 57B5ED : light blue : 11      : CCM & Cross

nice_cols = c("#273253", "#ED5C4D", "#FBBE4B", "#57B5ED", "grey")

# Improve tip labels
new_labels = str_remove(con_tree$tip.label, "[A-Z][0-9]+[A-Za-z]?_") %>% 
  str_remove("^J") %>% 
  str_replace("_", " ") %>% 
  str_remove(" 2013")

# Apply colours
d = d[con_tree$tip.label,]
tip_points = paste0(d$V2, d$V3)
tip_colours = ifelse(tip_points == "00", "#273253",
                     ifelse(tip_points == "01", "#ED5C4D",
                            ifelse(tip_points == "10", "#FBBE4B",
                                   ifelse(tip_points == "11", "#57B5ED", 
                                          ifelse(str_detect(tip_points, "-"), "grey", NA)))))
con_tree$tip.label = new_labels

pdf(file = "ccm_plot.pdf",width = 6, height = 7)
plotTree(con_tree, setEnv = TRUE, offset = 0.5, fsize = 0.6, cex = 2)
nodelabels(node = as.numeric(rownames(node_prob_wide)), pie = node_prob_matrix, 
           piecol = nice_cols, cex = 0.5)
tiplabels(pch = 21, bg = tip_colours, cex = 0.8)
legend(x = 0, y = 10, 
       legend = c("No CM & No BM", "No CM & BM", "CM & No BM", 
                  "CM & BM", "No Social data"), 
       pch = 19, 
       col = nice_cols, 
       bty = "n")
dev.off()

png(file = "ccm_plot.png", units="in", width=5, height=5, res=300)
plotTree(con_tree, 
         setEnv = TRUE, 
         offset = 0.5, 
         fsize = 0.6, 
         cex = 2)
nodelabels(node = as.numeric(rownames(node_prob_wide)), 
           pie = node_prob_matrix, 
           piecol = nice_cols, 
           cex = 0.5)
tiplabels(pch = 21, 
          bg = tip_colours, 
          cex = 0.8)
legend(x = 0, y = 15, 
       legend = c("No CM & No BM", "No CM & BM", "CM & No BM", 
                  "CM & BM", "No Social data"), 
       pch = 19, 
       col = nice_cols, 
       bty = "n")
dev.off()
