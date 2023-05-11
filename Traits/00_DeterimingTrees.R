###figuring out which families are trees####

setwd("C://Users//mavolio2//Dropbox//CoRRE_Database//Data//CompiledData//Species_lists//")

treefam<-read.csv("species_families_trees_2021.csv")

tree<-treefam %>% 
  group_by(family, tree.non.tree) %>% 
  summarize(n=length(tree.non.tree)) %>% 
  pivot_wider(names_from = tree.non.tree, values_from = n, values_fill = 0)
