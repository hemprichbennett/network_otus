library(here)
library(bipartite)

setwd(here())
nets <- c('SAFE', 'Beth_CR', 'hernani_dryforestdry', 'hernani_dryforestwet', 'hernani_rainforestdry', 'Texas',  'Jamaica')
#nets <- c('SAFE', 'Beth_CR', 'hernani_dryforestdry', 'hernani_dryforestwet', 'hernani_rainforestdry', 'Texas') #Without Jamaica as its still being weird
metric_names <- c(names(networklevel(matrix(nrow = 3, ncol = 3, seq(1,9)))), 'modularity')

metric_names <- gsub('interaction strength asymmetry', 'ISA', metric_names)
metric_names <- gsub('specialisation asymmetry', 'SA', metric_names)

specs <- matrix(nrow = 0, ncol = 3)

for(i in 1:length(nets)){
  net <- nets[i]
  for(m in 1:length(metric_names)){
    met <- metric_names[m]
    for(c in seq(910,980,2)){
      clust <- c
      specs <- rbind(specs, c(net, met, clust))
    }
  }
}

write.table(specs, 'fine_networklevel_parameters.csv', row.names = FALSE, col.names = FALSE, sep = ',')
