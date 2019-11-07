##################################################
## Project: Network OTUs
## Script purpose: generating the network level values for each metric
## Date: 24/05/18
## Author: Dave Hemprich-Bennett (hemprich.bennett@gmail.com)
## Notes
##################################################
dir <- getwd()
basedir <- strsplit(dir, split ='/')[[1]][2]
print(basedir)
if(grepl('data', basedir)){
  library(here, lib.loc = '/data/home/btw863/r_packages/')
  require(methods)
  library(permute, lib.loc = '/data/home/btw863/r_packages/')
  library(lattice, lib.loc = '/data/home/btw863/r_packages/')
  library(vegan, lib.loc = '/data/home/btw863/r_packages/')
  library(statnet.common, lib.loc = '/data/home/btw863/r_packages/')
  library(network, lib.loc = '/data/home/btw863/r_packages/')
  library(sna, lib.loc = '/data/home/btw863/r_packages/')
  library(bipartite, lib.loc = '/data/home/btw863/r_packages/')
  library(stringr, lib.loc = '/data/home/btw863/r_packages/')
  library(reshape2, lib.loc = '/data/home/btw863/r_packages/')
  library(LOTUS, lib.loc = '/data/home/btw863/r_packages/')
}else{
  library('here')
  library(vegan)
  library(bipartite)
  library(stringr)
  library(reshape2)
  library(LOTUS)
}

setwd(here())

getwd()

filenames <- list.files(pattern = 'matrix.csv', recursive = T)

netnames <- list.dirs(path = 'adjacency_matrixes/', recursive = F)
netnames <- gsub('adjacency_matrixes//', '', netnames)
netlist <- list()
for(i in 1:length(netnames)){
  net <- netnames[i]
  net_filenames <- filenames[grep(net, filenames)]
  tem <- lapply(net_filenames, read.csv, header = T, stringsAsFactors = F, row.names=1)
  names(tem) <- gsub('^.*/', '', dirname(net_filenames))
  names(tem) <- gsub('OTU_', '', names(tem))
  netlist[[i]] <- tem
  names(netlist)[i] <- net
}


combinations <- expand.grid(names(netlist), names(netlist$Beth_cr))



#args = commandArgs(trailingOnly=TRUE)

#cat(args, '\n')

#iteration <- as.numeric(args)
mod_function <- function(iteration){
  iteration
  print(iteration)
  #metric_names <- c(names(networklevel(matrix(nrow = 3, ncol = 3, seq(1,9)))), 'modularity')
  #metric <- metric_names[iteration]
  
  #combinations[19,]
  start_time <- Sys.time()
  net <- netlist[[combinations[iteration ,1]]][[combinations[iteration,2]]]
  
  outfilename <- paste('finenetworklevel_values/','modularity_',combinations[iteration ,1],'_', combinations[iteration ,2], '.csv', sep = '')
  cat('outfile name is ', outfilename, '\n', sep = '')
  
  in_list <- list(list(net))
  names(in_list) <- combinations[iteration ,1]
  names(in_list[[1]]) <- combinations[iteration,2]
  print('running metcalcs')
  out_metric <- metcalcs(networks = in_list, indices = 'modularity', list_format = 'net_clust')
  print('metcalcs run successfully')
  
  write.csv(out_metric, outfilename)
  
  end_time <- Sys.time()
  print('finished')
  print(end_time - start_time)

}

sapply(seq(1,252), function(x) mod_function(x))
