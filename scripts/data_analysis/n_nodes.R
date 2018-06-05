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

outfile <- matrix(nrow=37, ncol = 0)
for(i in 1:length(netlist)){
  nam <- names(netlist)[i]
  vals <- t(sapply(netlist[[1]], dim))
  outvals <- rbind(paste(nam, c('n_MOTU', 'n_bats')), vals)
  outfile <- cbind(outfile, outvals)
}


outfile[1,] <- gsub('SAFE', 'Malaysia', outfile[1,])
outfile[1,] <- gsub('Beth_cr', 'Guanacaste normal, 2009', outfile[1,])
outfile[1,] <- gsub('Beth_CR', 'Guanacaste normal, 2009', outfile[1,])
outfile[1,] <- gsub('hernani_dryforestdry', 'Guanacaste dry, 2015', outfile[1,])
outfile[1,] <- gsub('hernani_dryforestwet', 'Guanacaste wet, 2015', outfile[1,])
outfile[1,] <- gsub('hernani_rainforestdry', 'La Selva wet, 2015', outfile[1,])


write.csv(outfile, 'summary_statistics/n_nodes.csv')
