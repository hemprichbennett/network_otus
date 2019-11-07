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
  #require(methods)
  library(permute, lib.loc = '/data/home/btw863/r_packages/')
  library(lattice, lib.loc = '/data/home/btw863/r_packages/')
  library(crayon, lib.loc = '/data/home/btw863/r_packages/')
  library(backports, lib.loc = '/data/home/btw863/r_packages/')
  library(vctrs, lib.loc = '/data/home/btw863/r_packages/')
  library(vegan, lib.loc = '/data/home/btw863/r_packages/')
  library(statnet.common, lib.loc = '/data/home/btw863/r_packages/')
  library(network, lib.loc = '/data/home/btw863/r_packages/')
  library(sna, lib.loc = '/data/home/btw863/r_packages/')
  library(bipartite, lib.loc = '/data/home/btw863/r_packages/')
  library(stringr, lib.loc = '/data/home/btw863/r_packages/')
  #library(reshape2, lib.loc = '/data/home/btw863/r_packages/')
  library(LOTUS, lib.loc = '/data/home/btw863/r_packages/')
}else{
  library(here)
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




#rm(list= ls())

args = commandArgs(trailingOnly=TRUE)

cat(args, '\n')

iteration <- as.numeric(args)

#iteration <- 19
metric_names <- c(names(networklevel(matrix(nrow = 3, ncol = 3, seq(1,9)))), 'modularity')
print(metric_names)
metric <- metric_names[iteration]
#If a metric contains neither HL or LL then both greps will return a value of 0. Otherwise, one of them will return TRUE (a non-zero value)

#Lots of the specified metric names won't work as inputs, thanks to quirks in bipartite. This sorts them
if(length(grep('HL', metric)) + length(grep('LL', metric))==0){
  participant_metric <- FALSE
  metric <- metric
}else{
  part <- str_split(metric, pattern = '\\.')[[1]][length(str_split(metric, pattern = '\\.')[[1]])]
  if(length(grep('number.of.species', metric))>0){
    metric <- 'number of species'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else if(length(grep('mean.number.of.shared.partners', metric))>0){
    metric <- 'mean number of shared partners'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else if(length(grep('cluster.coefficient', metric))>0){
    metric <- 'cluster coefficient'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else if(length(grep('niche.overlap', metric))>0){
    metric <- 'niche overlap'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else if(length(grep('C.score', metric))>0){
    metric <- 'C score'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else if(length(grep('V.ratio', metric))>0){
    metric <- 'V ratio'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else if(length(grep('extinction.slope', metric))>0){
    metric <- 'extinction slope'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else if(length(grep('functional.complementarity', metric))>0){
    metric <- 'functional complementarity'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else if(length(grep('partner.diversity', metric))>0){
    metric <- 'partner diversity'
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }else{
    metric <- paste(str_split(metric, pattern = '\\.')[[1]][1:length(str_split(metric, pattern = '\\.')[[1]])-1])
    participant_metric <- TRUE
    if(part=='HL'){part <- 'higher'}
    if(part == 'LL'){part <- 'lower'}
  }
  
}

print(metric)
if(metric== 'interaction strength asymmetry'){metric <- 'ISA'}
if(metric== 'specialisation asymmetry'){metric <- 'SA'}



if(participant_metric == TRUE){
  
  out_metric <- metcalcs(networks = netlist, indices = metric, network_level = part, list_format = 'net_clust')
  outfilename <- paste('finenetworklevel_values/',gsub(' ', '_', metric), '_', part, '.csv', sep = '')
  cat('outfile name is ', outfilename, '\n', sep = '')
  write.csv(out_metric, outfilename)
}else{
  out_metric <- metcalcs(networks = netlist, indices = metric, list_format = 'net_clust')
  
  outfilename <- paste('finenetworklevel_values/',gsub(' ', '_', metric), '.csv', sep = '')
  cat('outfile name is ', outfilename, '\n', sep = '')
  write.csv(out_metric, outfilename)
  
}

print('finished')