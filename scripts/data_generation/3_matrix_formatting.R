##################################################
## Project: Network OTUs analysis
## Script purpose: Takes the files from our previous BASH and mothur steps and makes them into nicely formatted adjacency matrixes
## Date: 24/05/18
## Author: Dave Hemprich-Bennett (hemprich.bennett@gmail.com)
## Notes This is a modification of the old script 'fine_network_netlevel.R' which I used in a much less reproducible
##        workflow
##################################################

library(here)
library(stringr)
#library(davenetworks)

setwd(here())


# iteration <- as.numeric(args)
# #iteration <- 1
#param_file <- read.csv('net_dirs.csv', header = F, stringsAsFactors = FALSE)

net_files <- list.files(pattern = "binary", recursive = T) #make the list

#Make a for loop here
for(iteration in 1:length(net_files)){
  
  net <- read.table(net_files[[iteration]], header = T, sep = '\t', stringsAsFactors = F)
  
  net_name <- dirname(net_files[iteration])
  net_name <- gsub('sequencefiles/', '', net_name)
  
  cat('net_name is ', net_name, '\n', sep = '')
  
  #setwd(net_dir)
  
  if(grepl('hernani', net_name)){
    hernani_net <- TRUE
  }else{
    hernani_net <- FALSE
  }
  
  
  
  
  cat('dimensions of net are ', dim(net)[1],' ', dim(net)[2], '\n', sep = '')
  
  if(grepl('SAFE', net_name)){ #If one of the datasets is SAFE, we'll need the metadata
    #source('/data/home/btw863/functions/the.matrix.reloader.files/The.matrix.reloader.R')
    source('~/Dropbox/Education/PhD/Scripts/Clare lab scripts/the.matrix.reloader/The.matrix.reloader.R')
    #bat_data <- read.csv('../Bats_datasheet_formatted.csv', header = TRUE)
    bat_data <- read.csv('Bats_datasheet_formatted.csv', header = TRUE)
    badcols <- c("1076", "1130", "1217", "1687", "222","","376", "423", "430",  "549",  "712",  "714", "787", "950")
    
    
    cat(is.matrix(net), '\n')
    net <- rbind(colnames(net), net)
    net[1,] <- gsub( 'poo', '', net[1,])
    
    net <- the.matrix.reloader(master.data = bat_data, ID.column.1 = "Faeces_no1", ID.column.2 = "Faeces_no2", species.column = "Species", OTU.matrix = net)
    to_delete <- which(colnames(net) %in% badcols) #some of the columns sadly contain unmatched samples. This removes them, as we don't know what species they are
    net <- net[,-to_delete]
  } 
  if(grepl('Texas', net_name)){
    to_kill <- grep('Ef', colnames(net))
    net <- net[,-to_kill] #These belong to eumops floridensis, which was from a different site
    colnames(net) <- gsub('AP', 'Ap', colnames(net))
    colnames(net) <- gsub('M_volaans', 'Mv', colnames(net))
    colnames(net) <- gsub('Mvol', 'Mv', colnames(net))
    colnames(net) <- gsub('MyYu', 'My', colnames(net))
    colnames(net) <- gsub('NM', 'Nyma', colnames(net))
    colnames(net) <- gsub('NyMa', 'Nyma', colnames(net))
    colnames(net) <- gsub('Myca', 'Mc', colnames(net))
    colnames(net) <- gsub('TaBr', 'Tb', colnames(net))
  }
  if(!grepl('SAFE', net_name)){ #!! 
    
    rownames(net) <- net[,1]
    net <- net[,-1]
    
    colnames(net) <- gsub('\\.','_', colnames(net) ) #Some of the colnames go genus.species.ID, some go genus_species_ID     Make them uniform
    print(colnames(net))
    new_mat <- matrix(nrow=nrow(net), ncol = 0)
    a <- 1
    for(i in 1:ncol(net)){
      if(length(str_split(colnames(net), pattern = '_')[[i]])>1){
        newname <-str_split(colnames(net), pattern = '_')[[i]][2]
      }else{
        newname <- gsub('[0-9].*$', '', colnames(net)[i]) #Because a few of the texas samples didn't have underscores in the name, we have to do this
      }
      
      
      print(newname)
      if(hernani_net == TRUE){
        newname <- gsub('[0-9]', '', newname)
      }
      if(newname %in% colnames(new_mat)){
        #cat(newname, ' is already in', sep = '')
        merge_col <- which(colnames(new_mat) == newname)
        new_mat[,merge_col] <- new_mat[,merge_col]+as.numeric(net[,i])
      }
      else{
        #cat(newname, ' is new', sep = '')
        new_mat <- cbind(new_mat,as.numeric(net[,i]))
        colnames(new_mat)[a] <- newname
        a <- a + 1
      }
    }
    
    net <- new_mat
  }
  if(grepl('Texas', net_name)){
    break()}
  outdir <- paste('adjacency_matrixes/',net_name, sep = '')
  dir.create(file.path(outdir), showWarnings = T)
  write.csv(net, paste(outdir, 'matrix.csv', sep = '/'))
  
}
