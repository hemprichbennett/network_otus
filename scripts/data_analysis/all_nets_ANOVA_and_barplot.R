##################################################
## Project: Network OTUs
## Script purpose: This script runs an ANOVA on all the LOTUS outputs, then makes a barplot of the resulting data
## Date:
## Author: Dave Hemprich-Bennett (hemprich.bennett@gmail.com)
## Notes
##################################################
library(plyr)
library(ggplot2)
library(here)
library(gridExtra)
setwd(here())
getwd()


inpath <- 'finenetworklevel_values/'

#####Input and organise the data #####
files <- list.files(pattern = '.csv', path = inpath)
files <- paste(inpath, files, sep = '')
#files <- files[-grep('cluster coefficient', files)] #Cluster coefficient is a pain in the arse as it has been returning 3 values, not 1

file_list <- lapply(files, read.csv, stringsAsFactors = FALSE)


df <- ldply(file_list, data.frame)
df <- df[,-1] #The first column ends up being the rowname of every item in the list, which is meaningless
df$clustering <- df$clustering/10 #its currently an integer and we want it as a percentage

metric_types <- read.csv('metric_types.csv')

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
} #A function to capitalise the metric names when making plots


df[which(df$network=='SAFE'), 'network'] <- 'Malaysia'
df[which(df$network=='Beth_CR'), 'network'] <- 'Guanacaste normal, 2009'
df[which(df$network=='hernani_dryforestdry'), 'network'] <- 'Guanacaste dry, 2015'
df[which(df$network=='hernani_dryforestwet'), 'network'] <- 'Guanacaste wet, 2015'
df[which(df$network=='hernani_rainforestdry'), 'network'] <- 'La Selva wet, 2015'
df$network <- as.factor(df$network)

df$metric <- gsub('\\.', '_', df$metric)
df$metric <- gsub('HL', 'higher', df$metric)
df$metric <- gsub('LL', 'lower', df$metric)

df$metric <- as.factor(df$metric)





####and now for analysis of the fine dataset

effects_and_probabilities <- matrix(nrow= 0, ncol = 4)

values <- c()
fs <- c()
ps <- c()
met <- c()
for(i in 1:length(unique(df$metric))){
  #i <- 2
  metric <- unique(df$metric)[i]
  print(metric)
  temp_df <- df[which(df$metric == metric),]
  
  temp_df$value <- log10(abs(temp_df$value))
  #temp_df$value <- asin(temp_df$value) #Arcsine transform the values for connectance
  if(temp_df[1,'metric']=='compartment diversity'){
    next()
  }
  if(is.na(temp_df[1,'value'])){
    stop()
  }
  model = aov(value ~ clustering + network + clustering:network, data = temp_df)
  
  a <- summary(model)
  values <- c(values, rownames(a[[1]]))
  fs <- c(fs,a[[1]][,4])
  ps <- c(ps, a[[1]][,5])
  met <- c(met, rep(as.character(metric), nrow(a[[1]])))
  
  #effects_and_probabilities <- rbind(effects_and_probabilities, out)
  
}

effects_and_probabilities <- data.frame(met,values,  fs, ps)

#We now have a data frame of the effects of clustering, dataset, and clustering:dataset on each of our metrics
#We now want to assess which ones have the greatest effect size of clustering: dataset, compared to clustering

ms <- c()
pure_f <- c()
modded <- c()
modded_p <- c()
just_set <- c()
divd <- c()
for(i in 1:length(unique(effects_and_probabilities$met))){
  m <- as.character(unique(effects_and_probabilities$met)[i])
  ms <- c(ms, m)
  sub_df <- effects_and_probabilities[which(effects_and_probabilities$met == m),]
  pure_f <- c(pure_f, sub_df[3,3])
  moderated <- sub_df[3,3]
  modded <- c(modded, moderated)
  modded_p <- c(modded_p, sub_df[3,4])
  
  just_dataset <- sub_df[2,3] #changed to [2,3] to check effect 
  just_set <- c(just_set, just_dataset)
  
  divided <- just_dataset/moderated
  divd <- c(divd, divided)
}

rankings <- data.frame(ms, pure_f, just_set, modded, divd, modded_p)
write.csv(rankings, 'summary_statistics/metric_rankings.csv')

####Reinstate rankings_minus later ####
#rankings_minus <- rankings[-c(4,18),]#Get rid of compartments and number of species (HL), as they have huge values, being totally unaffected by clustering threshold
rankings_minus <- rankings

rankings_minus$ms <- as.character(rankings_minus$ms)


rankings_minus$colour <- as.character(rep('green', nrow(rankings_minus)))




for(i in 1:nrow(rankings_minus)){
  m = gsub(' ', '_',rankings_minus[i,1])
  #print(m)
  if(m %in% metric_types[,1]){
    rankings_minus$colour[i] <- "qualitative" #qualitative networks
    #print('qual')
  }
  else if(m %in% metric_types[,2]){
    rankings_minus$colour[i] <- "quantitative" #quantitative networks
    #print('quant')
  }
  else(print(m))
}

rankings_minus$colour <- as.factor(rankings_minus$colour)


for(i in 1:nrow(rankings_minus)){
  rankings_minus[i,1] <- firstup(as.character(rankings_minus[i,1]))
  rankings_minus[i,1] <- gsub('_', ', ', rankings_minus[i,1])
}


rankings_minus <- rankings_minus[order(rankings_minus$divd),]

rankings_minus$for_signif <- rep(NA, nrow(rankings_minus))


for(i in 1:nrow(rankings_minus)){
  if(rankings_minus[i,'modded_p'] <= 0.05){rankings_minus[i,'for_signif'] <- rankings_minus[i, 'pure_f']}
}


pdf('Figures/overall_rankings.pdf')

ggplot(data=rankings_minus, aes(x = reorder(ms, -pure_f), y = pure_f, fill = rankings_minus$colour)) +
  geom_bar(stat = 'identity')+
  labs(x = 'Metric', y = 'F-value')+
  scale_fill_grey()+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(fill = 'Metric type')+
  geom_vline(xintercept = 0.5+nrow(rankings_minus) -length(which(is.na(rankings_minus$for_signif)))) #Sort this, its bisecting a bar






dev.off()
