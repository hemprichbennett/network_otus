##################################################
## Project: network_OTUs
## Script purpose: makes line plots for the data output by metcalcs
## Date: 25/05/18
## Author: Dave Hemprich-Bennett (hemprich.bennett@gmail.com)
## Notes
##################################################

library(plyr)
library(ggplot2)
library(lattice)
library(gridExtra)
library(LOTUS)

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
#df$network <- as.factor(df$network)

df$metric <- gsub('\\.', '_', df$metric)
df$metric <- gsub('HL', 'higher', df$metric)
df$metric <- gsub('LL', 'lower', df$metric)

#df$metric <- as.factor(df$metric)


df[which(df$metric == 'SA'),'metric'] <- 'Specialisation asymmetry'
df[which(df$metric == 'ISA'),'metric'] <- 'Interaction strength asymmetry'
df$metric <- firstup(df$metric)

cbPalette <- c("#bbcb4b",
               "#5253b8",
               "#9aa323",
               "#8479e6",
               "#8dbc4d",
               "#4c2b7b",
               "#42c87f",
               "#c14ca7",
               "#7ac163",
               "#8b4fa4",
               "#407f25",
               "#d485e0",
               "#858f29",
               "#5488e3",
               "#d19b2e",
               "#445ba1",
               "#dab45b",
               "#66a1e5",
               "#c46b27",
               "#43c8ac",
               "#c64538",
               "#71bc79",
               "#8c2b66",
               "#347435",
               "#dc79b5",
               "#aeb867",
               "#ab8cd9",
               "#9b7821",
               "#c24c6e",
               "#717026",
               "#cd535c",
               "#d1a261",
               "#852a1f",
               "#e17d5e",
               "#975a25")


df <- df[-grep('coefficient', df$metric),]#There is zero point in plotting the values for this metric, they never change!
#df$trial_clustering <- df$clustering*10
#pdf('Figures/7_networks_reliable_ranges.pdf')
line_plot(df, network = 'network', clustering = 'trial_clustering', metric = 'metric', value = 'value', plotname = '7 networks', colour = T, palette= cbPalette)
#dev.off()
pdf('Figures/2_networks_reliable_ranges.pdf')
line_plot(df[grep('Guanacaste', df$network),], network = 'network', clustering = 'trial_clustering', metric = 'metric', value = 'value', plotname = '2 networks')
dev.off()