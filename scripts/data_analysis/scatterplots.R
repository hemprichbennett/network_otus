##################################################
## Project: Network OTUs
## Script purpose: This script makes scatterplots of all of the metrics output from LOTUS
## Date: 25/05/18
## Author: Dave Hemprich-Bennett (hemprich.bennett@gmail.com)
## Notes
##################################################
library(plyr)
library(ggplot2)
library(here)
library(gridExtra)
setwd(here())
getwd()


#Making colourblind-friendly palettes
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

inpath <- 'finenetworklevel_values/'

#####Input and organise the data #####
files <- list.files(pattern = '.csv', path = inpath)
files <- paste(inpath, files, sep = '')
#files <- files[-grep('cluster coefficient', files)] #Cluster coefficient is a pain in the arse as it has been returning 3 values, not 1

file_list <- lapply(files, read.csv, stringsAsFactors = FALSE)


df <- ldply(file_list, data.frame)
df <- df[,-1] #The first column ends up being the rowname of every item in the list, which is meaningless
df$clustering <- df$clustering/10 #its currently an integer and we want it as a percentage



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

df$metric <- as.factor(df$metric)
df$metric <- gsub('.LL', '_lower', df$metric)
df$metric <- gsub('.HL', '_higher', df$metric)
df$metric <- gsub('\\.', ' ', df$metric)



##### Plotting all networks #####

for(i in 1: length(unique(df$metric))){
  metric <- as.character(unique(df$metric)[i])
  pdf(paste('Figures/all_networks_individual_scatterplots/',metric,'.pdf', sep = ''))
  temp_df <- df[which(df$metric == metric),]
  
  
  p <- ggplot(temp_df, aes(x = clustering, y = value, color = network)) +
    geom_point()+
    labs(x = 'clustering', y = firstup(as.character(metric))) +
    geom_smooth(method = lm, se = T)+
    scale_x_continuous(breaks = seq(91, 98, 1))+
    geom_vline(xintercept = 93)+
    geom_vline(xintercept = 97)
  scale_fill_manual(values=cbPalette)
  scale_colour_manual(values=cbPalette)
  print(p)
  dev.off()
}


for_multiplot <- c('connectance', 'extinction slope_lower', 'togetherness_lower', 'nestedness',  'niche overlap_higher')
for_multiplot <- for_multiplot[order(for_multiplot)]#Make them in alphabetical order


lst <- list()

g_legend<-function(a.gplot){ 
  tmp <- ggplot_gtable(ggplot_build(a.gplot)) 
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box") 
  legend <- tmp$grobs[[leg]] 
  return(legend)} 





for(i in 1:length(for_multiplot)){
  for_plot <- df[which(df$metric == for_multiplot[i]),]
  caption_metric <- firstup(gsub('_', ' ',for_multiplot[i]))
  caption_metric <- firstup(gsub('\\.', '  ',caption_metric))
  caption_metric <- gsub(' lower', ', \nlower level', caption_metric)
  caption_metric <- gsub(' higher', ', \nhigherlevel', caption_metric)
  lst[[i]] <-  ggplot(for_plot, aes(x = clustering, y = value, color = network)) +
    geom_point()+
    labs(x = 'Clustering', y = caption_metric) +
    geom_smooth(method = lm, se = T)+
    scale_x_continuous(breaks = seq(91, 98, 1))+
    geom_vline(xintercept = 93)+
    geom_vline(xintercept = 97)+
    scale_fill_manual(values=cbPalette)+
    scale_colour_manual(values=cbPalette)+
    theme_bw()+
    theme(legend.position="none")+ #No legend
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
}

legend <- g_legend(ggplot(for_plot, aes(x = clustering, y = value, color = network)) +
                     geom_point()+
                     labs(x = 'clustering', y = firstup(gsub('_', ', ',for_multiplot[i]))) +
                     geom_smooth(method = lm, se = T)+
                     scale_x_continuous(breaks = seq(91, 98, 1))+
                     geom_vline(xintercept = 93)+
                     geom_vline(xintercept = 97)+
                     scale_fill_manual(values=cbPalette)+
                     theme_bw()+
                     scale_colour_manual(values=cbPalette)) #Make the legend for it

pdf('Figures/grid_plot.pdf')
grid.arrange(lst[[1]], lst[[2]],
             lst[[3]], lst[[4]], 
             lst[[5]], legend,
             nrow = 3,
             top = '7 networks')
dev.off()


#####Hernani's Guanacaste data ####
hernani_subset <- df[which(df$network == 'Guanacaste dry, 2015' | df$network == 'Guanacaste wet, 2015'),]



for(i in 1: length(unique(hernani_subset$metric))){
  metric <- unique(hernani_subset$metric)[i]
  pdf(paste('Figures/guanacaste_individual_scatterplots/',metric,'.pdf', sep = ''))
  temp_hernani <- hernani_subset[which(hernani_subset$metric == metric),]
  
  
  a <- ggplot(temp_hernani, aes(x = clustering, y = value, color = network)) +
    geom_point()+
    labs(x = 'Clustering', y = firstup(as.character(gsub('_', ' ',metric)))) +
    geom_smooth(method = lm, se = T)+
    scale_x_continuous(breaks = seq(91, 98, 1))+
    theme_bw()
  
  print(a)
  dev.off()
}

hernani_for_multiplot <- c('connectance', 'nestedness','niche overlap_higher', 'robustness_lower', 'Shannon diversity')



hernani_lst <- list()


for(i in 1:length(for_multiplot)){
  hernani_caption_metric <- firstup(gsub('_', ' ',for_multiplot[i]))
  hernani_caption_metric <- firstup(gsub('\\.', '  ',hernani_caption_metric))
  hernani_caption_metric <- gsub(' lower', ', \nlower level', hernani_caption_metric)
  hernani_caption_metric <- gsub(' higher', ', \nhigherlevel', hernani_caption_metric)
  for_plot <- hernani_subset[which(hernani_subset$metric == for_multiplot[i]),]
  hernani_lst[[i]] <-  ggplot(for_plot, aes(x = clustering, y = value, color = network)) +
    geom_point()+
    labs(x = 'Clustering', y = hernani_caption_metric) +
    geom_smooth(method = lm, se = T)+
    scale_x_continuous(breaks = seq(91, 98, 1))+
    geom_vline(xintercept = 93)+
    geom_vline(xintercept = 97)+
    scale_colour_grey()+
    theme_bw()+
    #scale_fill_manual(values=cbPalette)+
    #scale_colour_manual(values=cbPalette)+
    theme(legend.position="none")+ #These plots should not have a legend
    theme(plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm")) #The ones inside this if statement need big margins
  
  
}

hernani_legend <- g_legend(ggplot(for_plot, aes(x = clustering, y = value, color = network)) +
                             geom_point()+
                             #labs(x = 'Clustering', y = firstup(gsub('_', ', ',hernani_caption_metric))) +
                             geom_smooth(method = lm, se = T)+
                             scale_x_continuous(breaks = seq(91, 98, 1))+
                             geom_vline(xintercept = 93)+
                             geom_vline(xintercept = 97)+
                             scale_colour_grey())

pdf('Figures/Guanacaste_grid_plot.pdf')
grid.arrange(hernani_lst[[1]], hernani_lst[[2]], 
             hernani_lst[[3]], hernani_lst[[4]],
             hernani_lst[[5]], hernani_legend, 
             nrow = 3,
             top = '2 networks')
dev.off()
