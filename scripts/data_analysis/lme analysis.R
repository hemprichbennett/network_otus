##################################################
## Project: Network OTUs
## Script purpose: This script runs an ANOVA on the ACG 2015 LOTUS outputs, then makes a barplot of the resulting data
## Date:
## Author: Dave Hemprich-Bennett (hemprich.bennett@gmail.com)
## Notes
##################################################
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(here)
library(gridExtra)
library(lme4)
library(forcats)
setwd(here())
getwd()


inpath <- "finenetworklevel_values/"

##### Input and organise the data #####
files <- list.files(pattern = ".csv", path = inpath)
files <- paste(inpath, files, sep = "")


file_list <- lapply(files, read.csv, stringsAsFactors = FALSE)


df <- ldply(file_list, data.frame)
df <- df[, -1] # The first column ends up being the rowname of every item in the list, which is meaningless
df$clustering <- df$clustering / 10 # its currently an integer and we want it as a percentage

metric_types <- read.csv("metric_types.csv", stringsAsFactors = F)

metric_types <- metric_types %>%
  gather(qualitative, quantitative, key = "metric_type", value = "metric") %>%
  filter(metric != "") %>%
  mutate(metric = gsub("_", " ", metric))

# Function for capitalising metric names ----------------------------------


firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}


# Rename metric ----------------------------------------------------------


df[which(df$network == "hernani_dryforestdry"), "network"] <- "Guanacaste dry, 2015"
df[which(df$network == "hernani_dryforestwet"), "network"] <- "Guanacaste wet, 2015"
# df$network <- as.factor(df$network)

df$metric <- gsub("\\.", "_", df$metric)
df$metric <- gsub("HL", "higher", df$metric)
df$metric <- gsub("LL", "lower", df$metric)
df$metric <- gsub("_", " ", df$metric)


df <- df %>%
  # log10 transform the values
  mutate(logvalue = log10(abs(value))) %>%
  mutate(root = sqrt(abs(value))) %>%
  # mutate(cent = scale(value))
  # mutate(root_centered = sqrt())
  na.omit()

# for (i in 1:length(unique(df$metric))) {
#   met <- unique(df$metric)[i]
#   massive_plot <- df %>%
#     filter(metric == met) %>%
#     gather(`value`, logvalue, root, cent, key = "data_type", value = "n") %>%
#     ggplot(aes(x = n)) +
#     geom_histogram() +
#     facet_wrap(. ~ data_type, scales = "free") +
#     theme_bw()
#
#   ggsave(paste0("Figures/hists/", met, ".pdf"), massive_plot)
# }




# lme function ------------------------------------------------------------



# Use function ------------------------------------------------------------

df <- df %>%
  filter(metric != "compartment diversity"
  )

withWarnings <- function(expr) {
  myWarnings <- NULL
  wHandler <- function(w) {
    myWarnings <<- c(myWarnings, list(w))
    invokeRestart("muffleWarning")
  }
  val <- withCallingHandlers(expr, warning = wHandler)
  list(value = val, warnings = myWarnings)
}

# lme4_mod_function <- function(met) {
#   lme_mod <- lme4::lmer(root ~ clustering +
#                           #(1| network) +
#                           ( 1+clustering  | network) ,
#                         data = filter(df, metric == met),
#                         REML = F
#   )
#
#   return(lme_mod)
# } # Works but I'm dubious abouts its reliability due to weird coefficients



# Make model --------------------------------------------------------------

lme4_mod_function <- function(met) {
  lme_mod <- lme4::lmer(logvalue ~ clustering + network + clustering:network +
    (1 | network),
  data = filter(df, metric == met),
  REML = F
  )

  return(lme_mod)
}


# Use model ---------------------------------------------------------------


# In a loop, run the mixed effects models
mod_list <- list()
badmets <- c()
metric <- c()
altered_f_values <- c()
z <- 1 # an iterator, as we may not pass all values to the mod_lsit
for (i in 1:length(unique(df$metric))) {
  desired_metric <- unique(df$metric)[i]
  print(desired_metric)
  # Do the calculations, catching warnings if they occur
  out <- withWarnings(lme4_mod_function(desired_metric))
  if (!is.null(out$warnings)) {
    # store the names of any metric that give warnings: these shouldn't be used
    badmets <- c(badmets, desired_metric)
  } else {
    mod_list[[z]] <- out$value
    names(mod_list)[z] <- desired_metric
    z <- z + 1

    # write the model summary to a file
    sink(paste0("results/mixed_effects_summaries/", desired_metric, ".txt"))
    summary(out$value)
    sink()

    # Isolate the F-values
    anova_table <- anova(out$value)
    network_f <- anova_table$`F value`[2]
    interaction_f <- anova_table$`F value`[3]
    # standardise it for between-network comparisons
    newmet <- network_f / interaction_f
    metric <- c(metric, desired_metric)
    altered_f_values <- c(altered_f_values, newmet)
  }
}


# Tidy the model output df ------------------------------------------------

rankings_df <- data.frame(metric, altered_f_values) %>%
  left_join(metric_types) %>%
  mutate(
    metric_type = firstup(metric_type),
    metric = firstup(as.character(metric)),
    metric = gsub(" lower", ", lower", metric),
    metric = gsub(" higher", ", higher", metric),
    metric = gsub("H2", "H2'", metric),
    metric = fct_reorder(metric, altered_f_values),
  )


# Plot the model output ---------------------------------------------------

rankings_plot <- ggplot(rankings_df, aes(x = metric, y = altered_f_values, fill = metric_type)) + geom_bar(stat = "identity") +
  scale_fill_grey() +
  theme_bw() +
  labs(
    fill = "Metric type", x = "Metric",
    y = "F-value of Network/\nF-value of interaction"
  ) +
  theme(legend.position = 'bottom')+
  coord_flip() # Flip it so that metric names are easier to read

rankings_plot
ggsave('Figures/lme_barplot.pdf')
ggsave('Figures/lme_barplot.jpg', width = 6, height = 7.5)
