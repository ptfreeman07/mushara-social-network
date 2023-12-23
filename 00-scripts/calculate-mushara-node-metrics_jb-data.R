## ---------------------------
##
## Script name: calculate-mushara-node-metrics.R
##
## Purpose of script: This script is designed to calculate node-level centrality metrics for each observation year in the Mushara database and based on the composite data table cleaned by Jodie Berezin. 
##
## Author: Patrick Freeman
##
## Date Created: 11/18/2023
## Date last updated: 12/2/2023
##
## Email contact: ptfreeman[at]alumni.stanford.ed
##
## ---------------------------
##
## Notes: 
##  


# Load libraries ----------------------------------------------------------
library(sna)
library(asnipe)
library(igraph)
library(tidyverse)
library(purrr)
library(ggnetwork)
library(intergraph)

# Important functions -----------------------------------------------------
### Function that returns the unique individuals in each event 
build_attendance_records <- function(df){
  
  df_split <- split(df, f=df$year.event)
  
  attendance_records_list <- list()
  for(i in 1:length(df_split)){
    individuals <- df_split[[i]] %>%
      select(ElephantName, Year) %>%
      distinct() %>%
      mutate(year.event=unique(df_split[[i]]$year.event))
    attendance_records_list[[i]] <- individuals
  }
  
  attendance_records <- bind_rows(attendance_records_list)
  return(attendance_records)
}

### Function to prep network data 
prep_network_data <- function(
    attendance_df = attendance_df,
    excluded_individuals = excluded_individuals,
    association_index = "SRI"
){
  networkprep <- attendance_df %>% 
    dplyr::select(ElephantName, year.event) %>% 
    dplyr::filter(!ElephantName %in% excluded_individuals) %>% 
    dplyr::rename(Event = year.event,
                  Individual = ElephantName) %>% 
    ungroup() %>% 
    as.data.frame()
  
  gbi <- get_group_by_individual(networkprep, data_format="individuals")
  
  ### Get network based on the simple ratio index of association 
  set.seed(12345)
  network <- get_network(gbi, data_format="GBI", association_index=association_index)
  g=graph_from_adjacency_matrix(network, "undirected", weighted=T) #create a graph object
  return(g)
}

### Function to calculate centrality metrics for each individual
calculate_centrality_metrics <- function(
    g = g # adjacency graph
) {
  
  degree=igraph::degree
  betweenness=igraph::betweenness
  closeness=igraph::closeness
  eigenvector=igraph::eigen_centrality
  
  names=V(g)$name
  de=degree(g)
  st=graph.strength(g)
  be=betweenness(g, normalized=T)
  eig=eigenvector(g)
  
  #assemble dataset
  d=data.frame(node.name=names, degree=de, strength=st, betweenness=be, eigenvector=eig$vector) 
  
  return(d)
}

# Load data ---------------------------------------------------------------
mushara_records <- read_csv("/Users/patrickfreeman-csp/Downloads/mushara_5years_clean_jan2023_bullnamesadded.csv")


# Split dataset by year ----------------------------------------------
mushara_years <- split(mushara_records, f=mushara_records$Year)


# Obtain attendance records for each year   -------------------------------------
attendance_records_out <- list()
for(i in 1:length(mushara_years)){
  year = mushara_years[[i]] %>%
    dplyr::mutate(year.event = paste(Year, Event, sep="_"))
  
  attendance_records <- build_attendance_records(year) %>%
    dplyr::filter(!ElephantName %in% c("femaleelephant", "5minscan", NA, "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "unknown6", "unknown7", "unknown8", "unknown9", "unknown10", "incomingelephan", "unknownsubject", "vehicle", "watertrough", "wind", "{end}", "?", "herd", "herd member", "startscan", "otherwildlife", "othermisc"))
  
  attendance_records_out[[i]] <- attendance_records
}

names(attendance_records_out) <- c("y2007", "y2008", "y2009", "y2010", "y2011")

# tabulate number of events observed for each individual  -------------------------------------
event_count_out <- list() 
for(j in 1:length(attendance_records_out)){
  
  attendance_records <- attendance_records_out[[j]]
  
  event_count <- attendance_records %>%
    group_by(ElephantName) %>% 
    dplyr::summarise(total_events = n()) %>%
    dplyr::mutate(observation.year = unique(attendance_records$Year))
  
  event_count_out[[j]] <- event_count
  
}



# Identify Individual sighted less than three times (transients) ----------
transients_out <- list()

for(k in 1:length(event_count_out)){
  event_count <- event_count_out[[k]]
  year <- unique(event_count$observation.year)
  
  transients <- event_count %>%
    dplyr::filter(total_events < 3) %>%
    dplyr::pull(ElephantName)
  
  transients_out[[k]] <- transients
  
}

names(transients_out) <- c("y2007", "y2008", "y2009", "y2010", "y2011")

# Identify musth bulls ----------------------------------------------------
mushara_07_musth <- c("smokey", "jaws", "104")
mushara_08_musth <- c("smokey", "beckham", "gary", "jeff", "mike", "108", "123", "ozzie")
mushara_09_musth <- c("smokey", "mike", "gary", "etosha", "mike", "grey")
mushara_10_musth <- c("smokey", "beckham", "marlonbrando", "mike", "princecharles")
mushara_11_musth <- c("smokey", "mike", "marlonbrando")

# Compile individuals to exclude ----------------------------------------------------
mushara_07_allexclusions <- c(mushara_07_musth, transients_out$y2007)
mushara_08_allexclusions <- c(mushara_08_musth, transients_out$y2008)
mushara_09_allexclusions <- c(mushara_09_musth, transients_out$y2009)
mushara_10_allexclusions <- c(mushara_10_musth, transients_out$y2010)
mushara_11_allexclusions <- c(mushara_11_musth, transients_out$y2011)

# Prep for network conversion ---------------------------------------------
mushara_07_networkprep <- prep_network_data(
  attendance_df = attendance_records_out$y2007,
  excluded_individuals = mushara_07_allexclusions,
  association_index = "SRI"
)

mushara_08_networkprep <- prep_network_data(
  attendance_df = attendance_records_out$y2008,
  excluded_individuals = mushara_08_allexclusions,
  association_index = "SRI"
)

mushara_09_networkprep <- prep_network_data(
  attendance_df = attendance_records_out$y2009,
  excluded_individuals = mushara_09_allexclusions,
  association_index = "SRI"
)

mushara_10_networkprep <- prep_network_data(
  attendance_df = attendance_records_out$y2010,
  excluded_individuals = mushara_10_allexclusions,
  association_index = "SRI"
)

mushara_11_networkprep <- prep_network_data(
  attendance_df = attendance_records_out$y2011,
  excluded_individuals = mushara_11_allexclusions,
  association_index = "SRI"
)


# Assemble node-level centrality databases ---------------------------------
centrality_2007 <- calculate_centrality_metrics(g=mushara_07_networkprep) %>%
  dplyr::mutate(observation.year = "2007")
centrality_2008 <- calculate_centrality_metrics(g=mushara_08_networkprep) %>%
  dplyr::mutate(observation.year = "2008")
centrality_2009 <- calculate_centrality_metrics(g=mushara_09_networkprep) %>%
  dplyr::mutate(observation.year = "2009")
centrality_2010 <- calculate_centrality_metrics(g=mushara_10_networkprep) %>%
  dplyr::mutate(observation.year = "2010")
centrality_2011 <- calculate_centrality_metrics(g=mushara_11_networkprep) %>%
  dplyr::mutate(observation.year = "2011")

centrality_list <- list(centrality_2007, centrality_2008, centrality_2009, centrality_2010, centrality_2011)

# Join all centrality databases together ---------------------------------
all_centrality <- bind_rows(centrality_list) %>%
  group_by(observation.year) %>%
  dplyr::arrange(., desc(observation.year), desc(eigenvector)) 

all_centrality_summary <- all_centrality %>%
  group_by(node.name) %>%
  dplyr::select(eigenvector) %>%
  dplyr::summarise(
    n_years = n(),
    mean_eigenvector = mean(eigenvector),
    sd_eigenvector=sd(eigenvector))


# Network figures ---------------------------------------------------------
network_list <- list(mushara_07_networkprep, mushara_08_networkprep, mushara_09_networkprep, mushara_10_networkprep, mushara_11_networkprep)
centrality_list <- list(centrality_2007, centrality_2008, centrality_2009, centrality_2010, centrality_2011)

ggnet_list <- list() 

for(i in 1:length(network_list)){
  network <- asNetwork(network_list[[i]])
  ggnet <- ggnetwork(network, layout="fruchtermanreingold", cell.jitter=0.75, weights="weight")
  
  ggnet_centrality <- ggnet %>%
    left_join(., centrality_list[[i]], by=c("vertex.names"="node.name"))
  
  ggnet_list[[i]] <- ggnet_centrality
  
}

### Bind all rows of dataframe together 
ggnet_df <- bind_rows(ggnet_list)


### Facet labs 
year.labs <- c("2007", "2008", "2009", "2010", "2011")
names(year.labs) <- c("2007", "2008", "2009", "2010", "2011")

### Plot the network using facets for each observation year 
ggplot(ggnet_df,
       aes(
         x = x,
         y = y,
         xend = xend,
         yend = yend
       )) +
  geom_edges(aes(alpha = weight), curvature = 0.1) +
  geom_nodes(aes(size=eigenvector, col=eigenvector), col="gold") + 
  geom_nodetext(data=ggnet_df, aes(x = x,
                                   y = y,
                                   label = vertex.names),
                fontface = "bold", inherit.aes = F, size=3) +
  facet_wrap( .~ observation.year, nrow=2, labeller = labeller(observation.year = year.labs)) + 
  theme(strip.text.x = element_text(size = 12, color = "black", face = "bold.italic")) + 
  labs(size="Eigenvector centrality",
       alpha="Association Index (SRI)",
       title="Mushara male elephant networks") + 
  theme_blank()

### Can adjust settings to output file as desired
ggsave("mushara_networks.png", width = 14, height = 8, units = "in", dpi=300)


