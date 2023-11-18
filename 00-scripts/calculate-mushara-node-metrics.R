## ---------------------------
##
## Script name: calculate-mushara-node-metrics.R
##
## Purpose of script: This script is designed to calculate node-level centrality metrics for each observation year in the Mushara database
##
## Author: Patrick Freeman
##
## Date Created: 11/18/2023
## Date last updated: 11/18/2023
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

# Important functions -----------------------------------------------------
### Function that returns the unique individuals in each event 
build_attendance_records <- function(df){
  
  df_split <- split(df, f=df$year.event)
  
  attendance_records_list <- list()
  for(i in 1:length(df_split)){
    individuals <- df_split[[i]] %>%
      select(Subject1_Master, observation.year) %>%
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
    dplyr::select(Subject1_Master, year.event) %>% 
    dplyr::filter(!Subject1_Master %in% excluded_individuals) %>% 
    dplyr::rename(Event = year.event,
                  Individual = Subject1_Master) %>% 
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
mushara_records <- read_csv("/Users/patrickfreeman-csp/Downloads/mushara_2007-2011_events-2023-04-01.csv")


# Split dataset by year ----------------------------------------------
mushara_years <- split(mushara_records, f=mushara_records$observation.year)


# Obtain attendance records for each year   -------------------------------------
attendance_records_out <- list()
for(i in 1:length(mushara_years)){
  year = mushara_years[[i]] %>%
    dplyr::mutate(year.event = paste(observation.year, event.number, sep="_"))
  
  attendance_records <- build_attendance_records(year) %>%
    dplyr::filter(!Subject1_Master %in% c("female elephant", "5minscan", NA, "Unknown1", "Unknown2", "Unknown2", "Unknown3", "Unknown4", "herd", "herd member"))
  
  attendance_records_out[[i]] <- attendance_records
}

names(attendance_records_out) <- c("y2007", "y2008", "y2009", "y2010", "y2011")

# tabulate number of events observed for each individual  -------------------------------------
event_count_out <- list() 
for(j in 1:length(attendance_records_out)){
  
  attendance_records <- attendance_records_out[[j]]
  
  event_count <- attendance_records %>%
    group_by(Subject1_Master) %>% 
    dplyr::summarise(total_events = n()) %>%
    dplyr::mutate(observation.year = unique(attendance_records$observation.year))
  
  event_count_out[[j]] <- event_count
  
}



# Identify Individual sighted less than three times (transients) ----------
transients_out <- list()

for(k in 1:length(event_count_out)){
  event_count <- event_count_out[[k]]
  year <- unique(event_count$observation.year)
  
  transients <- event_count %>%
    dplyr::filter(total_events < 3) %>%
    dplyr::pull(Subject1_Master)
  
  transients_out[[k]] <- transients
  
}

names(transients_out) <- c("y2007", "y2008", "y2009", "y2010", "y2011")

# Identify musth bulls ----------------------------------------------------
mushara_07_musth <- c("Smokey (78)", "Jaws (104)")
mushara_08_musth <- c("Smokey (78)")
mushara_09_musth <- c("Smokey (78)")
mushara_10_musth <- c("Smokey (78)")
mushara_11_musth <- c("Smokey (78)")

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
