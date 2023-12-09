## ---------------------------
##
## Script name: calculate-mushara-dominance-hierarchies.R
##
## Purpose of script: This script is designed to calculate population-wide dominance scores based on a composite data table cleaned by Jodie Berezin covering the years 2007-2012. 
##
## Author: Patrick Freeman
##
## Date Created: 12/9/2023
## Date last updated: 12/29/23
##
## Email contact: ptfreeman[at]alumni.stanford.ed
##
## ---------------------------
##
## Notes: 
##  
library(aniDom)
library(EloRating)
library(Perc)
library(tidyverse)

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


prep_dominance_data <- function(yearly_records=yearly_records,
                                filter_vector=filter_vector,
                                attendance_record = attendance_record){
  
  ### Filter out transient and musth bulls first - all individuals should be reprsented at least once in the ElephantName column 
  yearly_records_filt <- yearly_records %>%
    dplyr::filter(!ElephantName %in% filter_vector,
                  !Object %in% mushara_07_allexclusions) %>%
    dplyr::mutate(year.event = paste(Year, Event, sep="_")) %>%
    dplyr::filter(ElephantName %in% attendance_record$ElephantName)
  
  ### Get record of all individuals present regardless of whether they were involved in a displacement or not 
  unique_individuals <- sort(unique(yearly_records_filt$ElephantName))
  
  ### Now retain only the displacements/dominance interactions 
  dom_only <- yearly_records_filt %>%
    dplyr::filter(Action1 == "displacement") %>%
    dplyr::filter(!ElephantName %in% filter_vector,
                  !Object %in% filter_vector) %>%
    dplyr::select(Year, Date, Event, year.event, ElephantName, Object) %>%
    dplyr::rename(Winner = ElephantName, 
                  Loser = Object)
  
  ### And prep a conflict matrix for the calculation of David's Scores using the EloRating Package
  ds_conflict_mat <- dom_only %>%
    dplyr::select(Winner, Loser) %>%
    as.matrix() %>%
    as.conflictmat(.)
  
  ### Output as a list with two elements, first with the list of all individuals recorded after filtering transients and musth bulls and the second being the dominance interactions that will be used for dominance hierarchy calculations
  output_list <- list(unique_individuals, dom_only, ds_conflict_mat)
  
  return(output_list)
}



# Load composite dataset  -------------------------------------------------
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


# Create vector of extraneous removals  -----------------------------------

remove <- c("femaleelephant", "5minscan", NA, "unknown1", "unknown2", "unknown3", "unknown4", "unknown5", "unknown6", "unknown7", "unknown8", "unknown9", "unknown10", "incomingelephan", "unknownsubject", "vehicle", "watertrough", "wind", "{end}", "?", "feces", "herd", "herd member", "startscan", "otherwildlife", "othermisc")

# Compile individuals to exclude ----------------------------------------------------
mushara_07_allexclusions <- c(mushara_07_musth, transients_out$y2007, remove)
mushara_08_allexclusions <- c(mushara_08_musth, transients_out$y2008, remove)
mushara_09_allexclusions <- c(mushara_09_musth, transients_out$y2009, remove)
mushara_10_allexclusions <- c(mushara_10_musth, transients_out$y2010, remove)
mushara_11_allexclusions <- c(mushara_11_musth, transients_out$y2011, remove)

# Prep dominance data ----------------------------------------------------

mushara_07_dominance_prep <- prep_dominance_data(yearly_records = mushara_years$`2007`,
                                                 filter_vector = mushara_07_allexclusions,
                                                 attendance_record=attendance_records_out$y2007)

mushara_08_dominance_prep <- prep_dominance_data(yearly_records = mushara_years$`2008`,
                                                 filter_vector = mushara_08_allexclusions,
                                                 attendance_record=attendance_records_out$y2008)

mushara_09_dominance_prep <- prep_dominance_data(yearly_records = mushara_years$`2009`,
                                                 filter_vector = mushara_09_allexclusions,
                                                 attendance_record=attendance_records_out$y2009)


mushara_10_dominance_prep <- prep_dominance_data(yearly_records = mushara_years$`2010`,
                                                 filter_vector = mushara_10_allexclusions,
                                                 attendance_record=attendance_records_out$y2010)


mushara_11_dominance_prep <- prep_dominance_data(yearly_records = mushara_years$`2011`,
                                                 filter_vector = mushara_11_allexclusions,
                                                 attendance_record=attendance_records_out$y2011)



# Estimate Elo ratings, hierarchy, and ranks ----------------------------------------------------

mushara_07_dom <- elo_scores(winners=mushara_07_dominance_prep[[2]]$Winner,
                             losers=mushara_07_dominance_prep[[2]]$Loser,
                             identities=mushara_07_dominance_prep[[1]],
                             sigmoid.param=1/100, 
                             K=200,
                             init.score=0,
                             randomise=T,
                             n.rands=1000, 
                             return.as.ranks=TRUE,
                             return.trajectories=FALSE, 
                             dates=NULL)
### Plot the ranks
plot_ranks(mushara_07_dom, plot.CIs=T, plot.identities=T, ordered.by.rank = T)

### Calculate David's Scores - corrected for number of interactions in a dyad 
mush_07_ds <- DS(mushara_07_dominance_prep[[3]], prop=c("Dij"))

mushara_08_dom <- elo_scores(winners=mushara_08_dominance_prep[[2]]$Winner,
                             losers=mushara_08_dominance_prep[[2]]$Loser,
                             #identities=mushara_08_dominance_prep[[1]],
                             sigmoid.param=1/100, 
                             K=200,
                             init.score=0,
                             randomise=T,
                             n.rands=1000, 
                             return.as.ranks=TRUE,
                             return.trajectories=FALSE, 
                             dates=NULL)
### Plot the ranks
plot_ranks(mushara_08_dom, plot.CIs=T, plot.identities=T, ordered.by.rank = T)

### Calculate David's Scores - corrected for number of interactions in a dyad 
mush_08_ds <- DS(mushara_08_dominance_prep[[3]], prop=c("Dij"))


mushara_09_dom <- elo_scores(winners=mushara_09_dominance_prep[[2]]$Winner,
                             losers=mushara_09_dominance_prep[[2]]$Loser,
                             #identities=mushara_09_dominance_prep[[1]],
                             sigmoid.param=1/100, 
                             K=200,
                             init.score=0,
                             randomise=T,
                             n.rands=1000, 
                             return.as.ranks=TRUE,
                             return.trajectories=FALSE, 
                             dates=NULL)
### Plot the ranks
plot_ranks(mushara_09_dom, plot.CIs=T, plot.identities=T, ordered.by.rank = T)

### Calculate David's Scores - corrected for number of interactions in a dyad 
mush_09_ds <- DS(mushara_09_dominance_prep[[3]], prop=c("Dij"))


mushara_10_dom <- elo_scores(winners=mushara_10_dominance_prep[[2]]$Winner,
                             losers=mushara_10_dominance_prep[[2]]$Loser,
                            # identities=mushara_10_dominance_prep[[1]],
                             sigmoid.param=1/100, 
                             K=200,
                             init.score=0,
                             randomise=T,
                             n.rands=1000, 
                             return.as.ranks=TRUE,
                             return.trajectories=FALSE, 
                             dates=NULL)
### Plot the ranks
plot_ranks(mushara_10_dom, plot.CIs=T, plot.identities=T, ordered.by.rank = T)
### Calculate David's Scores - corrected for number of interactions in a dyad 
mush_10_ds <- DS(mushara_10_dominance_prep[[3]], prop=c("Dij"))


mushara_11_dom <- elo_scores(winners=mushara_11_dominance_prep[[2]]$Winner,
                             losers=mushara_11_dominance_prep[[2]]$Loser,
                             identities=mushara_11_dominance_prep[[1]],
                             sigmoid.param=1/100, 
                             K=200,
                             init.score=0,
                             randomise=T,
                             n.rands=1000, 
                             return.as.ranks=TRUE,
                             return.trajectories=FALSE, 
                             dates=NULL)


### Plot the ranks
plot_ranks(mushara_11_dom, plot.CIs=T, plot.identities=T, ordered.by.rank = T)
### Calculate David's Scores - corrected for number of interactions in a dyad 
mush_11_ds <- DS(mushara_11_dominance_prep[[3]], prop=c("Dij"))

