#-----------------------------------------------------
# Segmented tree filters
#-----------------------------------------------------
# Individual filter steps used to narrow the segmented trees down to the set
# comparable to what the BLW pays for, kept as small standalone functions so
# a diagnostic script can report counts after each step while 004_FilterTrees.R
# just chains all of them.

# Trees on forest parcels (LN code 901)
filter_forest_parcels <- function(trees) {
  trees %>%
    filter(!grepl("(^|;)901($|;)", lnf_codes))
}

# Trees too close to the TLM forest mask
filter_forest_distance <- function(trees, min_dist = 2) {
  trees %>%
    filter(dist_to_forest > min_dist)
}

# Trees on productive orchard parcels (702/703/704)
filter_orchard_parcels <- function(trees) {
  trees %>%
    filter(!grepl("(^|;)(702|703|704)($|;)", lnf_codes))
}

# Christmas trees (712) and pepinieres (713, 722, 723, 724)
filter_nurseries <- function(trees) {
  trees %>%
    filter(!grepl("(^|;)(712|713|722|723|724)($|;)", lnf_codes))
}

# Hedges (852, 857, 858)
filter_hedges <- function(trees) {
  trees %>%
    filter(!grepl("(^|;)(852|857|858)($|;)", lnf_codes))
}

# Other non-eligible LN codes (902-909, 998)
filter_misc_lnf <- function(trees) {
  trees %>%
    filter(!grepl("(^|;)(902|903|904|905|906|907|908|909|998)($|;)", lnf_codes))
}

# Sommerungsweiden and Waldweiden (930, 933, 935, 936, 618, 625)
filter_pastures <- function(trees) {
  trees %>%
    filter(!grepl("(^|;)(930|933|935|936|618|625)($|;)", lnf_codes))
}

# All filters chained together, in the order they should be applied
filter_segmented_trees <- function(trees, min_dist_to_forest = 2) {
  trees %>%
    filter_forest_parcels() %>%
    filter_forest_distance(min_dist_to_forest) %>%
    filter_orchard_parcels() %>%
    filter_nurseries() %>%
    filter_hedges() %>%
    filter_misc_lnf() %>%
    filter_pastures()
}
