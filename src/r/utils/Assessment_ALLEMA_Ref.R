#-----------------------------------------------------
# Setting up
#-----------------------------------------------------

#-----------------------------------------------------
# Libraries
#-----------------------------------------------------

library(terra)
library(sf)
library(dplyr)
library(tidyr)

#-----------------------------------------------------
# Load reference data
#-----------------------------------------------------

ALLEMA_EB <- st_read(
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/REF_DATA/ALLEMA/ALLEMA_EB_ref.gpkg"
) 

# ALLEMA_schafboden <- ALLEMA_EB[which(ALLEMA_EB$FK_Quadrat == 689214),]

# recompute area to have a "unit" object
ALLEMA_EB <- ALLEMA_EB %>%
  mutate(
    area_allema = st_area(geom),
  )

#-----------------------------------------------------
# Segmentation files
#-----------------------------------------------------

ALLEMA_seg_path <- "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/PROCESSED_DATA/ALLEMA/"

ALLEMA_seg_EB <- list.files(
  ALLEMA_seg_path,
  pattern = "//.gpkg$",
  full.names = FALSE
)

#-----------------------------------------------------
# Container
#-----------------------------------------------------

all_results <- list()

counter <- 1

#-----------------------------------------------------
# MAIN LOOP
#-----------------------------------------------------

for(i in seq_along(ALLEMA_seg_EB)){
  
  # i <- 24 # Schafboden
  
  current_file <- ALLEMA_seg_EB[i]
  
  quadrant_id <- as.integer(
    gsub("ALLEMA_", "", gsub(".gpkg", "", current_file))
  )
  
  cat("/n============================/n")
  cat("Quadrant:", quadrant_id, "/n")
  
  #---------------------------------------------------
  # Reference polygons for current quadrant
  #---------------------------------------------------
  
  EB_ref <- ALLEMA_EB %>%
    filter(FK_Quadrat == quadrant_id)
  
  #---------------------------------------------------
  # Available methods
  #---------------------------------------------------
  
  seg_EB_layers <- st_layers(
    paste0(ALLEMA_seg_path, current_file)
  )
  
  #---------------------------------------------------
  # LOOP SEGMENTATION METHODS
  #---------------------------------------------------
  
  for(j in seq_along(seg_EB_layers$name)){
    
    segmentation_method <- seg_EB_layers$name[j]
    
    cat("Method:", segmentation_method, "/n")
    
    #-------------------------------------------------
    # Read segmentation
    #-------------------------------------------------
    
    EB_seg <- st_read(
      paste0(ALLEMA_seg_path, current_file),
      layer = segmentation_method,
      quiet = TRUE
    )
    
    # Skip empty geometries
    if(nrow(EB_seg) == 0){
      cat("Empty layer! /n")
      next
    }
    
    # Standardize crown ID column
    names(EB_seg)[1] <- "crown_id"
    
    EB_seg <- EB_seg %>%
      st_transform(st_crs(EB_ref)) %>%
      mutate(
        area_crown = st_area(geom)
      )
    
    #-------------------------------------------------
    # Intersections
    #-------------------------------------------------
    
    intersections <- st_intersection(
      
      EB_ref %>%
        select(
          FK_Quadrat,
          OBJECT_ID,
          Gehoelztyp,
          roundness,
          area_allema
        ),
      
      EB_seg %>%
        select(
          crown_id,
          area_crown
        )
    )
    
    #-------------------------------------------------
    # Metrics
    #-------------------------------------------------
    
    intersections <- intersections %>%
      mutate(
        
        area_intersection =
          st_area(geom),
        
        perc_ALLEMA =
          as.numeric(
            area_intersection / area_allema
          ) * 100,
        
        perc_crown =
          as.numeric(
            area_intersection / area_crown
          ) * 100,
        
        # IoU
        iou =
          as.numeric(
            area_intersection /
              (area_allema +
                 area_crown -
                 area_intersection)
          ),
        
        segmentation_method =
          segmentation_method
      )
    
    #-------------------------------------------------
    # Final table
    #-------------------------------------------------
    
    result_table <- intersections %>%
      st_drop_geometry() %>%
      select(
        FK_Quadrat,
        segmentation_method,
        OBJECT_ID,
        crown_id,
        Gehoelztyp,
        roundness,
        perc_ALLEMA,
        perc_crown,
        iou
      )
    
    #-------------------------------------------------
    # Store
    #-------------------------------------------------
    
    all_results[[counter]] <- result_table
    
    counter <- counter + 1
  }
}

#-----------------------------------------------------
# Combine everything
#-----------------------------------------------------

results_all <- bind_rows(all_results)

#=====================================================
# ANALYSIS
#=====================================================

#-----------------------------------------------------
# BEST MATCH PER REFERENCE TREE
#-----------------------------------------------------

# Keep best overlapping crown for each reference tree

best_matches <- results_all %>%
  group_by(
    FK_Quadrat,
    segmentation_method,
    OBJECT_ID
  ) %>%
  slice_max(
    iou,
    n = 1,
    with_ties = FALSE
  ) %>%
  ungroup()

#-----------------------------------------------------
# CREATE COMPLETE REFERENCE TABLE
#-----------------------------------------------------

all_ref <- ALLEMA_EB %>%
  st_drop_geometry() %>%
  select(
    FK_Quadrat,
    OBJECT_ID
  )

all_methods <- results_all %>%
  distinct(
    FK_Quadrat,
    segmentation_method
  )

all_combinations <- all_ref %>%
  inner_join(
    all_methods,
    by = "FK_Quadrat"
  )

#-----------------------------------------------------
# DETECTION THRESHOLD
#-----------------------------------------------------

# Since reference polygons are approximate use a low threshold

iou_threshold <- 0.25

#-----------------------------------------------------
# DETECTION TABLE
#-----------------------------------------------------

detection_table <- all_combinations %>%
  left_join(
    best_matches,
    by = c(
      "FK_Quadrat",
      "segmentation_method",
      "OBJECT_ID"
    )
  ) %>%
  mutate(
    detected =
      ifelse(
        is.na(iou),
        FALSE,
        iou >= iou_threshold
      )
  )

#=====================================================
# OVERSEGMENTATION
#=====================================================

# One reference tree intersects many crowns

oversegmentation <- results_all %>%
  group_by(
    FK_Quadrat,
    segmentation_method,
    OBJECT_ID
  ) %>%
  summarise(
    n_crowns =
      n_distinct(crown_id),
    .groups = "drop"
  )

#=====================================================
# UNDERSEGMENTATION
#=====================================================

# One segmented crown intersects many refs

undersegmentation <- results_all %>%
  group_by(
    FK_Quadrat,
    segmentation_method,
    crown_id
  ) %>%
  summarise(
    n_ref =
      n_distinct(OBJECT_ID),
    .groups = "drop"
  )

#=====================================================
# SUMMARY STATISTICS
#=====================================================

summary_detection <- detection_table %>%
  group_by(
    FK_Quadrat,
    segmentation_method
  ) %>%
  summarise(
    
    n_reference =
      n(),
    
    n_detected =
      sum(detected),
    
    n_omission =
      sum(!detected),
    
    detection_rate =
      mean(detected),
    
    mean_iou =
      mean(iou, na.rm = TRUE),
    
    median_iou =
      median(iou, na.rm = TRUE),
    
    mean_perc_ALLEMA =
      mean(perc_ALLEMA, na.rm = TRUE),
    
    mean_perc_crown =
      mean(perc_crown, na.rm = TRUE),
    
    .groups = "drop"
  )

#-----------------------------------------------------
# Oversegmentation summary
#-----------------------------------------------------

summary_overseg <- oversegmentation %>%
  group_by(
    FK_Quadrat,
    segmentation_method
  ) %>%
  summarise(
    
    n_one_to_one =
      sum(n_crowns == 1),
    
    n_one_to_many =
      sum(n_crowns > 1),
    
    overseg_rate =
      mean(n_crowns > 1),
    
    mean_intersections =
      mean(n_crowns),
    
    .groups = "drop"
  )

#-----------------------------------------------------
# Undersegmentation summary
#-----------------------------------------------------

summary_underseg <- undersegmentation %>%
  group_by(
    FK_Quadrat,
    segmentation_method
  ) %>%
  summarise(
    
    n_many_to_one =
      sum(n_ref > 1),
    
    underseg_rate =
      mean(n_ref > 1),
    
    .groups = "drop"
  )

#=====================================================
# FINAL SUMMARY TABLE
#=====================================================

summary_table <- summary_detection %>%
  left_join(
    summary_overseg,
    by = c(
      "FK_Quadrat",
      "segmentation_method"
    )
  ) %>%
  left_join(
    summary_underseg,
    by = c(
      "FK_Quadrat",
      "segmentation_method"
    )
  )

#=====================================================
# METHOD RANKING
#=====================================================

method_ranking <- summary_table %>%
  group_by(segmentation_method) %>%
  summarise(
    
    mean_detection_rate =
      mean(detection_rate, na.rm = TRUE),
    
    mean_iou =
      mean(mean_iou, na.rm = TRUE),
    
    mean_overseg_rate =
      mean(overseg_rate, na.rm = TRUE),
    
    mean_underseg_rate =
      mean(underseg_rate, na.rm = TRUE),
    
    mean_one_to_one =
      mean(n_one_to_one / n_reference),
    
    .groups = "drop"
  )

#-----------------------------------------------------
# FINAL COMPOSITE SCORE
#-----------------------------------------------------

method_ranking <- method_ranking %>%
  mutate(

    rank_detection =
      rank(-mean_detection_rate),

    rank_iou =
      rank(-mean_iou),

    rank_overseg =
      rank(mean_overseg_rate),

    rank_underseg =
      rank(mean_underseg_rate)

  ) 
  # # TBD - Final score is a bit tricky, very much dependant on the coefficients we set for the different methods, if we use ranking or mean value, etc.
  # %>%
  # mutate(
  #   
  #   final_score =
  #     (
  #       rank_detection * 0.25 +
  #         rank_iou * 0.25 +
  #         rank_overseg * 0.25 +
  #         rank_underseg * 0.25
  #     )
  #   
  # ) %>%
  # arrange(final_score)



#=====================================================
# EXPORT
#=====================================================

write.csv(
  results_all,
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ASSESSMENT_DATA/ALLEMA/all_intersections.csv",
  row.names = FALSE
)

write.csv(
  summary_table,
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ASSESSMENT_DATA/ALLEMA/summary_table.csv",
  row.names = FALSE
)

write.csv(
  method_ranking,
  "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ASSESSMENT_DATA/ALLEMA/method_ranking.csv",
  row.names = FALSE
)

#=====================================================
# Linking it back to the ref trees
#=====================================================

tree_status <- best_matches %>%
  
  left_join(
    oversegmentation,
    by = c(
      "FK_Quadrat",
      "segmentation_method",
      "OBJECT_ID"
    )
  ) %>%
  
  left_join(
    undersegmentation,
    by = c(
      "FK_Quadrat",
      "segmentation_method",
      "crown_id"
    )
  )


tree_status <- tree_status %>%
  mutate(
    
    status = case_when(
      
      is.na(iou) ~ "not_detected",
      
      iou < iou_threshold ~ "not_detected",
      
      n_crowns > 1 ~ "oversegmented",
      
      n_ref > 1 ~ "undersegmented",
      
      n_crowns == 1 &
        n_ref == 1 ~ "one_to_one",
      
      TRUE ~ "other"
    )
    
  )


tree_status_complete <- all_combinations %>%
  
  left_join(
    tree_status,
    by = c(
      "FK_Quadrat",
      "segmentation_method",
      "OBJECT_ID"
    )
  ) %>%
  
  mutate(
    status =
      ifelse(
        is.na(status),
        "not_detected",
        status
      )
  )

tree_summary <- tree_status_complete %>%
  group_by(
    FK_Quadrat,
    OBJECT_ID
  ) %>%
  summarise(
    
    n_methods =
      n(),
    
    n_not_detected =
      sum(status == "not_detected"),
    
    n_one_to_one =
      sum(status == "one_to_one"),
    
    n_oversegmented =
      sum(status == "oversegmented"),
    
    n_undersegmented =
      sum(status == "undersegmented"),
    
    .groups = "drop"
  )

ALLEMA_assessment <- ALLEMA_EB %>%
  
  left_join(
    tree_summary,
    by = c(
      "FK_Quadrat",
      "OBJECT_ID"
    )
  )

st_write(ALLEMA_assessment, "//speedy16-36/data_15/_PROJEKTE/20260401_Boschettissimo/01_Daten/GIS/ASSESSMENT_DATA/ALLEMA/ALLEMA_assessment.gpkg", append = FALSE)
