#-----------------------------------------------------
# Test_Obstgarten_V2.R
#
# Identify orchard patches ("Obstgärten") from single trees, following the
# AGRIDEA definition used here:
#   - trees less than 50 m apart from each other belong to the same patch
#   - a patch must contain at least 5 trees
#
# Approach (replaces the alpha shape of V1):
#   1. Clustering = connected components of the "neighbour graph"
#      (two trees are linked if their distance is < max_dist). This is exactly
#      the definition (single-linkage / DBSCAN with minPts = 1), so membership
#      is unambiguous and no tree is lost.
#   2. Keep clusters with >= min_trees trees.
#   3. Outline per cluster = union of
#        - Delaunay triangles whose three edges are all < max_dist
#        - Delaunay edges < max_dist (keeps tree rows / chains connected)
#        - a small buffer around each tree (or the crown polygons themselves)
#      Result: exactly one connected polygon per orchard, tight around the trees.
#-----------------------------------------------------

#-----------------------------------------------------
# Libraries
#-----------------------------------------------------

library(sf)
library(dplyr)
library(dbscan)   # fast fixed-radius clustering (trunk mode)
library(igraph)   # connected components (crown mode)

#-----------------------------------------------------
# Paths & config
#-----------------------------------------------------

source("src/r/001_Initialization.R")

in_file        <- "D:/temp/EB_sub_GL_sub2.gpkg"
out_orchards   <- "D:/temp/obstgarten_v2.gpkg"
out_trees      <- "D:/temp/obstgarten_v2_trees.gpkg"

max_dist       <- 50      # [m] max distance between neighbouring trees
min_trees      <- 5       # minimum number of trees per orchard
dist_mode      <- "trunk" # "trunk": distance between tree centres (points)
                          # "crown": distance between crown polygon edges
outline_buffer <- 5       # [m] buffer around trees/edges for the outline

#-----------------------------------------------------
# Functions
#-----------------------------------------------------

# Assign a cluster id to every tree (connected components at distance < max_dist)
cluster_trees <- function(trees, max_dist, dist_mode = "trunk") {
  if (dist_mode == "trunk") {
    xy <- st_coordinates(st_centroid(st_geometry(trees)))
    # minPts = 1 -> every point is a core point -> clusters = connected components.
    # dbscan uses "<= eps"; subtract a tiny amount to get a strict "< max_dist".
    return(dbscan::dbscan(xy, eps = max_dist - 1e-6, minPts = 1)$cluster)
  }
  if (dist_mode == "crown") {
    nb <- st_is_within_distance(trees, trees, dist = max_dist)
    g  <- igraph::graph_from_adj_list(nb, mode = "all")
    return(igraph::components(g)$membership)
  }
  stop("dist_mode must be 'trunk' or 'crown'")
}

# Straight line segments between pairs of points (fallback for degenerate cases)
pair_lines <- function(pts, max_dist) {
  nb <- st_is_within_distance(pts, pts, dist = max_dist)
  xy <- st_coordinates(pts)
  pr <- do.call(rbind, lapply(seq_along(nb), function(i) {
    j <- nb[[i]][nb[[i]] > i]
    if (length(j)) cbind(i, j) else NULL
  }))
  if (is.null(pr)) return(st_sfc(crs = st_crs(pts)))
  st_sfc(lapply(seq_len(nrow(pr)), function(k)
    st_linestring(xy[pr[k, ], , drop = FALSE])), crs = st_crs(pts))
}

# Outline polygon of one cluster of trees
orchard_outline <- function(pts, crowns = NULL, max_dist, buf) {
  crs <- st_crs(pts)
  mp  <- st_union(pts)                      # removes duplicate points as well

  # Delaunay triangles, keep those whose longest edge is < max_dist
  tri <- st_collection_extract(st_triangulate(mp), "POLYGON")
  if (length(tri) > 0) {
    cc <- st_coordinates(tri)               # X, Y, L1 (ring), L2 (triangle)
    max_edge <- tapply(seq_len(nrow(cc)), cc[, "L2"], function(ix) {
      p <- cc[ix, c("X", "Y")]
      max(sqrt(rowSums(diff(p)^2)))
    })
    tri <- tri[max_edge < max_dist]
  }

  # Delaunay edges < max_dist (they contain the minimum spanning tree, so the
  # cluster stays connected, including linear tree rows)
  edg <- st_triangulate(mp, bOnlyEdges = TRUE)
  edg <- if (!st_is_empty(edg)) st_cast(st_cast(edg, "MULTILINESTRING"), "LINESTRING") else st_sfc(crs = crs)
  if (length(edg) > 0) edg <- edg[as.numeric(st_length(edg)) < max_dist]
  if (length(edg) == 0) edg <- pair_lines(pts, max_dist)   # e.g. all trees collinear

  parts <- c(st_geometry(tri),
             st_buffer(edg, buf),
             st_buffer(st_geometry(pts), buf))
  if (!is.null(crowns)) parts <- c(parts, st_geometry(crowns))

  st_union(st_make_valid(parts))
}

# Main wrapper
identify_orchards <- function(trees, max_dist = 50, min_trees = 5,
                              dist_mode = "trunk", outline_buffer = 5) {

  is_poly <- all(st_geometry_type(trees) %in% c("POLYGON", "MULTIPOLYGON"))
  if (dist_mode == "crown" && !is_poly) stop("dist_mode = 'crown' needs crown polygons")

  trees$cluster_id <- cluster_trees(trees, max_dist, dist_mode)

  # Keep only clusters with enough trees, renumber them
  trees <- trees %>%
    group_by(cluster_id) %>%
    mutate(n_trees = n()) %>%
    ungroup() %>%
    mutate(is_orchard = n_trees >= min_trees,
           orchard_id = ifelse(is_orchard,
                               as.integer(factor(ifelse(is_orchard, cluster_id, NA))),
                               NA_integer_))

  ids <- sort(unique(na.omit(trees$orchard_id)))
  if (length(ids) == 0) {
    warning("No orchard found.")
    return(list(orchards = NULL, trees = trees))
  }

  geoms <- lapply(ids, function(k) {
    sel  <- trees[which(trees$orchard_id == k), ]
    pts  <- st_centroid(st_geometry(sel))
    orchard_outline(pts, if (is_poly) sel else NULL, max_dist, outline_buffer)
  })

  orchards <- st_sf(orchard_id = ids,
                    geom = do.call(c, geoms),
                    crs  = st_crs(trees)) %>%
    left_join(trees %>% st_drop_geometry() %>%
                filter(is_orchard) %>% count(orchard_id, name = "n_trees"),
              by = "orchard_id") %>%
    mutate(area_m2        = as.numeric(st_area(geom)),
           trees_per_ha   = n_trees / area_m2 * 1e4)

  list(orchards = orchards, trees = trees)
}

#-----------------------------------------------------
# Test
#-----------------------------------------------------

EB_GL <- st_read(in_file)
if (is.na(st_crs(EB_GL))) EB_GL <- st_set_crs(EB_GL, 2056)

res <- identify_orchards(EB_GL,
                         max_dist       = max_dist,
                         min_trees      = min_trees,
                         dist_mode      = dist_mode,
                         outline_buffer = outline_buffer)

cat(sprintf("%d trees, %d orchards, %d trees in orchards\n",
            nrow(res$trees), nrow(res$orchards), sum(res$trees$is_orchard)))

st_write(res$orchards, out_orchards, append = FALSE)
st_write(res$trees,    out_trees,    append = FALSE)
