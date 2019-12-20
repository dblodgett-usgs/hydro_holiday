review_trees <- function(trees, forest) {
  tree_ids <- trees$LevelPathI
  
  fr <- select(forest, COMID, LevelPathI, Hydroseq, DnHydroseq, Pathlength, LENGTHKM)
  tree_data <- pblapply(tree_ids, get_tree, forest = fr)
  
  tree_data <- lapply(tree_data, trunk_metric)
}

get_tree <- function(tree_id, forest) {
  stump <- filter(forest, .data$LevelPathI == tree_id) %>%
    filter(.data$Hydroseq == min(.data$Hydroseq))
  
  all_branches <- get_UT(forest, stump$COMID)
  
  filter(forest, .data$COMID %in% all_branches)
}

trunk_metric <- function(tree) {
  trunk <- filter(tree, .data$LevelPathI == min(.data$LevelPathI))
  
  top_bottom <- get_top_bottom(trunk)
  
  trunk_length <- sum(trunk$LENGTHKM)
  
  trunk_height <- st_distance(top_bottom$top, top_bottom$bottom) / 1000
  
  straightness <- trunk_height / trunk_length
  
  return(c(list(tree = tree),
           top_bottom, 
           list(length = trunk_length, 
                height = trunk_height,
                straigtness = straightness)))
}

get_top_bottom <- function(trunk) {
  top_coords <- trunk %>%
    filter(.data$Hydroseq == max(.data$Hydroseq)) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    filter(row_number() == 1) %>%
    ungroup() %>%
    select(X, Y) %>%
    st_as_sf(coords = c("X", "Y"))
  
  bottom_coords <- trunk %>%
    filter(.data$Hydroseq == min(.data$Hydroseq)) %>%
    st_coordinates() %>%
    as.data.frame() %>%
    filter(row_number() == n()) %>%
    ungroup() %>%
    select(X, Y) %>%
    st_as_sf(coords = c("X", "Y"))
  
  list(top = top_coords, bottom = bottom_coords)
}

plot_inventory <- function(inventory) {
  paths <- pblapply(inventory, function(x) {
    id <- x$tree$LevelPathI[x$tree$Hydroseq == min(x$tree$Hydroseq)]
    f <- file.path("plots", paste0(id, ".png"))
    png(f)
    plot(st_geometry(x$tree), main = id)
    dev.off()
    f
  })
  return(as.character(paths))
}
