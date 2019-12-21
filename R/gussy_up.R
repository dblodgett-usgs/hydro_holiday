get_good_trees <- function(trees, good_ones, tree_atts) {
  good_trees <- lapply(trees, function(x, good_ones, tree_atts) {
    id <- x$tree$LevelPathI[x$tree$Hydroseq == min(x$tree$Hydroseq)]
    if(id %in% good_ones) {
      x$tree <- x$tree  %>%
        select(.data$COMID) %>%
        left_join(tree_atts, by = "COMID")
      x
    } else {
      NULL
    }
  }, good_ones = good_ones, tree_atts = tree_atts)
  
  good_trees[!sapply(good_trees, is.null)]
}

straighten_trees <- function(trees) {
  lapply(trees, function(tree) {
    id <- tree$tree$LevelPathI[tree$tree$Hydroseq == min(tree$tree$Hydroseq)]
    
    top <- as.numeric(st_coordinates(tree$top))
    bot <- as.numeric(st_coordinates(tree$bottom))
    
    th <- atan2((bot[2] - top[2]), (bot[1] - top[1]))
    
    th_desired <- (-pi/2)
    
    diff <- th - th_desired
    
    centroid <- st_centroid(st_union(st_geometry(tree$tree)))
    
    st_geometry(tree$tree) <- (st_geometry(tree$tree) - centroid) * rot(diff) + centroid
    
    png(file.path("plots", paste0("straight_", id, ".png")))
    plot(st_geometry(tree$tree))
    dev.off()
  
    return(tree)
  })
}

rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
