get_good_trees <- function(trees, good_ones) {
  good_trees <- lapply(trees, function(x, good_ones) {
    id <- x$tree$LevelPathI[x$tree$Hydroseq == min(x$tree$Hydroseq)]
    if(id %in% good_ones) {
      x
    } else {
      NULL
    }
  }, good_ones = good_ones)
  
  good_trees[!sapply(good_trees, is.null)]
}

straighten_trees <- function(trees) {
  lapply(trees, function(tree) {
    id <- tree$tree$LevelPathI[tree$tree$Hydroseq == min(tree$tree$Hydroseq)]
    
    top <- as.numeric(st_coordinates(tree$top))
    bot <- as.numeric(st_coordinates(tree$bottom))
    
    m <- (bot[2] - top[2]) / (bot[1] - top[1])
    th <- atan(m)
    
    th_desired <- (-pi/2)
    
    # So hacked up...
    if(th > 0 & th < pi/2) th <- th - pi
    
    diff <- th - th_desired
    
    if(diff > pi) {
      diff <- diff - pi
    }
    
    
    
    centroid <- st_centroid(st_union(st_geometry(tree$tree)))
    
    st_geometry(tree$tree) <- (st_geometry(tree$tree) - centroid) * rot(diff) + centroid
    
    png(file.path("plots", paste0("straight_", id, ".png")))
    plot(st_geometry(tree$tree))
    dev.off()
  
    return(tree)
  })
}

rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
