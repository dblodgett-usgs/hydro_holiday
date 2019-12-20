plot_card <- function(straight_trees, pick, file) {
  tree <- straight_trees[[which(sapply(straight_trees, function(x, pick) {
    x$tree$LevelPathI[x$tree$Hydroseq == min(x$tree$Hydroseq)] == pick
  }, pick = pick))]]
  
  
  trunk <- filter(tree$tree, .data$LevelPathI == min(.data$LevelPathI))
  
  top_bottom <- get_top_bottom(trunk)
  
  png(file)
  plot(st_geometry(tree$tree), col = "darkgreen", lwd = 1.5)
  plot(top_bottom$top, pch = 8, col = "gold", lwd = 3, add = TRUE)
  dev.off()
}