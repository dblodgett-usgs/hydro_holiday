plot_card <- function(straight_trees, pick, file) {
  tree <- straight_trees[[which(sapply(straight_trees, function(x, pick) {
    x$tree$LevelPathI[x$tree$Hydroseq == min(x$tree$Hydroseq)] == pick
  }, pick = pick))]]
  
  trunk <- filter(tree$tree, .data$LevelPathI == min(.data$LevelPathI))
  
  top_bottom <- get_top_bottom(trunk)
  
  trunk <- filter(trunk, .data$StreamOrde > 2)
  
  trunk_bottom <- arrange(trunk, Hydroseq)[1:40, ]
  lower_branch_roots <-filter(tree$tree, .data$DnHydroseq %in% trunk_bottom$Hydroseq &
                                !.data$Hydroseq %in% trunk_bottom$Hydroseq &
                                !.data$LevelPathI %in% trunk_bottom$LevelPathI)
  lower_branches <- do.call(c, lapply(lower_branch_roots$COMID, 
                                      function(x, network) get_UT(network, x), 
                                      network = tree$tree)) %>%
    unique()
  
  main_branch_roots <- filter(tree$tree, .data$DnHydroseq %in% trunk$Hydroseq &
                            !.data$COMID %in% trunk$Hydroseq)
  main_branches <- filter(tree$tree, .data$LevelPathI %in% main_branch_roots$LevelPathI &
                            .data$StreamOrde > 1 &
                            !.data$COMID %in% c(trunk$COMID, lower_branches))
  
  branches <- filter(tree$tree, 
                     !.data$COMID %in% c(main_branches$COMID, trunk$COMID, 
                                         lower_branches))
  
  all_points <- rbind(st_coordinates(branches), st_coordinates(main_branches))
  
  ornaments <- get_lights(all_points, 500, 3500, tree)
  blue_lights <- get_lights(all_points, 500, 3000, tree)
  green_lights <- get_lights(all_points, 500, 3000, tree)
  yellow_lights <- get_lights(all_points, 500, 3000, tree)
  colors <- list(list("blue", "green", "red"),
                 list("red", "green", "blue"),
                 list("green", "blue", "red"),
                 list("blue", "red", "green"))
  gifski::save_gif(expr = {
    for(i in 1:4) {
      par(mar = c(0,0,0,0))
      plot(st_geometry(tree$tree), col = NA)
      usr <- par()$usr
      par(lheight = 1.25)
      text(usr[1] + (usr[2] - usr[1]) / 5, usr[4] - (usr[4] - usr[3]) / 3,
           paste("As we get ready to gather", "with friends and family to have",
                 "a party, give a gift, and", "celebrate the new decade,",
                 "it's important to remember", "to have fun, be silly, and",
                 "to not take things too seriously.",
                 sep = "\n"), cex = 1.9)
      text(usr[1] + (usr[2] - usr[1]) / 1.3, usr[4] - (usr[4] - usr[3]) / 2,
           paste("This card was created by", "searching NHDPlusV2 data",
                 "for river networks with", "the right characteristics",
                 "then straigtening them up", "and selecting 'tree' parts",
                 "from the resulting network.", "Code available from:",
                 "github.com/dblodgett-usgs/hydro_holiday",
                 "This is the San Juan River in CO.",
                 sep = "\n"), cex = 1.7)
      par(lheight = 1)
      text(usr[1] + (usr[2] - usr[1]) / 5, usr[4] - (usr[4] - usr[3]) / 1.3,
           paste("Wishing all my friends", "a happy holidays!!",
                 sep = "\n"), cex = 2.8)
      plot(st_geometry(trunk), col = "brown", lwd = trunk$StreamOrde / 1.5, add = TRUE)
      plot(st_geometry(main_branches), col = "sandybrown", 
           lwd = main_branches$StreamOrde / 2, add = TRUE)
      plot(st_geometry(branches), col = "darkgreen", lwd = 1.5,  add = TRUE)
      plot(st_geometry(blue_lights), pch = 20, cex = 0.7, col = colors[[i]][[1]], add = TRUE)
      plot(st_geometry(green_lights), pch = 20, cex = 0.7, col = colors[[i]][[2]], add = TRUE)
      plot(st_geometry(yellow_lights), pch = 20, cex = 0.7, col = colors[[i]][[3]], add = TRUE)
      plot(st_geometry(ornaments), pch = 20, cex = 2, col = "gold", add = TRUE) 
      plot(top_bottom$top, pch = 8, col = "gold", lwd = 3, cex = 2, add = TRUE)
    }
  }, gif_file = file, width = 1024, height = 512, delay = 0.5, loop = TRUE)
  
  png("context.png")
  plot_nhdplus(list("comid", as.character(trunk_bottom[1,]$COMID)), 
               nhdplus_data = "plots/temp.gpkg", gpkg = "plots/temp.gpkg")
  dev.off
  
}

get_lights <- function(points, num_lights, min_distance, tree) {
  lights <- points[sample(seq_len(nrow(points)), num_lights), ] %>%
    data.frame()
  lights <- st_as_sf(lights, coords = c("X", "Y"), crs = st_crs(tree$tree))
  
  distances <- st_distance(lights)
  remove <- (distances < min_distance & distances != 0)
  while(any(remove)) {
    remove <- unique(which(remove, arr.ind = TRUE)[, 1])
    if(length(remove) > 4) {
      remove <- sample(remove, round((length(remove) / 4)))
    }
    rows <- seq_len(nrow(lights))
    rows <- rows[!rows %in% remove]
    lights <- lights[rows, ]
    distances <- st_distance(lights)
    remove <- (distances < min_distance & distances != 0)
  }
  lights
}
  
  
  
  
