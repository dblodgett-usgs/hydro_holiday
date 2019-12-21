library(drake)
library(sf)
library(dplyr)
library(nhdplusTools)
library(pbapply)
library(gifski)

source("R/get_trees.R")
source("R/review_trees.R")

nhdplus_path("data/nhdp/NHDPlusNationalData/NHDPlusV21_National_Seamless.gdb/")

min_tree_size <- 100
max_tree_size <- 400
min_tree_volume <- 100

plan <- drake_plan(forest = readRDS(stage_national_data()$flowline),
                   candidates = get_candidates(forest),
                   trees = find_trees(candidates,
                                      height = list(min = min_tree_size, 
                                                    max = max_tree_size),
                                      num_branches = list(min = 15, max = 50)),
                   inventory = review_trees(trees, candidates),
                   inventory_plots = plot_inventory(inventory),
                   gif = gifski(inventory_plots, "candidate_trees.gif", delay = 0.2, progress = TRUE))

make(plan)

# After flipping through...

source("R/gussy_up.R")

plan <- bind_rows(
  plan,
  drake_plan(tree_attributes = readRDS(stage_national_data()$attributes),
             good_ones = c(10009766, 90011521, 290008836, 390020157, 
                            550025262, 550027957, 590024987, 590029190, 
                            590029958, 630008081, 760005323),
             good_trees = get_good_trees(inventory, good_ones, 
                                         tree_attributes),
             straight_trees = straighten_trees(good_trees),
             gif2 = gifski(list.files("plots", 
                                      pattern = "^straight_.*", 
                                      full.names = TRUE), 
                           gif_file = "straightened_trees.gif", 
                           delay = 1)))

make(plan)

pick <- 760005323

source("R/plot_card.R")

plan <- bind_rows(plan, 
                  drake_plan(pc = plot_card(straight_trees, 
                                            pick, "hydro_holiday.gif")))

make(plan)

