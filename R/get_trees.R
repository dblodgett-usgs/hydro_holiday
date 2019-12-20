find_trees <- function(fl, height, num_branches) {
  fl <- fl %>%
    st_drop_geometry() %>%
    group_by(LevelPathI) %>%
    mutate(trunk_length = sum(.data$LENGTHKM)) %>%
    ungroup()
  
  branches <- fl %>%
    select(.data$LevelPathI, .data$DnLevelPat) %>%
    filter(.data$DnLevelPat != .data$LevelPathI) %>%
    distinct() %>%
    group_by(.data$DnLevelPat) %>%
    mutate(branches = n()) %>%
    select(-LevelPathI, LevelPathI = .data$DnLevelPat) %>%
    distinct()
  
  fl %>%
    filter(.data$trunk_length > height$min & .data$trunk_length < height$max) %>%
    left_join(branches, by = "LevelPathI") %>%
    select(.data$LevelPathI, trunk_length, branches) %>%
    distinct() %>%
    filter(.data$branches > num_branches$min & .data$branches < num_branches$max)
}

get_candidates <- function(forest) {
  prepare_nhdplus(forest, 
                  min_network_size = 0, 
                  min_path_length = 0, 
                  min_path_size = 0, 
                  purge_non_dendritic = TRUE) %>%
    select(COMID) %>%
    left_join(forest, by = "COMID") %>%
    st_sf() %>%
    st_transform(5070) %>%
    st_simplify(dTolerance = 100)
}