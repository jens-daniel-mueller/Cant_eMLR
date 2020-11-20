layer_inventory <- function(df, parameter) {

  parameter <- sym(parameter)

  depth_level_volume <- tibble(
    depth = unique(df$depth))

  depth_level_volume <- depth_level_volume %>%
    mutate(layer_thickness_above = replace_na((depth - lag(depth)) / 2, 0),
           layer_thickness_below = replace_na((lead(depth) - depth) / 2, 0),
           layer_thickness = layer_thickness_above + layer_thickness_below) %>%
    select(-c(layer_thickness_above,
              layer_thickness_below))

  df <- full_join(df, depth_level_volume)

  df <- df %>%
    mutate(layer_inv = !!parameter * layer_thickness) %>%
    mutate(layer_inv_pos = if_else(layer_inv < 0, 0, layer_inv)) %>%
    select(-layer_thickness)

  return(df)

}

calc_cant_inv <- function(df) {

  depth_level_volume <- tibble(
    depth = unique(df$depth)) %>%
    arrange(depth)

  depth_level_volume <- depth_level_volume %>%
    mutate(layer_thickness_above = replace_na((depth - lag(depth)) / 2, 0),
           layer_thickness_below = replace_na((lead(depth) - depth) / 2, 0),
           layer_thickness = layer_thickness_above + layer_thickness_below) %>%
    select(-c(layer_thickness_above,
              layer_thickness_below))

  df <- full_join(df, depth_level_volume)

  df <- df %>%
    filter(depth <= parameters$inventory_depth)

  df <- df %>%
    mutate(cant_layer_inv = cant * layer_thickness,
           cant_pos_layer_inv = cant_pos * layer_thickness) %>%
    select(-layer_thickness)

  df_inv <- df %>%
    group_by(lon, lat, basin_AIP, eras) %>%
    summarise(
      cant_pos_inv = sum(cant_pos_layer_inv, na.rm = TRUE) / 1000,
      cant_inv     = sum(cant_layer_inv, na.rm = TRUE) / 1000
    ) %>%
    ungroup()

  return(df_inv)

}

zonal_mean_section <- function(df) {

  zonal_mean_section <- df %>%
    select(-lon) %>%
    fgroup_by(lat, depth, eras, basin_AIP) %>% {
      add_vars(fgroup_vars(.,"unique"),
               fmean(., keep.group_vars = FALSE) %>% add_stub(pre = FALSE, "_mean"),
               fsd(., keep.group_vars = FALSE) %>% add_stub(pre = FALSE, "_sd"))
    }

  return(zonal_mean_section)

}



grid_horizontal <- function(df) {

  # cut lat and lon to a 1 x 1 deg horizontal grid

  df <- df %>%
    mutate(
      lat = cut(lat, seq(-90, 90, 1), seq(-89.5, 89.5, 1)),
      lat = as.numeric(as.character(lat)),
      lon = cut(lon, seq(20, 380, 1), seq(20.5, 379.5, 1)),
      lon = as.numeric(as.character(lon))
    )

  return(df)

}


