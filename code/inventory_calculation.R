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
