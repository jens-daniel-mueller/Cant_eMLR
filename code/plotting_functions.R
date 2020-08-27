map_climatology <- function(df, var) {

  var <- sym(var)

  ggplot() +
    geom_raster(data = landmask %>% filter(region == "land"),
                aes(lon, lat), fill = "grey80") +
    geom_raster(data = df %>% filter(depth %in% parameters$depth_levels),
                aes(lon, lat, fill = !!var)) +
    geom_vline(
      xintercept = c(parameters$lon_Atl_section,
                     parameters$lon_Pac_section,
                     parameters$lon_Ind_section),
      col = "white"
    ) +
    coord_quickmap(expand = 0) +
    scale_fill_viridis_c() +
    theme(
      axis.title = element_blank()
    ) +
    facet_wrap(~ depth)

}

map_climatology_inv <- function(df, var) {

  var <- sym(var)

  ggplot() +
    geom_raster(data = landmask %>% filter(region == "land"),
                aes(lon, lat), fill = "grey80") +
    geom_raster(data = df,
                aes(lon, lat, fill = !!var)) +
    coord_quickmap(expand = 0) +
    scale_fill_viridis_c() +
    theme(
      axis.title = element_blank()
    )
}

section_climatology <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df %>%
    filter(lon %in% parameters$longitude_sections_basin,
           depth <= parameters$inventory_depth) %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_filled() +
    scale_fill_viridis_d(name = name_var) +
    guides(fill = guide_colorsteps(barheight = unit(7, "cm"))) +
    scale_y_reverse() +
    scale_x_continuous(breaks = seq(-80,80,20)) +
    coord_cartesian(expand = 0) +
    facet_wrap(~lon, ncol = 1)


}

section_climatology_regular <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df %>%
    filter(lon %in% parameters$longitude_sections_regular,
           depth <= parameters$inventory_depth) %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_filled() +
    scale_fill_viridis_d(name = name_var) +
    guides(fill = guide_colorsteps(barheight = unit(7, "cm"))) +
    scale_y_reverse() +
    scale_x_continuous(breaks = seq(-80,80,40)) +
    coord_cartesian(expand = 0) +
    facet_wrap(~lon, ncol = 3)


}

section_climatology_shallow <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df %>%
    filter(lon %in% parameters$longitude_sections_basin,
           depth <= parameters$depth_shallow_max) %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_filled() +
    scale_fill_viridis_d(name = name_var) +
    guides(fill = guide_colorsteps(barheight = unit(7, "cm"))) +
    scale_y_reverse() +
    scale_x_continuous(breaks = seq(-80,80,20)) +
    coord_cartesian(expand = 0) +
    facet_wrap(~lon, ncol = 1)


}


section_zonal_average_divergent <- function(df, var, gamma) {

  name_var <- var
  var <- sym(var)
  gamma <- sym(gamma)

  df %>%
    filter(depth <= parameters$inventory_depth) %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_fill(breaks = MakeBreaks(5),
                      na.fill = TRUE) +
    scale_fill_divergent(guide = "colorstrip",
                         breaks = MakeBreaks(5),
                         name = name_var) +
    geom_contour(aes(lat, depth, z = !!gamma),
                 breaks = slab_breaks,
                 col = "black") +
    geom_text_contour(
      aes(lat, depth, z = !!gamma),
      breaks = slab_breaks,
      col = "black",
      skip = 1
    ) +
    scale_y_reverse() +
    coord_cartesian(expand = 0) +
    guides(fill = guide_colorsteps(barheight = unit(10, "cm"))) +
    facet_grid(basin_AIP ~ eras)

}


section_zonal_average_continous <- function(df, var, gamma) {

  name_var <- var
  var <- sym(var)
  gamma <- sym(gamma)

  df %>%
    filter(depth <= parameters$inventory_depth) %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_filled() +
    scale_fill_viridis_d(name = name_var) +
    geom_contour(aes(lat, depth, z = !!gamma),
                 breaks = slab_breaks,
                 col = "white") +
    geom_text_contour(
      aes(lat, depth, z = !!gamma),
      breaks = slab_breaks,
      col = "white",
      skip = 1
    ) +
    scale_y_reverse() +
    coord_cartesian(expand = 0) +
    guides(fill = guide_colorsteps(barheight = unit(10, "cm"))) +
    facet_grid(basin_AIP ~ eras)

}
