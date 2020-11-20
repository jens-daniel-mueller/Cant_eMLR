# define Gruber color scale

  rgb2hex <- function(r, g, b)
    rgb(r, g, b, maxColorValue = 100)

  cols = c(rgb2hex(95, 95, 95),
           rgb2hex(0, 0, 95),
           rgb2hex(100, 0, 0),
           rgb2hex(100, 100, 0))

  Gruber_rainbow <- colorRampPalette(cols)

  rm(rgb2hex, cols)



map_inventory <- function(df, var) {
  var <- sym(var)

  map +
    geom_raster(data = df,
                aes(lon, lat, fill = !!var)) +
    scale_fill_viridis_c() +
    theme(axis.title = element_blank()) +
    facet_wrap(~ eras, labeller = label_both)

}

map_inventory_divergent <- function(df, var) {
  var <- sym(var)

  max <- df %>%
    select(!!var) %>%
    pull %>%
    abs() %>%
    max()

  limits <- c(-1, 1) * max

  map +
    geom_raster(data = df,
                aes(lon, lat, fill = !!var)) +
    scale_fill_scico(palette = "vik",
                     limits = limits) +
    facet_wrap( ~ eras, labeller = label_both)

}

map_inventory_divergent_estimate <- function(df, var) {
  var <- sym(var)

  max <- df %>%
    select(!!var) %>%
    pull %>%
    abs() %>%
    max()

  limits <- c(-1, 1) * max

map +
    geom_raster(data = df,
                aes(lon, lat, fill = !!var)) +
    scale_fill_scico(palette = "vik",
                     limits = limits) +
    theme(axis.title = element_blank()) +
    facet_wrap( ~ estimate, labeller = label_both, ncol = 1)

}

map_climatology <- function(df, var) {
  var <- sym(var)

  map +
    geom_raster(data = df %>% filter(depth %in% parameters$depth_levels),
                aes(lon, lat, fill = !!var)) +
    geom_raster(data = section_global_coordinates,
               aes(lon, lat), fill = "white") +
    scale_fill_viridis_c() +
    facet_wrap( ~ depth, labeller = label_both)

}

map_climatology_divergent <- function(df, var) {
  var <- sym(var)

  df <- df %>% filter(depth %in% parameters$depth_levels)

  max <- df %>%
    select(!!var) %>%
    pull %>%
    abs() %>%
    max()

  limits <- c(-1, 1) * max

  map +
    geom_raster(data = df,
                aes(lon, lat, fill = !!var)) +
    geom_raster(data = section_global_coordinates,
               aes(lon, lat), fill = "white") +
    scale_fill_scico(palette = "vik",
                     limits = limits) +
    facet_wrap( ~ depth, labeller = label_both)

}

map_climatology_discrete <- function(df, var) {
  var <- sym(var)

map +
    geom_raster(data = df %>% filter(depth %in% parameters$depth_levels),
                aes(lon, lat, fill = !!var)) +
    geom_raster(data = section_global_coordinates,
               aes(lon, lat), fill = "white") +
    scale_fill_viridis_d() +
    theme(axis.title = element_blank()) +
    facet_wrap( ~ depth, labeller = label_both)

}

map_climatology_eras <- function(df, var) {
  var <- sym(var)

map +
    geom_raster(data = df %>% filter(depth %in% parameters$depth_levels),
                aes(lon, lat, fill = !!var)) +
    geom_raster(data = section_global_coordinates,
               aes(lon, lat), fill = "white") +
    scale_fill_viridis_c() +
    theme(axis.title = element_blank()) +
    facet_grid(eras ~ depth, labeller = label_both)

}

map_climatology_inv <- function(df, var) {
  var <- sym(var)

map +
    geom_raster(data = df,
                aes(lon, lat, fill = !!var)) +
    scale_fill_viridis_c() +
    theme(axis.title = element_blank())
}


section_climatology <- function(df, var) {
  name_var <- var
  var <- sym(var)

  df %>%
    filter(lon %in% parameters$longitude_sections_basin) %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_filled() +
    scale_fill_viridis_d(name = name_var) +
    guides(fill = guide_colorsteps(barheight = unit(7, "cm"))) +
    scale_y_reverse() +
    scale_x_continuous(breaks = seq(-80, 80, 20)) +
    coord_cartesian(expand = 0) +
    facet_wrap( ~ lon, ncol = 1, labeller = label_both) +
    theme(axis.title.x = element_blank())


}

section_climatology_regular <- function(df, var) {
  name_var <- var
  var <- sym(var)

  df %>%
    filter(lon %in% parameters$longitude_sections_regular) %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_filled() +
    scale_fill_viridis_d(name = name_var) +
    guides(fill = guide_colorsteps(barheight = unit(7, "cm"))) +
    scale_y_reverse() +
    scale_x_continuous(breaks = seq(-80, 80, 40)) +
    coord_cartesian(expand = 0) +
    facet_wrap( ~ lon, ncol = 3, labeller = label_both) +
    theme(axis.title.x = element_blank())


}

section_climatology_shallow <- function(df, var) {
  name_var <- var
  var <- sym(var)

  df %>%
    filter(
      lon %in% parameters$longitude_sections_basin,
      depth <= parameters$depth_shallow_max
    ) %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_filled() +
    scale_fill_viridis_d(name = name_var) +
    guides(fill = guide_colorsteps(barheight = unit(7, "cm"))) +
    scale_y_reverse() +
    scale_x_continuous(breaks = seq(-80, 80, 20)) +
    coord_cartesian(expand = 0) +
    facet_wrap( ~ lon, ncol = 1, labeller = label_both) +
    theme(axis.title.x = element_blank())


}


section_zonal_average_divergent <- function(df, var, gamma) {
  name_var <- var
  var <- sym(var)
  gamma <- sym(gamma)

  df %>%
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
      skip = 2
    ) +
    scale_y_reverse() +
    coord_cartesian(expand = 0) +
    guides(fill = guide_colorsteps(barheight = unit(10, "cm"))) +
    facet_grid(basin_AIP ~ eras)

}


section_zonal_layered_continous <-
  function(df,
           i_basin_AIP,
           i_estimate,
           var,
           breaks,
           legend_title) {

    name_var <- var
    var <- sym(var)

    lat_max <- max(df$lat)
    lat_min <- min(df$lat)

    breaks_n <- length(breaks) - 1

    df_sub <- df %>%
      filter(basin_AIP == i_basin_AIP,
             estimate == i_estimate)

    section <- df_sub %>%
      ggplot(aes(lat, depth, z = !!var)) +
      geom_contour_filled(breaks = breaks) +
      scale_fill_manual(values = Gruber_rainbow(breaks_n),
                        name = legend_title) +
      guides(fill = guide_colorsteps(barheight = unit(8, "cm"))) +
      scale_y_reverse() +
      scale_x_continuous(breaks = seq(-100, 100, 20))

    surface <-
      section +
      coord_cartesian(
        expand = 0,
        ylim = c(500, 0),
        xlim = c(lat_min, lat_max)
      ) +
      labs(y = "Depth (m)",
           title = paste("Basin:", i_basin_AIP, "| estimate:", i_estimate)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    deep <-
      section +
      coord_cartesian(
        expand = 0,
        ylim = c(3000, 500),
        xlim = c(lat_min, lat_max)
      ) +
      labs(x = expression(latitude~(degree*N)), y = "Depth (m)")

    surface / deep +
      plot_layout(guides = "collect")

  }

section_zonal_layered_continous_eras <-
  function(df,
           i_basin_AIP,
           i_eras,
           var,
           breaks,
           legend_title) {

    name_var <- var
    var <- sym(var)

    lat_max <- max(df$lat)
    lat_min <- min(df$lat)

    breaks_n <- length(breaks) - 1

    df_sub <- df %>%
      filter(basin_AIP == i_basin_AIP,
             eras == i_eras)

    section <- df_sub %>%
      ggplot(aes(lat, depth, z = !!var)) +
      geom_contour_filled(breaks = breaks) +
      scale_fill_manual(values = Gruber_rainbow(breaks_n),
                        name = legend_title) +
      guides(fill = guide_colorsteps(barheight = unit(8, "cm"))) +
      scale_y_reverse() +
      scale_x_continuous(breaks = seq(-100, 100, 20))

    surface <-
      section +
      coord_cartesian(
        expand = 0,
        ylim = c(500, 0),
        xlim = c(lat_min, lat_max)
      ) +
      labs(y = "Depth (m)",
           title = paste("Basin:", i_basin_AIP, "| eras:", i_eras)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    deep <-
      section +
      coord_cartesian(
        expand = 0,
        ylim = c(3000, 500),
        xlim = c(lat_min, lat_max)
      ) +
      labs(x = expression(latitude~(degree*N)), y = "Depth (m)")

    surface / deep +
      plot_layout(guides = "collect")

  }


section_zonal_layered_divergent <-
  function(df,
           i_basin_AIP,
           i_estimate,
           var,
           breaks,
           legend_title) {

    name_var <- var
    var <- sym(var)

    lat_max <- max(df$lat)
    lat_min <- min(df$lat)

    breaks_n <- length(breaks) - 1

    df_sub <- df %>%
      filter(basin_AIP == i_basin_AIP,
             estimate == i_estimate)

    section <- df_sub %>%
      ggplot(aes(lat, depth, z = !!var)) +
      geom_contour_filled(breaks = breaks) +
      scale_fill_scico_d(palette = "vik", drop = FALSE,
                         name = legend_title) +
      guides(fill = guide_colorsteps(barheight = unit(8, "cm"))) +
      scale_y_reverse() +
      scale_x_continuous(breaks = seq(-100, 100, 20))

    surface <-
      section +
      coord_cartesian(
        expand = 0,
        ylim = c(500, 0),
        xlim = c(lat_min, lat_max)
      ) +
      labs(y = "Depth (m)",
           title = paste("Basin:", i_basin_AIP, "| estimate:", i_estimate)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )

    deep <-
      section +
      coord_cartesian(
        expand = 0,
        ylim = c(3000, 500),
        xlim = c(lat_min, lat_max)
      ) +
      labs(x = expression(latitude~(degree*N)), y = "Depth (m)")

    surface / deep +
      plot_layout(guides = "collect")

  }


section_global <- function(df, var, title="Global section NAtl -> SO -> NPac") {

  name_var <- var
  var <- sym(var)

  df_sub <- left_join(section_global_coordinates, df)

  section <- df_sub %>%
    ggplot(aes(dist, depth, z = !!var)) +
    geom_contour_filled() +
    geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
               aes(xintercept = dist), col = "white") +
    scale_fill_viridis_d(name = name_var) +
    guides(fill = guide_colorsteps(barheight = unit(8, "cm"))) +
    scale_y_reverse() +
    labs(y = "Depth (m)")

  surface <-
    section +
    coord_cartesian(expand = 0,
                    ylim = c(500,0)) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(y = "Depth (m)",
         title = title)

  deep <-
    section +
    coord_cartesian(expand = 0,
                    ylim = c(parameters$plotting_depth, 500)) +
    labs(x = "Distance (Mm)", y = "Depth (m)")

  surface / deep +
    plot_layout(guides = "collect")

}


section_global_surface <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df_sub <- left_join(section_global_coordinates, df)

  df_sub %>%
    ggplot(aes(dist, depth, z = !!var)) +
    geom_contour_filled() +
    geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
               aes(xintercept = dist), col = "white") +
    scale_fill_viridis_d(name = name_var) +
    coord_cartesian(expand = 0) +
    scale_y_reverse() +
    theme(legend.position = "top") +
    labs(y = "Depth (m)")

}

section_global_surface_eras <- function(df, var) {

  name_var <- var
  var <- sym(var)

  df_sub <- left_join(section_global_coordinates, df)

  df_sub %>%
    filter(!is.na(eras)) %>%
    ggplot(aes(dist, depth, z = !!var)) +
    geom_contour_filled() +
    geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
               aes(xintercept = dist), col = "white") +
    scale_fill_viridis_d(name = name_var) +
    coord_cartesian(expand = 0) +
    scale_y_reverse() +
    theme(legend.position = "top") +
    labs(y = "Depth (m)") +
    facet_grid(eras ~ .)

}

