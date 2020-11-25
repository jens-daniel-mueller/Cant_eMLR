# Gruber color scale

rgb2hex <- function(r, g, b)
  rgb(r, g, b, maxColorValue = 100)

cols = c(rgb2hex(95, 95, 95),
         rgb2hex(0, 0, 95),
         rgb2hex(100, 0, 0),
         rgb2hex(100, 100, 0))

p_gruber_rainbow <- colorRampPalette(cols)

rm(rgb2hex, cols)


# Global section with continous color scale
p_section_global_continous <-
  function(df,
           var,
           title_text = "Global section",
           subtitle_text = "N-Atl -> SO -> N-Pac") {
    name_var <- var
    var <- sym(var)

    df_sub <- left_join(section_global_coordinates, df)

    section <- df_sub %>%
      ggplot(aes(dist, depth, z = !!var)) +
      geom_contour_filled() +
      geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
                 aes(xintercept = dist),
                 col = "white") +
      scale_fill_viridis_d(name = name_var) +
      guides(fill = guide_colorsteps(barheight = unit(8, "cm"))) +
      scale_y_reverse() +
      labs(y = "Depth (m)")

    surface <-
      section +
      coord_cartesian(expand = 0,
                      ylim = c(500, 0)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(y = "Depth (m)")

    deep <-
      section +
      coord_cartesian(expand = 0,
                      ylim = c(parameters$plotting_depth, 500)) +
      labs(x = "Distance (Mm)", y = "Depth (m)")

    surface / deep +
      plot_layout(guides = "collect") +
      plot_annotation(title = title_text,
                      subtitle = subtitle_text)

  }


# Global section with continous divergent scale
p_section_global_divergent <-
  function(df,
           var,
           title_text = "Global section",
           subtitle_text = "N-Atl -> SO -> N-Pac | color range 99th percentile") {
    name_var <- var
    var <- sym(var)

    df_sub <- left_join(section_global_coordinates, df)

    max <- df %>%
      select(!!var) %>%
      pull %>%
      abs() %>%
      quantile(0.99)

    breaks = seq(-max, max, length.out = 20)

    section <- df_sub %>%
      ggplot(aes(dist, depth, z = !!var)) +
      geom_contour_filled(breaks = breaks) +
      geom_vline(data = section_global_coordinates %>% filter(lat == 0.5),
                 aes(xintercept = dist),
                 col = "white") +
      scale_fill_scico_d(palette = "vik", drop = FALSE,
                         name = name_var) +
      guides(fill = guide_colorsteps(barheight = unit(8, "cm"))) +
      scale_y_reverse() +
      labs(y = "Depth (m)")

    surface <-
      section +
      coord_cartesian(expand = 0,
                      ylim = c(500, 0)) +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      ) +
      labs(y = "Depth (m)")

    deep <-
      section +
      coord_cartesian(expand = 0,
                      ylim = c(parameters$plotting_depth, 500)) +
      labs(x = "Distance (Mm)", y = "Depth (m)")

    surface / deep +
      plot_layout(guides = "collect") +
      plot_annotation(title = title_text,
                      subtitle = subtitle_text)

  }

# plot sections at lon intervals with continuous color scale
p_section_climatology_regular_continous <- function(df, var) {
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

# plot sections at lon intervals with divergent color scale
p_section_climatology_regular_divergent <-
  function(df,
           var,
           title_text = "Latitudinal sections",
           subtitle_text = "Color range 99th percentile") {
    name_var <- var
    var <- sym(var)

    max <- df %>%
      select(!!var) %>%
      pull %>%
      abs() %>%
      quantile(0.99)

    breaks = seq(-max, max, length.out = 20)

    df %>%
      filter(lon %in% parameters$longitude_sections_regular) %>%
      ggplot(aes(lat, depth, z = !!var)) +
      geom_contour_filled(breaks = breaks) +
      scale_fill_scico_d(palette = "vik",
                         drop = FALSE,
                         name = name_var) +
      guides(fill = guide_colorsteps(barheight = unit(7, "cm"))) +
      scale_y_reverse() +
      scale_x_continuous(breaks = seq(-80, 80, 40)) +
      coord_cartesian(expand = 0) +
      facet_wrap(~ lon, ncol = 3, labeller = label_both) +
      theme(axis.title.x = element_blank())
  }


# Zonal mean section with continuous color scale, default for pos cant estimates
p_section_zonal_continous <-
  function(df,
           var,
           breaks = parameters$breaks_cant_pos,
           legend_title = expression(atop(Delta * C[ant],
                                          (mu * mol ~ kg ^ {-1}))),
           title_text = "Zonal mean section",
           subtitle_text = "") {

    name_var <- var
    var <- sym(var)

    lat_max <- max(df$lat)
    lat_min <- min(df$lat)

    breaks_n <- length(breaks) - 1

    section <- df %>%
      ggplot(aes(lat, depth, z = !!var)) +
      geom_contour_filled(breaks = breaks) +
      scale_fill_manual(values = p_gruber_rainbow(breaks_n),
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
           title = title_text,
           subtitle = subtitle_text) +
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



# Zonal mean section with continuous color scale, default for pos cant estimates
p_section_zonal_continous_gamma <-
  function(df,
           var,
           gamma,
           breaks = parameters$breaks_cant_pos,
           legend_title = expression(atop(Delta * C[ant],
                                          (mu * mol ~ kg ^ {-1}))),
           title_text = "Zonal mean section",
           subtitle_text = "") {

    name_var <- var
    var <- sym(var)
    gamma <- sym(gamma)

    lat_max <- max(df$lat)
    lat_min <- min(df$lat)

    breaks_n <- length(breaks) - 1

    section <- df %>%
      ggplot(aes(lat, depth, z = !!var)) +
      geom_contour_filled(breaks = breaks) +
      geom_contour(aes(lat, depth, z = !!gamma),
                   breaks = slab_breaks,
                   col = "black") +
      geom_text_contour(
        aes(lat, depth, z = !!gamma),
        breaks = slab_breaks,
        col = "black",
        skip = 1
      ) +
      scale_fill_manual(values = p_gruber_rainbow(breaks_n),
                        name = legend_title,
                        drop = FALSE) +
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
           title = title_text,
           subtitle = subtitle_text) +
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


# Zonal mean section with divergent color scale, default for all cant estimates
p_section_zonal_divergent <-
  function(df,
           var,
           breaks = parameters$breaks_cant,
           legend_title = expression(atop(Offset~Delta * C[ant],
                                          (mu * mol ~ kg ^ {-1}))),
           title_text = "Zonal mean section",
           subtitle_text = "") {

    name_var <- var
    var <- sym(var)

    lat_max <- max(df$lat)
    lat_min <- min(df$lat)

    breaks_n <- length(breaks) - 1

    section <- df %>%
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
           title = title_text,
           subtitle = subtitle_text) +
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


# Zonal mean section with divergent color scale, default for all cant estimates
# plot neutral density isolines overlay

p_section_zonal_divergent_gamma <-
  function(df,
           var,
           gamma,
           breaks = parameters$breaks_cant,
           legend_title = expression(atop(Offset~Delta * C[ant],
                                          (mu * mol ~ kg ^ {-1}))),
           title_text = "Zonal mean section",
           subtitle_text = "") {

    name_var <- var
    var <- sym(var)
    gamma <- sym(gamma)

    lat_max <- max(df$lat)
    lat_min <- min(df$lat)

    breaks_n <- length(breaks) - 1

    section <- df %>%
      ggplot() +
      geom_contour_filled(aes(lat, depth, z = !!var),
                          breaks = breaks) +
      geom_contour(aes(lat, depth, z = !!gamma),
                   breaks = slab_breaks,
                   col = "black") +
      geom_text_contour(
        aes(lat, depth, z = !!gamma),
        breaks = slab_breaks,
        col = "black",
        skip = 1
      ) +
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
           title = title_text,
           subtitle = subtitle_text) +
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

p_section_zonal_divergent_gamma_eras_basin <- function(df, var, gamma) {
  name_var <- var
  var <- sym(var)
  gamma <- sym(gamma)

  max <- df %>%
    select(!!var) %>%
    pull %>%
    abs() %>%
    max()

  breaks = seq(-max, max, length.out = 20)

  df %>%
    ggplot(aes(lat, depth, z = !!var)) +
    geom_contour_filled(breaks = breaks) +
    scale_fill_scico_d(palette = "vik",
                       drop = FALSE) +
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


# plot column inventory map of pos cant estimate
p_map_cant_pos_inv <-
  function(df,
           var = "cant_pos_inv",
           breaks = parameters$breaks_cant_pos_inv,
           title_text = "Column inventory map",
           subtitle_text = "era: JGOFS/WOCE - GO-SHIP") {

    var <- sym(var)

    breaks_n <- length(breaks) - 1

    df <- df %>%
      mutate(var_int = cut(!!var,
                           breaks,
                           right = FALSE))
    map +
      geom_raster(data = df,
                  aes(lon, lat, fill = var_int)) +
      scale_fill_manual(values = p_gruber_rainbow(breaks_n),
                        name = expression(atop(Delta * C["ant,pos"],
                                               (mol ~ m ^ {
                                                 -2
                                               })))) +
      # scale_fill_scico_d(palette = "batlow", direction = -1) +
      guides(fill = guide_colorsteps(barheight = unit(6, "cm"))) +
      labs(title = title_text,
           subtitle = subtitle_text)

  }


# plot map of mean pos cant within density slab
p_map_cant_pos_slab <-
  function(df,
           var = "cant_pos",
           breaks = parameters$breaks_cant_pos_inv,
           legend_title = expression(atop(Delta * C[ant],
                                          (mu * mol ~ kg ^ {-1}))),
           title_text = "Isoneutral slab concentration map",
           subtitle_text = "era: JGOFS/WOCE - GO-SHIP") {

    var <- sym(var)
    breaks_n <- length(breaks) - 1

    df <- df %>%
      mutate(var_int = cut(!!var,
                           breaks,
                           right = FALSE))
    map +
      geom_raster(data = df,
                  aes(lon, lat, fill = var_int)) +
      scale_fill_manual(values = p_gruber_rainbow(breaks_n),
                        name = legend_title,
                        drop = FALSE) +
      # scale_fill_scico_d(palette = "batlow", direction = -1) +
      guides(fill = guide_colorsteps(barheight = unit(6, "cm"))) +
      labs(title = title_text,
           subtitle = subtitle_text)

  }


# plot column inventory map of cant incl. negative estimates
p_map_cant_inv <-
  function(df,
           breaks = parameters$breaks_cant_inv,
           title_text = "Column inventory map",
           subtitle_text = "era: JGOFS/WOCE - GO-SHIP") {

    map +
      geom_raster(data = df,
                  aes(lon, lat, fill = cut(cant_inv, breaks))) +
      scale_fill_scico_d(palette = "vik",
                         drop = FALSE,
                         name = expression(atop(Delta * C[ant],
                                                (mu * mol ~ kg ^ {-1})))) +
      guides(fill = guide_colorsteps(barheight = unit(6, "cm")))  +
      labs(title = title_text,
           subtitle = subtitle_text)

  }


# plot column inventory map of cant incl. negative estimates
p_map_cant_slab <-
  function(df,
           breaks = parameters$breaks_cant_inv,
           legend_title = expression(atop(Delta * C[ant],
                                          (mu * mol ~ kg ^ {-1}))),
           title_text = "Isoneutral slab concentration map",
           subtitle_text = "era: JGOFS/WOCE - GO-SHIP") {

    map +
      geom_raster(data = df,
                  aes(lon, lat, fill = cut(cant, breaks))) +
      scale_fill_scico_d(palette = "vik",
                         drop = FALSE,
                         name = expression(atop(Delta * C[ant],
                                                (mu * mol ~ kg ^ {-1})))) +
      guides(fill = guide_colorsteps(barheight = unit(6, "cm")))  +
      labs(title = title_text,
           subtitle = subtitle_text)

  }

# plot column inventory map of cant offset
p_map_cant_inv_offset <-
  function(df,
           var,
           breaks = parameters$breaks_cant_inv_offset,
           title_text = "Column inventory map - offset",
           subtitle_text = "era: JGOFS/WOCE - GO-SHIP") {

    var <- sym(var)

    map +
      geom_raster(data = df,
                  aes(lon, lat, fill = cut(!!var, breaks))) +
      scale_fill_scico_d(palette = "vik", drop = FALSE,
                         name = expression(atop(Offset~Delta*C[ant],
                                                (mol~m^{-2})))) +
      guides(fill = guide_colorsteps(barheight = unit(6, "cm"))) +
      labs(title = title_text,
           subtitle = subtitle_text)

  }


# Maps at predefined depth layers with continuous color scale
p_map_climatology_continous <-
  function(df,
           var,
           title_text = "Distribution maps",
           subtitle_text = "at predefined depth levels") {
    var <- sym(var)

    df <- df %>%
      filter(depth %in% parameters$depth_levels)

    map +
      geom_raster(data = df,
                  aes(lon, lat, fill = !!var)) +
      geom_raster(data = section_global_coordinates,
                  aes(lon, lat), fill = "white") +
      scale_fill_viridis_c() +
      facet_wrap( ~ depth, labeller = label_both) +
      labs(title = title_text,
           subtitle = subtitle_text)

  }

# Maps at predefined depth layers with divergent color scale
p_map_climatology_divergent <-
  function(df,
           var,
           title_text = "Distribution maps",
           subtitle_text = "at predefined depth levels") {
    var <- sym(var)

    df <- df %>%
      filter(depth %in% parameters$depth_levels)

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
      scale_fill_scico(
        palette = "vik",
        limit = limits
      ) +
      facet_wrap( ~ depth, labeller = label_both) +
      labs(title = title_text,
           subtitle = subtitle_text)

  }

