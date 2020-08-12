map_climatology <- function(df, var) {
  var <- sym(var)

  ggplot() +
    geom_raster(data = landmask,
                aes(lon, lat)) +
    geom_raster(data = df %>% filter(depth %in% depth_levels),
                aes(lon, lat, fill = !!var)) +
    geom_vline(
      xintercept = c(parameters$lon_Atl_section,
                     parameters$lon_Pac_section),
      col = "white"
    ) +
    coord_quickmap(expand = 0) +
    scale_fill_viridis_c() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    facet_wrap(~ depth)

}
