#' @rdname ggproto-classes
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
GeomDumbbellSizeText <- 
  ggproto("GeomDumbbellSizeText", Geom,
          required_aes = c("x", "xend", "y", "size_x", "size_xend"),
          non_missing_aes = c("shape", "colour_x", "colour_xend",
                              "dot_guide", "dot_guide_size", 
                              "dot_guide_colour"),
          default_aes = aes(
            shape = 19, colour = "black", fill = NA, size = 0.5,
            alpha = NA, stroke = 0.5, size_x = 1, size_xend = 1,
            text_colour = "black", family = '', fontface = 1
          ),
          
          setup_data = function(data, params) {
            data <- mutate(data, diff = abs(x - xend)*100,
                           range = `-`(range(x, xend, na.rm = TRUE)[2], 
                                       range(x, xend, na.rm = TRUE)[1])*100,
                           diffrange = diff/range, 
                           alpha = ifelse(diffrange < 0.25, 0.5, 1),
                           yend = y)
            
            print(data)
          },
          
          draw_group = function(data, panel_scales, coord,
                                size_x = NULL, size_xend = NULL,
                                colour_x = NULL, colour_xend = NULL,
                                dot_guide = NULL, dot_guide_size = NULL,
                                dot_guide_colour = NULL) {
            
            points.x <- data
            points.x$colour <- colour_x %||% data$colour
            points.x$xend <- NULL
            points.x$size_x <- NULL
            points.x$size_xend <- NULL
            points.x$size <- (data$size_x * 1.2) # %||% size_x
            
            points.xend <- data
            points.xend$x <- points.xend$xend
            points.xend$xend <- NULL
            points.xend$size_x <- NULL
            points.xend$size_xend <- NULL
            points.xend$colour <- colour_xend %||% data$colour
            points.xend$size <- (data$size_xend * 1.2)# %||% size_xend
            
            dot_df <- data
            dot_df$alpha <- NULL
            dot_df$size_x <- NULL
            dot_df$size_xend <- NULL
            dot_df$xend <- ifelse(data$xend < data$x, data$xend, data$x)
            dot_df$x <- -Inf
            dot_df$linetype <- "11"
            # dot_df$size <- dot_guide_size %||% (data$size * 0.5)
            dot_df$size <- dot_guide_size %||% 1
            dot_df$colour <- dot_guide_colour %||% "#5b5b5b"
            
            text.x <- data
            text.x$label <- ifelse(data$alpha == 1, round(data$x, 2), '')
            text.x$size <- data$size_x*0.55
            text.x$xend <- NULL
            text.x$size_x <- NULL
            text.x$size_xend <- NULL
            text.x$diff <- NULL
            text.x$range <- NULL
            text.x$diffrange <- NULL
            text.x$colour <- data$text_colour %||% '#f5f5f2'
            text.x$angle <- 0
            
            text.xend <- data
            text.xend$label <- ifelse(data$alpha == 1, round(data$xend, 2), '')
            text.xend$size <- data$size_xend*0.55
            text.xend$x <- data$xend
            text.xend$xend <- NULL
            text.xend$size_x <- NULL
            text.xend$size_xend <- NULL
            text.xend$diff <- NULL
            text.xend$range <- NULL
            text.xend$diffrange <- NULL
            text.xend$colour <- data$text_colour %||% '#f5f5f2'
            text.xend$angle <- 0
            
            grid::gList(
              ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
              ggplot2::GeomPoint$draw_panel(points.x, panel_scales, coord),
              ggplot2::GeomPoint$draw_panel(points.xend, panel_scales, coord),
              ggplot2::GeomText$draw_panel(text.x, panel_scales, coord),
              ggplot2::GeomText$draw_panel(text.xend, panel_scales, coord)
            )
          },
          
          draw_key = draw_key_point
  )

#' @rdname ggproto-classes
#' @format NULL
#' @usage NULL
#' @export
GeomDumbbellSize <- 
  ggproto("GeomDumbbellSize", Geom,
          required_aes = c("x", "xend", "y", "size_x", "size_xend"),
          non_missing_aes = c("shape", "colour_x", "colour_xend",
                              "dot_guide", "dot_guide_size", 
                              "dot_guide_colour"),
          default_aes = aes(
            shape = 19, colour = "black", fill = NA, size = 0.5,
            alpha = NA, stroke = 0.5, size_x = 1, size_xend = 1
          ),
          
          setup_data = function(data, params) {
            data <- mutate(data, diff = abs(x - xend)*100,
                           range = `-`(range(x, xend, na.rm = TRUE)[2], 
                                       range(x, xend, na.rm = TRUE)[1])*100,
                           diffrange = diff/range, 
                           alpha = ifelse(diffrange < 0.25, 0.5, 1),
                           yend = y)
            
            print(data)
          },
          
          draw_group = function(data, panel_scales, coord,
                                size_x = NULL, size_xend = NULL,
                                colour_x = NULL, colour_xend = NULL,
                                dot_guide = NULL, dot_guide_size = NULL,
                                dot_guide_colour = NULL) {
            
            points.x <- data
            points.x$colour <- colour_x %||% data$colour
            points.x$xend <- NULL
            points.x$size_x <- NULL
            points.x$size_xend <- NULL
            points.x$size <- (data$size_x * 1.2) # %||% size_x
            
            points.xend <- data
            points.xend$x <- points.xend$xend
            points.xend$xend <- NULL
            points.xend$size_x <- NULL
            points.xend$size_xend <- NULL
            points.xend$colour <- colour_xend %||% data$colour
            points.xend$size <- (data$size_xend * 1.2)# %||% size_xend
            
            dot_df <- data
            dot_df$alpha <- NULL
            dot_df$size_x <- NULL
            dot_df$size_xend <- NULL
            dot_df$xend <- ifelse(data$xend < data$x, data$xend, data$x)
            dot_df$x <- -Inf
            dot_df$linetype <- "11"
            # dot_df$size <- dot_guide_size %||% (data$size * 0.5)
            dot_df$size <- dot_guide_size %||% 1
            dot_df$colour <- dot_guide_colour %||% "#5b5b5b"
            
            grid::gList(
              ggplot2::GeomSegment$draw_panel(data, panel_scales, coord),
              ggplot2::GeomPoint$draw_panel(points.x, panel_scales, coord),
              ggplot2::GeomPoint$draw_panel(points.xend, panel_scales, coord)
            )
          },
          
          draw_key = draw_key_point
  )

#' Display dumbbell plots with sized and labelled dumbbell ends.
#' 
#' A complete rip-off of the geom_dumbbell function from hrbrmstr/ggalt, but
#' with: 1) size_x and size_xend aesthetics (unlike in the original 
#' geom_dumbbell, where the sizes are parameters) so that you can size the dots,
#' for example by the relative number of observations in each group; 
#' 2) labels on the dots. so far, the labels correspond to the values of the
#' variable on the x-axis, rounded to 2 decimal points.
#' @inheritParams ggplot2::layer
#' @inheritParams ggalt::geom_dumbbell
#' @export
geom_dumbbell_size_text <- function(mapping = NULL, data = NULL, ...,
                                    colour_x = NULL, size_x = NULL,
                                    colour_xend = NULL, size_xend = NULL,
                                    dot_guide = FALSE, dot_guide_size = NULL,
                                    dot_guide_colour = NULL,
                                    na.rm = FALSE, show.legend = NA, 
                                    inherit.aes = TRUE, position = "identity") {
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomDumbbellSizeText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      colour_x = colour_x,
      colour_xend = colour_xend,
      dot_guide = dot_guide,
      dot_guide_size = dot_guide_size,
      dot_guide_colour = dot_guide_colour,
      ...
    )
  )
}

#' Display dumbbell plots with sized dumbbell ends.
#' 
#' A complete rip-off of the geom_dumbbell function from hrbrmstr/ggalt, but
#' with extra size_x and size_xend aesthetics (unlike in the original 
#' geom_dumbbell, where the sizes are parameters) so that you can size the dots,
#' for example by the relative number of observations in each group.
#' @inheritParams ggplot2::layer
#' @inheritParams ggalt::geom_dumbbell
#' @export
geom_dumbbell_size <- function(mapping = NULL, data = NULL, ...,
                               colour_x = NULL, size_x = NULL,
                               colour_xend = NULL, size_xend = NULL,
                               dot_guide = FALSE, dot_guide_size = NULL,
                               dot_guide_colour = NULL,
                               na.rm = FALSE, show.legend = NA, 
                               inherit.aes = TRUE, position = "identity") {
  
  layer(
    data = data,
    mapping = mapping,
    stat = "identity",
    geom = GeomDumbbellSize,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      colour_x = colour_x,
      colour_xend = colour_xend,
      dot_guide = dot_guide,
      dot_guide_size = dot_guide_size,
      dot_guide_colour = dot_guide_colour,
      ...
    )
  )
}

#' Better theme_map() for ggplot2.
#'
#' @importFrom ggplot2 theme theme_minimal element_text element_blank 
#' @importFrom ggplot2 element_line element_rect
#' @param ... other options to ggplot2::theme()
#' @export
theme_map <- function(...) {
  theme_minimal() +
    theme(text = element_text(family = "Brandon Grotesque Medium",
                              color = "#22211d", size = 13),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "#f5f5f2"),
          panel.background = element_rect(fill = "#f5f5f2"),
          legend.background = element_rect(fill = "#f5f5f2"),
          strip.background = element_rect(fill = "#f5f5f2"),
          panel.border = element_blank(),
          rect = element_rect(linetype = 0, colour = NA),
          legend.key = element_rect(fill = "#f5f5f2"),
          ...)
}

#' A function that extends ggplot2::annotation_custom by automatically
#' picking a textGrob, and allowing the user to pass the graphical parameters
#' of the grob into the function call itself.
#' 
#' @importFrom grid textGrob gpar
#' @importFrom ggplot2 layer
#' @inheritParams ggplot2::annotation_custom
#' @param label a string.
#' @param ... optional, passed to grid::gpar.
#' 
#' @export
annotation_custom_text <- function (label, xmin = -Inf, xmax = Inf, 
                                    ymin = -Inf, ymax = Inf, ...) {
  grob <- grid::textGrob(label, gp = grid::gpar(...))
  layer(data = data.frame(x = NA), stat = StatIdentity, 
        position = PositionIdentity, geom = GeomCustomAnn, 
        inherit.aes = FALSE, 
        params = list(grob = grob, xmin = xmin, xmax = xmax, 
                      ymin = ymin, ymax = ymax))
  cat(paste("Don't forget to set plot.margin in theme()",
            "and clip = 'off' in coord_cartesian() if needed!"))
}

#' A version of ggsave() meant to be piped to from a ggplot call or a ggplot
#' object.
#' 
#' @importFrom ggplot2 last_plot is.ggplot
#' @inheritParams ggplot2::ggsave
#' @export
save_gg <- function (plot = last_plot(), filename, device = NULL, path = NULL,
                     scale = 1, width = NA, height = NA,
                     units = c("in", "cm", "mm"),
                     dpi = 300, limitsize = TRUE, ...) {
  args <- lapply(substitute(list(...)), deparse)
  if (is.character(plot)){
    stop(paste('You passed a string as the first argument of this function.',
               'Are you using ggsave() syntax? This function is meant to be',
               'piped to from a ggplot() call, so the first argument is the',
               'plot itself, not the filename.'))
  }
  else if (!is.ggplot(plot)){
    stop('plot needs to be a "ggplot" class object.')
  }
  dpi <- ggplot2:::parse_dpi(dpi)
  dev <- ggplot2:::plot_dev(device, filename, dpi = dpi)
  dim <- ggplot2:::plot_dim(c(width, height), scale = scale, units = units,
                            limitsize = limitsize)
  if (!is.null(path)) {
    filename <- file.path(path, filename)
  }
  old_dev <- grDevices::dev.cur()
  if (capabilities('cairo') & grepl('\\.png$', filename) &
      !('type' %in% names(args))){
    dev(filename = filename, width = dim[1], height = dim[2],
        type = 'cairo-png', ...)
  }
  else{
    dev(filename = filename, width = dim[1], height = dim[2],
        ...)
  }
  on.exit(utils::capture.output({
    grDevices::dev.off()
    if (old_dev > 1) grDevices::dev.set(old_dev)
  }))
  ggplot2:::grid.draw.ggplot(plot)
  invisible()
}