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