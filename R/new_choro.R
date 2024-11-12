#' #' @title Plot a legend for a choropleth map
#' #' @description This function plots a legend for a choropleth map.
#' #'
#' #' @param pal a set of colors
#' #' @param alpha if \code{pal} is a \link{hcl.colors} palette name, the
#' #' alpha-transparency level in the range \[0,1\]
#' #' @param col_na color for missing values
#' #' @param pos position of the legend, one of "topleft", "top",
#' #' "topright", "right", "bottomright", "bottom", "bottomleft",
#' #' "left", "interactive" or a vector of two coordinates in map units
#' #' (c(x, y))
#' #' @param val break labels
#' #' @param title title of the legend
#' #' @param title_cex size of the legend title
#' #' @param val_cex size of the values in the legend
#' #' @param val_rnd number of decimal places of the values in
#' #' the legend.
#' #' @param no_data if TRUE a "missing value" box is plotted
#' #' @param no_data_txt label for missing values
#' #' @param frame whether to add a frame to the legend (TRUE) or not (FALSE)
#' #' @param box_border color of the boxes' borders
#' #' @param size size of the legend; 2 means two times bigger
#' #' @param bg background of the legend
#' #' @param fg foreground of the legend
#' #' @param box_cex width and height cex of boxes
#' #' @param mar plot margins
#' #' @param return_bbox return only bounding box of the legend.
#' #' No legend is plotted.
#' #' @param adj adj
#' #' @param frame_border border color of the frame
#' #' @keywords internal
#' #' @noRd
#' #' @import graphics
#' #' @return No return value, a legend is displayed.
#' #' @examples
#' #' plot.new()
#' #' plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = 1)
#' #' leg_choro(val = c(1, 2, 3, 4), pal = c("red1", "red3", "red4"))
#' leg_choro <- function(pos = "left",
#'                       val,
#'                       pal = "Inferno",
#'                       alpha = 1,
#'                       title = "Legend Title",
#'                       title_cex = .8 * size,
#'                       val_cex = .6 * size,
#'                       val_rnd = 0,
#'                       col_na = "white",
#'                       no_data = FALSE,
#'                       no_data_txt = "No Data",
#'                       frame = FALSE,
#'                       frame_border = fg,
#'                       box_border = "#333333",
#'                       bg = "#f7f7f7",
#'                       fg = "#333333",
#'                       size = 1,
#'                       box_cex = c(1, 1),
#'                       return_bbox = FALSE,
#'                       mar = par("mar"),
#'                       adj = c(0, 0)) {
#'
#'   offset <- xinch(par("csi")) / 3
#'
#'   # box size mgmt
#'   # box width
#'   w <- offset * size * 4
#'   # box height
#'   h <- inset / 1.5
#'   if (length(box_cex) == 2) {
#'     w <- w * box_cex[1]
#'     h <- h * box_cex[2]
#'   }
#'
#'   # get well rounded and ordered values for the legend
#'   val <- rev(get_val_rnd(val = val, val_rnd = val_rnd))
#'
#'   # number of boxes
#'   n <- length(val) - 1
#'
#'   # box colors
#'   pal <- rev(get_pal(pal, n))
#'
#'   ########################### FUNS
#'
#'   # get sizes
#'   size_title <- function(title, title_cex) {
#'     w <- h <- 0
#'     if(title != ""){
#'       h <- strheight(title, units = "user", cex = title_cex, font = 1)
#'       w <- strwidth(title, units = "user", cex = title_cex, font = 1)
#'     }
#'     return(list(w = w, h = h))
#'   }
#'
#'   size_boxes <- function(w, h, n){
#'     return(list(w = w, h = h * n))
#'   }
#'
#'   size_NA_box <- function(w, h, no_data){
#'     if(isFALSE(no_data)){
#'       w <- h <- 0
#'     }
#'     return(list(w = w, h = w))
#'   }
#'
#'   size_boxes_lab <- function(val, val_cex, h, n){
#'     w <- max(strwidth(val, units = "user", cex = val_cex, font = 1))
#'     h <- h * n +
#'       strwidth(val[1], units = "user", cex = val_cex, font = 1) / 2 +
#'       strwidth(val[n+1], units = "user", cex = val_cex, font = 1) / 2
#'     return(list(w = w, h = h))
#'   }
#'
#'   size_NA_lab <- function(no_data, val_cex){
#'     w <- h <- 0
#'     if(isTRUE(no_data)){
#'       w <- strwidth(no_data_txt, units = "user", cex = val_cex, font = 1)
#'       h <- strheight(no_data_txt, units = "user", cex = val_cex, font = 1)
#'     }
#'     return(list(w = w, h = w))
#'   }
#'
#'
#'   size_leg <- function(s_title, s_boxes, s_NA_box, s_boxes_lab, s_NA_lab, offset) {
#'
#'     offset1 <- offset2 <- offset
#'     if (s_title$h != 0) {
#'       h_title <- 0
#'       offsets[1] <- 0
#'     }
#'     if (s_NA_box$h == 0) {
#'       h_NA_box <- 0
#'       offsets[2] <- 0
#'     }
#'     h <- h_title + offsets[1] + h_boxes_lab + offsets[2] + s_NA_box
#'
#'
#'     w_text <- max(s_boxes_lab$w, s_NA_lab$w)
#'
#'
#'     offsets
#'
#'
#'
#'
#'     if (s_title$h != 0) {}
#'
#'
#'
#'   }
#'
#'
#'
#'
#'
#'   if (!is.null(xy_leg)) {
#'     break
#'   }
#'
#'   xy_leg <- get_pos_leg(
#'     pos = pos,
#'     xy_rect = unlist(xy_rect),
#'     adj = adj,
#'     xy_title = xy_title,
#'     frame = frame
#'   )
#' }
#'
#' if (return_bbox) {
#'   return(invisible(
#'     list(
#'       xleft = xy_rect[[1]] - insetf / 4,
#'       ybottom = xy_rect[[2]] - insetf / 4,
#'       xright = xy_rect[[3]] + insetf / 4,
#'       ytop = xy_rect[[4]] + insetf / 4
#'     )
#'   ))
#' }
#'
#' # Display
#' if (frame) {
#'   rect(
#'     xleft = xy_rect[[1]] - insetf / 4,
#'     ybottom = xy_rect[[2]] - insetf / 4,
#'     xright = xy_rect[[3]] + insetf / 4,
#'     ytop = xy_rect[[4]] + insetf / 4,
#'     col = bg,
#'     border = frame_border,
#'     lwd = .7
#'   )
#' }
#' # title
#' text(
#'   xy_title$x,
#'   y = xy_title$y,
#'   labels = title,
#'   cex = title_cex,
#'   adj = c(0, 0),
#'   col = fg
#' )
#' # boxes
#' rect(
#'   xy_box[[1]],
#'   xy_box[[2]],
#'   xy_box[[3]],
#'   xy_box[[4]],
#'   col = pal,
#'   border = box_border,
#'   lwd = .7
#' )
#' # labels
#' text(
#'   xy_box_lab$x,
#'   y = xy_box_lab$y,
#'   labels = val,
#'   cex = val_cex,
#'   adj = c(0, 0.5),
#'   col = fg
#' )
#' # no data
#' if (no_data) {
#'   # NA box
#'   rect(
#'     xy_nabox[[1]],
#'     xy_nabox[[2]],
#'     xy_nabox[[3]],
#'     xy_nabox[[4]],
#'     col = col_na,
#'     border = box_border,
#'     lwd = .7
#'   )
#'   # NA text
#'   text(
#'     xy_nabox_lab$x,
#'     y = xy_nabox_lab$y,
#'     labels = no_data_txt,
#'     cex = val_cex,
#'     adj = c(0, 0.5),
#'     col = fg
#'   )
#' }
#'
#' return(invisible(NULL))
#' }
