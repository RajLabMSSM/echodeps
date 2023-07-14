#' HTML to PNG
#' 
#' Convert an interactive HTML widget to a static PNG image.
#' @param path Path to an HTML file.
#' @param res Resolution multiplier.
#' @inheritParams webshot2::webshot 
#' @returns Path to PNG.
#' 
#' @export
#' @examples
#' \dontrun{
#' png_path <- html_to_png(path=res$save_path,
#'                         png_path=file.path("~/Downloads/depgraph.png"),
#'                         res=5, zoom=2)
#' }
html_to_png <- function(path,
                        png_path=gsub("\\.html",".png",path),
                        res=3,
                        vwidth=500*res,
                        vheight=500*res,
                        delay=1,
                        zoom=1){
  # f <-  here::here("figures","network.html")
  requireNamespace("webshot2")
  
  webshot2::webshot(url = path,
                    file = png_path,
                    vwidth = vwidth,
                    vheight = vheight,
                    delay = delay, 
                    zoom = zoom)
  return(png_path)
}