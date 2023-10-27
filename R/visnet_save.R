#' @title visNetwork save
#'
#' @description
#' Save a visNetwork object as a png.
#' @param res Resolution multiplier.
#' @param run_shrink Run \link[webshot2]{shrink} on the png.
#' @inheritParams webshot2::webshot
#' @inheritParams webshot2::resize
#' @returns png file path
#'
#' @export
#' @examples
#' save_path <- tempfile(fileext = ".html")
#' res <- dep_graph(pkg = "echoverse",
#'                  layout=layout_star,
#'                  save_path=save_path)
#' visnet_save(url = "https://datastorm-open.github.io/visNetwork/")
visnet_save <- function(url,
                        file=gsub("\\.html",".png",url),
                        res=8,
                        delay=.2,
                        zoom=1,
                        vwidth = 992*res,
                        vheight = 744*res,
                        geometry=NULL,
                        run_shrink=FALSE
                        ){
    # devoptera::args2vars(visnet_save)
    requireNamespace("webshot2")

    # cliprect = c(.5*vwidth,
    #              .3*vheight,
    #              .5*vwidth,
    #              .3*vheight)

    webshot2::webshot(url = url,
                      file = file,
                      vwidth = vwidth,
                      vheight = vheight,
                      delay = delay,
                      zoom = zoom)

    if(!is.null(geometry)){
        webshot2::resize(filename = fpng,
                         geometry = geometry)
    }
    if(run_shrink){
        webshot2::shrink(filename = fpng)
    }
    return(fpng)
}
