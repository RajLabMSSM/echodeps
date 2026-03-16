# HTML to PNG

Convert an interactive HTML widget to a static PNG image.

## Usage

``` r
html_to_png(
  path,
  png_path = gsub("\\.html", ".png", path),
  res = 3,
  vwidth = 500 * res,
  vheight = 500 * res,
  delay = 1,
  zoom = 1
)
```

## Arguments

- path:

  Path to an HTML file.

- png_path:

  Path to save the PNG file. Defaults to replacing the `.html` extension
  with `.png`.

- res:

  Resolution multiplier.

- vwidth, vheight:

  Viewport width and height. This is the width or height of the virtual
  browser "window". Chrome expects integer values; numeric values are
  rounded to the nearest integer.

- delay:

  Time to wait before taking screenshot, in seconds. Sometimes a longer
  delay is needed for all assets to display properly.

- zoom:

  A number specifying the zoom factor. A zoom factor of 2 will result in
  twice as many pixels vertically and horizontally. Note that using 2 is
  not exactly the same as taking a screenshot on a HiDPI (Retina)
  device: it is like increasing the zoom to 200% in a desktop browser
  and doubling the height and width of the browser window. This differs
  from using a HiDPI device because some web pages load different,
  higher-resolution images when they know they will be displayed on a
  HiDPI device (but using zoom will not report that there is a HiDPI
  device).

## Value

Path to PNG.

## Examples

``` r
if (FALSE) { # \dontrun{
png_path <- html_to_png(path=res$save_path,
                        png_path=file.path("~/Downloads/depgraph.png"),
                        res=5, zoom=2)
} # }
```
