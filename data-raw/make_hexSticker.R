library(hexSticker)
library(showtext)
library(magick)

imgpath <- "data-raw/hexSticker_input.png"

font_add_google("Bungee Shade", "bung")
font_add_google("Gemunu Libre", "gemu")
p_family <- "gemu"
p_fontface <- 2

# library(extrafont)
# font_import(paths = "C:/Windows/Fonts")
# loadfonts(device = "win")
# p_family = "Magneto"

packageName <- "mixfishtools"

fname <- paste0("hexSticker/hexSticker_", packageName, ".png")
fname <- paste0(tempfile(), ".png")
sticker(imgpath, package="mixfishtools",
  h_size = 1.25, h_color = "purple4", h_fill = "#F1FF71",
  p_x = 1, p_y = 1.62, p_size = 35, p_color = "yellow",
  url = "ices-tools-dev/mixfishtools", u_x = 1.05,
  u_y = 0.1, u_size = 10, u_angle = 30, u_color = "yellow",
  p_family = p_family, p_fontface = p_fontface,
  s_x = 1.05, s_y = 0.9, s_width = 1.08, dpi = 600,
  white_around_sticker = TRUE,
  filename = fname)

fuzz <- 50
p <- image_read(fname)
pp <- p %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = "+1+1") %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+", image_info(p)$width-1, "+1")) %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+1", "+", image_info(p)$height-1)) %>%
  image_fill(color = "transparent", refcolor = "white", fuzz = fuzz, point = paste0("+", image_info(p)$width-1, "+", image_info(p)$height-1))
image_write(image = pp, path = paste0("hexSticker/hexSticker_", packageName, "_trans.png"))
file.show(paste0("hexSticker/hexSticker_", packageName, "_trans.png"))
