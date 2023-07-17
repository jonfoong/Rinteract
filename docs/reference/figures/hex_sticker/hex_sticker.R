library(magick)
library(hexSticker)
library(sysfonts)

hex_img <- image_read('hex_sticker/hex_base.png')

font_add("neville", regular = "hex_sticker/fonts/Neville_Regular.ttf")

sticker(
  subplot = hex_img,
  package = "Rinteract",
  p_x = 1,
  p_y = 1,
  p_color = 'black',
  p_family = "neville",
  p_size = 35,
  s_width = 2.03,
  s_height = 2.03,
  s_x = 1,
  s_y = 1,
  h_fill = 'lightblue',
  h_color = NA,
  h_size = 0,
  u_size = 5,
  spotlight = T,
  l_y = 1,
  l_x = 1,
  l_width = 3,
  l_height = 3,
  l_alpha = 0.5,
  dpi = 300,
  filename = "hex_sticker/sticker.png"
) %>% print()





