# draw a hex sticker for `synthesisr`
library(tibble)
library(dplyr)
library(string2path)
library(showtext)
library(sf)
library(ggplot2)
library(hexSticker)
# library(viridis)
# remotes::install_github("johannesbjork/LaCroixColoR")
# library(LaCroixColoR)

# get 'synthesisr' text as a polygon
final_size <- 1.4
synth_line <- string2path("synthesisr",
                          font = "inst/hex/Space_Mono/SpaceMono-Bold.ttf") |>
  tibble::rowid_to_column() |>
  tibble() |>
  mutate(x = x - min(x), y = y - min(y)) |> # place both mins at 0
  mutate(y = y / max(x), x = x / max(x)) |> # now at x = c(0, 1)
  mutate(x = (x * final_size) - (final_size * 0.5), y = y * final_size) |> # scale to required size
  mutate(y = y - (max(y) * 0.5)) # centre vertically

# convert to `sf` object to allow calculation of spatial properties
text_polygons <- synth_line %>%
  st_as_sf(coords = c("x", "y")) |>
  group_by(path_id) |>
  summarise(geometry = st_combine(geometry)) |>
  st_cast("POLYGON")

# need to clip 6 ('e') with 7 (inside of 'e')
text_cutouts <- text_polygons[7, ]
text_polygons <- text_polygons[-7, ]
words <- st_difference(text_polygons, text_cutouts)

# clean up
rm(final_size, synth_line, text_polygons, text_cutouts)

# now create hexagons
# from hexSticker, but using sf objects
create_hexagon <- function(scale = 1){
  hexd <- data.frame(x = 1+c(rep(-sqrt(3)/2, 2), 0, rep(sqrt(3)/2, 2), 0),
                     y = 1+c(0.5, -0.5, -1, -0.5, 0.5, 1))
  rbind(hexd, hexd[1, ]) |>
    tibble() |>
    mutate(x = (x - 1) * scale,
           y = (y - 1) * scale) |>
    st_as_sf(coords = c("x", "y")) |>
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
}

external_hexagon <- create_hexagon(scale = 1.00)
internal_hexagon <- create_hexagon(scale = 0.935)

# now create vertical lines that intersect with words
x_vec <- seq(-0.87, 0.87, by = 0.005)
result_internal <- lapply(x_vec, function(a){
  b <- data.frame(x = a, y = c(-1, 1)) |>
    st_as_sf(coords = c("x", "y")) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("LINESTRING") |>
    st_intersection(words)

  tibble(x = a, length = sum(st_length(b)))
}) |>
  bind_rows()

result_external <- lapply(x_vec, function(a){
  b <- data.frame(x = a, y = c(-1, 1)) |>
    st_as_sf(coords = c("x", "y")) |>
    summarise(geometry = st_combine(geometry)) |>
    st_cast("LINESTRING") |>
    st_intersection(internal_hexagon)

  b |> mutate(x = a)
}) |>
  bind_rows()

# merge
background_lines <- left_join(result_external,
                              result_internal,
                              by = "x")

# clean up
rm(x_vec)

# draw
font_add("spacemono", "inst/hex/Space_Mono/SpaceMono-Regular.ttf")
showtext_auto()

edge_color <- "#000000" # "#b951c9"
# palette <- lacroix_palette("CranRaspberry", n = 15, type = "continuous") |>
#   as.character()

# example colors:
# x <- lacroix_palette("CranRaspberry", n = 7, type = "continuous") |> as.character()
simple_palette <- c("#c92029",
                    "#a3086a",
                    "#6c159e",
                    "#0a238a")

p <- ggplot() +
  geom_sf(data = external_hexagon, fill = "white", color = NA) +
  geom_sf(data = background_lines,
          mapping = aes(
            color = x,
            alpha = (length ^ 1.2)),
          linewidth = 0.3) +
  geom_sf(data = internal_hexagon, fill = NA, color = edge_color, linewidth = 0.1) +
  geom_sf(data = words, fill = "white", color = edge_color, linewidth = 0.1) +
  annotate(geom = "text",
           x = 0.7,
           y = -0.17,
           label = "mjwestgate",
           family = "spacemono",
           size = 8,
           hjust = 1,
           color = "#ffffff") +
  # geom_vline(xintercept = 0.35) +
  scale_colour_gradientn(colors = simple_palette) +
  # scale_color_viridis(option = "H") +
  scale_alpha(range = c(0.5, 1)) +
  # scale_color_gradient(low = "#800194", high = "#b951c9") +
  theme_void() +
  theme(legend.position = "none")

ggsave("man/figures/logo.png",
       p,
       width = 43.9,
       height = 50.8,
       units = "mm",
       bg = "transparent",
       dpi = 600)
