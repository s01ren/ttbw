# load packages -----------------------------------------------------------
library(tidyverse)
library(sf)


# define colours ----------------------------------------------------------
colour_spaetzlegelb <- "#FFFEF9"
colour_rangemax <- "#AA4465"


# define functions --------------------------------------------------------
get_centroid <- function(name){
  coords <- 
    EINWOHNER.Kreise %>% 
    filter(Name %in% name) %>% 
    select(geometry) %>% 
    st_as_sf() %>% 
    st_centroid() %>% 
    unlist()
  
  if(length(coords)>2){
    warning("More than one centroid found!")
  }
  
  returnData <- 
    tribble(
      ~"Name", ~"x", ~"y",
      name, coords[1], coords[2]
    )
  
  return(returnData)
}


# read and tidy data ------------------------------------------------------
file_bevoelkerung <- "data/Bevoelk_I_Flaeche_j.csv"
file_shapes <- "data/shapefiles/AX_Gebiet_Kreis.shp"

EINWOHNER.Kreise <- 
  read_delim(
    file = file_bevoelkerung, 
    locale = locale(encoding = readr::guess_encoding(file_bevoelkerung) %>% select(encoding) %>% unlist()),
    delim = ";", 
    skip = 20,
    col_names = c(
      "ID.Regionaleinheit",
      "AmtlicherGemeindeschluessel",
      "PLZ",
      "Regionalname",
      "Stichtag",
      "Bevoelkerung_insgesamt",
      "Gemeindegebiet_ha",
      "Bevoelkerungsdichte_EW_km2"
    ),
    col_types = c(
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_character(),
      col_double(),
      col_double(),
      col_double()
    )
  ) %>% 
  filter(
    Stichtag == "31.12.2018",
    ID.Regionaleinheit == "KR"
  ) %>% 
  left_join(
    y = read_sf(file_shapes),
    by = c("AmtlicherGemeindeschluessel" = "Schlüssel")
  ) %>% 
  select(
    AmtlicherGemeindeschluessel,
    Name,
    Bevoelkerung_insgesamt,
    Gemeindegebiet_ha,
    Bevoelkerungsdichte_EW_km2,
    geometry
  )
EINWOHNER.Kreise


# prepare annotations -----------------------------------------------------
plot_subtitle <- str_c(
  "Die Karte zeigt die Bevölkerungsdichte nach Kreisen in Baden-Württemberg.",
  "Am dünnsten besiedelt ist der Main-Tauber-Kreis. Hier wohnen 101 Personen pro km².",
  "Mehr als 30x mehr Personen pro Quadratkilomenter wohnen in der Landeshauptstadt.",
  sep = " "
)

plot_caption <- str_c(
  "Daten: Statistisches Landesamt Baden-Württemberg;",
  "Karten: Landesamt für Geoinformation und Landentwicklung",
  "Grafik: https://gluecko.se",
  sep = " "
)


# save relevant coordinates -----------------------------------------------
coord_xmin <- sf::st_bbox(EINWOHNER.Kreise$geometry)$xmin
coord_xmax <- sf::st_bbox(EINWOHNER.Kreise$geometry)$xmax
coord_ymin <- sf::st_bbox(EINWOHNER.Kreise$geometry)$ymin
coord_ymax <- sf::st_bbox(EINWOHNER.Kreise$geometry)$ymax

coord_Stuttgart_xlabel <- 1.05 * coord_xmin
coord_Stuttgart_ylabel <- 1.0045 * get_centroid("Stuttgart")$y

coord_Mannheim_xlabel <- 1.1 * coord_xmin
coord_Mannheim_ylabel <- 1.005 * get_centroid("Mannheim")$y

coord_Karlsruhe_xlabel <- 1.1 * coord_xmin
coord_Karlsruhe_ylabel <- 1.005 * get_centroid("Karlsruhe")$y

coord_MainTauberKreis_xlabel <- 0.95 * coord_xmax
coord_MainTauberKreis_ylabel <- 1.005 * get_centroid("Main-Tauber-Kreis")$y

coord_Sigmaringen_xlabel <- 0.98 * coord_xmax
coord_Sigmaringen_ylabel <- 1.005 * get_centroid("Sigmaringen")$y

coord_NeckarOdenwaldKreis_xlabel <- 0.82 * coord_xmax
coord_NeckarOdenwaldKreis_ylabel <- 1.0115 * get_centroid("Neckar-Odenwald-Kreis")$y


# plot map ----------------------------------------------------------------
ggplot(data = EINWOHNER.Kreise) + 
  theme(
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  panel.background = element_rect(fill = colour_spaetzlegelb),
  plot.background = element_rect(fill = colour_spaetzlegelb),
  legend.position = "none"
  ) + 
  
  geom_sf(aes(fill = Bevoelkerungsdichte_EW_km2)) + 
  scale_fill_gradient(low = colour_spaetzlegelb, high = colour_rangemax) + 
  
  annotate("text", label = "Bevölkerungsdichte nach Kreisen", x = .7 * coord_xmin, y = 1.01 * coord_ymax, size = 6, hjust = 0) +
  annotate("text", label = str_wrap(plot_subtitle, 40), x = .7 * coord_xmin, y = coord_ymax, size = 3, hjust = 0) +
  annotate("text", label = str_wrap(plot_caption, 40), x = .7 * coord_xmin, y = coord_ymax, size = 3, hjust = 0, vjust = 2.5) + 
  
  annotate("segment", x = coord_Stuttgart_xlabel, y = coord_Stuttgart_ylabel, xend = get_centroid("Stuttgart")$x,  yend = get_centroid("Stuttgart")$y) + 
  annotate("label", label = "Stuttgart\n3.062 EW/km²", x = coord_Stuttgart_xlabel, y = coord_Stuttgart_ylabel) + 
  
  annotate("segment", x = coord_Mannheim_xlabel, y = coord_Mannheim_ylabel, xend = get_centroid("Mannheim")$x, yend = get_centroid("Mannheim")$y) + 
  annotate("label", label = "Mannheim\n2.134 EW/km²", x = coord_Mannheim_xlabel, y = coord_Mannheim_ylabel) + 
  
  annotate("segment", x = coord_Karlsruhe_xlabel, y = coord_Karlsruhe_ylabel, xend = get_centroid("Karlsruhe")$x, yend = get_centroid("Karlsruhe")$y) + 
  annotate("label", label = "Karlsruhe\n1.805 EW/km²", x = coord_Karlsruhe_xlabel, y = coord_Karlsruhe_ylabel) + 

  annotate("segment", x = coord_MainTauberKreis_xlabel, y = coord_MainTauberKreis_ylabel, xend = get_centroid("Main-Tauber-Kreis")$x, yend = get_centroid("Main-Tauber-Kreis")$y) +
  annotate("label", label = "Main-Tauber-Kreis\n101 EW/km²", x = coord_MainTauberKreis_xlabel, y = coord_MainTauberKreis_ylabel) + 

  annotate("segment", x = coord_Sigmaringen_xlabel, y = coord_Sigmaringen_ylabel, xend = get_centroid("Sigmaringen")$x, yend = get_centroid("Sigmaringen")$y) +
  annotate("label", label = "Sigmaringen\n109 EW/km²", x = coord_Sigmaringen_xlabel, y = coord_Sigmaringen_ylabel) +

  annotate("segment", x = coord_NeckarOdenwaldKreis_xlabel, y = coord_NeckarOdenwaldKreis_ylabel, xend = get_centroid("Neckar-Odenwald-Kreis")$x, yend = get_centroid("Neckar-Odenwald-Kreis")$y) +
  annotate("label", label = "Neckar-Odenwald-Kreis\n127 EW/km²", x = coord_NeckarOdenwaldKreis_xlabel, y = coord_NeckarOdenwaldKreis_ylabel)

ggsave("plots/Bevoelkerungsdichte.png", bg = "transparent", dpi = 72)
