aoi   = sf::read_sf("./inputs/chilwa_watershed_4326.shp")
malawi_border = giscoR::gisco_get_countries(country = "Malawi",resolution = "3")
region_border = giscoR::gisco_get_countries(
  country = c("Malawi", "Zambia", "Tanzania", "Mozambique"), resolution = "3"
  )

bbox_aoi  = terrainr::add_bbox_buffer(aoi, 20000, "meters")
bbox_malawi  = terrainr::add_bbox_buffer(malawi_border, 400000, "meters")
bbox_malawi_ext = terra::vect(terra::ext(vect(malawi_border)) * 1.5) 
crs(bbox_malawi_ext) = "epsg:3857"

# 'zoom' = resolution (higher than tm_basemap)
basemap_150k = maptiles::get_tiles(
  bbox_aoi, 
  zoom      = 12, 
  crop      = T,
  provider  = "OpenTopoMap"
)

basemap_4m = maptiles::get_tiles(
  bbox_malawi_ext, 
  zoom      = 7, 
  crop      = T,
  provider  = "OpenStreetMap"
)

tmap::tm_shape(basemap_150k) + tm_rgb() + 
  tmap::tm_shape(aoi) +
  tmap::tm_borders(lwd = 1, col = "red") +
  tmap::tm_graticules(lines=T,labels.rot=c(0,90),lwd=0.2) +
  tmap::tm_credits("EPSG:4326", position = c("left", "bottom"), credits.fontface="bold") + 
  tmap::tm_scalebar(c(0, 10, 20, 40), position = c("right", "bottom")) +
  tmap::tm_compass(
    type = "4star", size = 1.5,
    color.dark = "gray60", text.color = "gray60",
    position = c("left", "top")
    ) -> fieldmap
fieldmap

tmap::tm_shape(basemap_4m) + tm_rgb(alpha=0.2) + 
  tmap::tm_shape(region_border) +
  tmap::tm_borders(lwd = 0.5, col = "black") +
  tmap::tm_shape(aoi) +
  tmap::tm_borders(lwd = 2, col = "red", fill="#e28672", fill_alpha=0.5) +
  tmap::tm_graticules(lines=T,labels.rot=c(0,90),lwd=0.2) +
  tmap::tm_compass(
    type = "4star", size = 1.5,
    color.dark = "black", text.color = "gray60",
    position = c("left", "bottom")) +
  tmap::tm_scalebar(c(0, 50, 100, 200), position = c("RIGHT", "BOTTOM"), text.size = 0.9) +
  tm_credits("EPSG:4326") -> insetmap
insetmap

#vp <- grid::viewport(x = 0.615, y = 0.5, width = 0.6, height = 0.6, just = c("left", "top"))

# width & height = res, dpi = size of add-ons
tmap::tmap_save(
  fieldmap, "./outputs/map_locator_site.png", 
  width=21600, height=21600, asp=0, dpi=2400
  )

tmap::tmap_save(insetmap, "./outputs/map_locator_country.png")

tmap::tmap_arrange(insetmap, fieldmap, ncol = 2)



# gistillery::gist_upload(gist_name = "fieldmaps-opentopo.R") 
# gistillery::gist_to_carbon(file = "fieldmap-opentopo.png") 
# gist_append_img(imgur_url = "https://i.imgur.com/avPHwev.png", gist_id = "3df2cc659c919d3db0a3296f82ce8db0")
  