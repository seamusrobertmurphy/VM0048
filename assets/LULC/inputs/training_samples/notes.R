
# Step 1: Reload the edited training_samples from the CSV file
training_samples <- read.csv("./data/training_samples/modified_samples.csv")
training_samples <- as_tibble(training_samples)
cube_dates <- as.Date(c("2014-01-04", "2019-01-02", "2024-01-16"))
training_samples <- training_samples %>%
  mutate(start_date = min(cube_dates), end_date = max(cube_dates))
write.csv(training_samples, "./data/training_samples/snapped_samples.csv", row.names = FALSE)

samples  = st_read("./data/training_samples/samples.shp") |> sf::st_transform(32629)
ndvi     = terra::rast("./data/NDVIs/LANDSAT_TM-ETM-OLI_200055_NDVI_2014-01-04_32629.tif")
ndvi_ext = terra::ext(ndvi)
bbox = c(
  xmin = ndvi_ext[1],
  ymin = ndvi_ext[3],
  xmax = ndvi_ext[2],
  ymax = ndvi_ext[4]
  ) |> terra::ext()

# Extend the samples to match the bbox_extent
bbox_polygon <- terra::as.polygons(bbox)
bbox_sf <- st_as_sf(bbox_polygon)
st_crs(bbox_sf) = 32629
samples_extended <- st_intersection(samples, bbox_sf)


#################################
# SITS sample data...
# Select a small area inside the tile
roi <- c(
  lon_max = -63.25790, lon_min = -63.6078,
  lat_max = -8.72290, lat_min = -8.95630
)

# Select a S2 tile
s2_cube_ro <- sits_cube(
  source      = "MPC",
  collection  = "LANDSAT-C2-L2",
  bands = c("RED", "NIR08", "CLOUD"),
  start_date = as.Date("2020-06-01"),
  end_date = as.Date("2021-09-01"),
  roi = roi
)


# Regularize the small area cube
s2_reg_cube_ro <- sits_regularize(
  cube = s2_cube_ro,
  output_dir = "./tempdir/chp12/",
  res = 60,
  roi = roi,
  period = "P16D",
  multicores = 8,
  progress = FALSE
)

devtools::install_github("e-sensing/sitsdata")
library(sitsdata)
# Load the training set
data("samples_prodes_4classes")
# Select the same three bands used in the data cube
samples_4classes_3bands <- sits_select(
  data = samples_prodes_4classes,
  bands = c("B02", "B8A", "B11")
)



# Train a random forest model
rfor_model <- sits_train(
  samples = samples_4classes_3bands,
  ml_method = sits_rfor()
)

# Classify the small area cube
s2_cube_probs <- sits_classify(
  data = s2_reg_cube_ro,
  ml_model = rfor_model,
  output_dir = "./tempdir/chp12/",
  memsize = 15,
  multicores = 5
)

# Post-process the probability cube
s2_cube_bayes <- sits_smooth(
  cube = s2_cube_probs,
  output_dir = "./tempdir/chp12/",
  memsize = 16,
  multicores = 4
)

# Label the post-processed  probability cube
s2_cube_label <- sits_label_classification(
  cube = s2_cube_bayes,
  output_dir = "./tempdir/chp12/",
  memsize = 16,
  multicores = 4
)

plot(s2_cube_label)

###################################



aoi = sf::read_sf("~/OneDrive - Winrock International Institute for Agricultural Development/20087 - RSPB Gola Feasibility/Working Files/Winrock_GIS Analysis Gola/ProjectArea.shp") 
aoi = aoi |>
  sf::st_cast("POLYGON") |>
  sf::st_cast("MULTIPOLYGON") |>
  dplyr::filter(
    #NAME   == "Gola Forest National Park" | 
      #NAME == "Tonglay" |
      NAME == "Normon") |>
  sf::st_transform(4326)



