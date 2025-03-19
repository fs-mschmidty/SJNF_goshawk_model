library(sf)
library(tidyverse)
library(mapview)

nests <- st_read("T:\\FS\\NFS\\SanJuan\\Program\\2600WildlifeMgmt\\GIS\\ColumbineRangerDistrict\\Columbine_GIS_Modernization_build_projects\\Raptor_nest_database_update\\gina_raptorDB.gdb", "Raptor_Nest_Sites") |>
  st_transform(4326)

nogo <- nests |>
  filter(SPECIES == "NOGO")

nogo %>%
  bind_cols(st_coordinates(.))|>
  as_tibble()|>
  select(NEST_NAME, X, Y)|>
  write_csv("output/amgo_nest_xy.csv")

mapview(nogo)
