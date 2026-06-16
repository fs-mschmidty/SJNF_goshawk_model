evt_extract<-function(file, nest_sites, epsg){

  evt<-terra::rast(file)
  evt

  activeCat(evt)<-"EVT_GP_N"
  
  nest_sites_t<-nest_sites|>
    st_transform(crs(evt))
  
  extract(evt, as(nest_sites_t, "SpatVector"))
}

