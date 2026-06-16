build_model_area<-function(r2_bd, nests, epsg){
  r2_bd_cl<-st_transform(r2_bd, epsg)

  forests_w_nests<-r2_bd_cl |>
    st_intersection(nests)  |> 
  count(forestname, forestnumber) |>
  pull(forestnumber)  

  r2_bd_cl |>
    filter(forestnumber %in% forests_w_nests)
}

