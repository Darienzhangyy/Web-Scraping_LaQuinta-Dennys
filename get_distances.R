format_coords = function(row, data.1, data.2) {
  lqd_distance = cbind(data.1[row[,1],,drop=F], data.2[row[,2],,drop=F])
  colnames(lqd_distance) = c('id1', 'lat1', 'lon1', 'id2', 'lat2', 'lon2')
  return(lqd_distance)
}

haversine = function(row, metric=F) {
  # Check that aspace package is installed and install if necessary.
  if(!('aspace' %in% installed.packages()[,'Package'])) {
    install.packages('aspace', repos='http://cran.rstudio.com/') 
  }
  require(aspace)
  
  # Provide output in kilometers if directed.
  if(metric) {
    radius = 6378.1
  } else {
    radius = 3963.17
  }
  
  # Implement the haversine formula with degree-based input.
  a = sin_d((row$lat1 - row$lat2)/2)^2 + 
    cos_d(row$lat1) * cos_d(row$lat2) * 
    sin_d((row$lon1 - row$lon2)/2)^2
  distance = 2 * atan2(sqrt(a), sqrt(1 - a)) * radius
}

get_coords = function(value, var=variable) {
  lq_lat = hotel_info$latitude[which(hotel_info[,var]==value)] %>% as.numeric
  lq_lon = hotel_info$longitude[which(hotel_info[,var]==value)] %>% as.numeric
  d_lat = dennys_info$latitude[which(dennys_info[,var]==value)] %>% as.numeric
  d_lon = dennys_info$longitude[which(dennys_info[,var]==value)] %>% as.numeric
  la_quinta = cbind(id=seq(length(lq_lat)), latitude=lq_lat, longitude=lq_lon) %>% as.data.frame
  dennys = cbind(id=seq(length(d_lat)), latitude=d_lat, longitude=d_lon) %>% as.data.frame
  indexes = expand.grid(la_quinta$id, dennys$id)
  coords = adply(indexes, 1, format_coords, data.1=la_quinta, data.2=dennys)
  out = adply(coords, 1, haversine) %>% .[,c('id1', 'id2', 'V1')]
  colnames(out) = c('lq_id', 'd_id', 'distance')
  return(out)
}

distances_by_cluster = function(variable, data1=hotel_info, data2=dennys_info) {
  stopifnot(variable %in% colnames(data1) & variable %in% colnames(data2))
  clusters = intersect(unique(data1[,variable]), unique(data2[,variable])) %>% as.list
  coords = llply(clusters, get_coords, var=variable)
  names(coords) = clusters
  out = ldply(coords, function(value) { summary(value$distance) } )
  out = out[order(out$.id),]
  colnames(out)[1] = variable
  return(out)
}
court_distances = distances_by_cluster('court')
cd_distances = distances_by_cluster('district')
party_distances = distances_by_cluster('party')

save(file='data/distance_data.Rdata', list=c('court_distances', 'cd_distances', 'party_distances'))
