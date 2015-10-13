rm(list=ls())
library(rvest)
library(magrittr)
library(stringr)
library(plyr)

#Read data from 'dennys_coords.csv'
to_get = read.csv('dennys_coords.csv', header=F)
#Name to_put as directory 'data/dennys/' 
to_put = 'data/dennys/'
#Create the directory to_put 
dir.create(to_put, recursive=T, showWarnings=F)
#List the api key
api_key = '6B962D40-03BA-11E5-BC31-9A51842CA48B'


# query_api()
#############################################################################################
# Input: coordinate, a data frame row containing a longitude, a latitude, and a search radius;
#        key (default=api_key), a valid API key allowing data scraping;
#        limit (default=1000), an upper bound for the number of search results returned.
#
# Output: Nothing; an XML file containing the results of the API query is downloaded to disk.

query_api = function(coordinate, key=api_key, limit=1000) {
  longitude = coordinate[,1]
  latitude = coordinate[,2]
  radius = coordinate[,3]
  query = paste0('https://hosted.where2getit.com/dennys/responsive/ajax?&xml_request=<request><appkey>',
                 key,
                 '</appkey><formdata id="locatorsearch"><dataview>store_default</dataview><limit>',
                 limit,
                 '</limit><order>_distance</order><geolocs><geoloc><addressline></addressline><longitude>',
                 longitude,
                 '</longitude><latitude>',
                 latitude,
                 '</latitude><country>US</country></geoloc></geolocs><stateonly>1</stateonly><searchradius>',
                 radius,
                 '</searchradius></formdata></request>')
  
# Generate the query that consists of elements from coordinate and download it into the directory saved as 'to_put'  
  download.file(query, destfile=paste0(to_put, 'longitude_', longitude, '.xml'), method='wget', quiet=T)
# Suspend exucution of R for 0.25 seconds
  Sys.sleep(0.25)
}
# Run query_api on the rows of the to_get data frame to download XML files containing Dennys locations.
# a_ply takes arguments (.data, .margins, .fun)
a_ply(to_get, 1, query_api)
