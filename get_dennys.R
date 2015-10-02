rm(list=ls())
library(rvest)
library(magrittr)
library(stringr)
library(plyr)

to_get = read.csv('dennys_coords.csv', header=F)
to_put = 'data/dennys/'
dir.create(to_put, recursive=T, showWarnings=F)

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
  query = paste0('https://hosted.where2getit.com/dennys/responsive/ajax?&xml_request=%3Crequest%3E%3Cappkey%3E',
                 key,
                 '%3C%2Fappkey%3E%3Cformdata+id%3D%22locatorsearch%22%3E%3Cdataview%3Estore_default%3C%2Fdataview%3E%3Climit%3E',
                 limit,
                 '%3C%2Flimit%3E%3Corder%3Erank%2C_distance%3C%2Forder%3E%3Cgeolocs%3E%3Cgeoloc%3E%3Caddressline%3E%3C%2Faddressline%3E%3Clongitude%3E',
                 longitude,
                 '%3C%2Flongitude%3E%3Clatitude%3E',
                 latitude,
                 '%3C%2Flatitude%3E%3Ccountry%3EUS%3C%2Fcountry%3E%3C%2Fgeoloc%3E%3C%2Fgeolocs%3E%3Cstateonly%3E%3C%2Fstateonly%3E%3Csearchradius%3E',
                 radius,
                 '%3C%2Fsearchradius%3E%3C%2Fformdata%3E%3C%2Frequest%3E')
  download.file(query, destfile=paste0(to_put, 'latitude_', latitude, '.xml'), method='wget', quiet=T)
}

# Run query_api on the rows of the to_get data frame to download XML files containing Dennys locations.
a_ply(to_get, 1, query_api)
