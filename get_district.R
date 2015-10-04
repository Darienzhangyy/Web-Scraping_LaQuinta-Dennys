rm(list=ls())
library(magrittr)
library(stringr)
library(plyr)

# Load the Dennys data frame and add an ID column.
load('data/dennys.Rdata')
dennys_info$id = nrow(dennys_info) %>% 
                 seq %>% 
                 as.character %>% 
                 str_pad(width=4, side='left', pad='0')

# Load the La Quinta data frame and add an ID column.
load('data/lq.Rdata')
hotel_info$id = nrow(hotel_info) %>% 
                seq %>% 
                as.character %>% 
                str_pad(width=4, side='left', pad='0')

# Create directories into which JSON files will be downloaded.
to_put = 'data/districts/'
dir.create(to_put, recursive=T, showWarnings=F)
dir.create(paste0(to_put, 'dennys'), recursive=T, showWarnings=F)
dir.create(paste0(to_put, 'lq'), recursive=T, showWarnings=F)

# Create a data frame with names of folders and data frames, used below.
to_do = data.frame(folder=c('dennys', 'lq'), frame=c('dennys_info', 'hotel_info'), stringsAsFactors=F)

# API key for the Sunlight Foundation's Sunlight Congress API.
sunlight_apikey = '9e01460f6df44599bec682478cd13fdc'


# query_sunlight()
#############################################################################################
# Input: row, a data frame row containing the longitude and latitude of the location;
#        folder, an identifier for the type of location used in file names;
#        key (default=sunlight_apikey), a valid API key from the Sunlight Foundation.
#
# Output: Nothing; an XML file containing the results of the API query is downloaded to disk.

query_sunlight = function(row, folder, key=sunlight_apikey) {
  query = paste0('congress.api.sunlightfoundation.com/legislators/locate?latitude=',
                 row$latitude,
                 '&longitude=',
                 row$longitude,
                 '&apikey=',
                 key)
  if(!(file.exists(paste0(to_put, folder, '/', row$id, '.json')))) {
    download.file(query, destfile=paste0(to_put, folder, '/', row$id, '.json'), method='wget', quiet=T)
    Sys.sleep(0.25)
  }  
}


# to_get()
#############################################################################################
# Input: folder, a file folder containing downloaded JSON files.
#
# Output: A list of all JSON files in the folder.

to_get = function(folder) {
  list.files(paste0(to_put, folder, '/')) %>% 
  paste0(paste0(to_put, folder, '/'), .) %>% 
  as.list(.)
}


# parse_json()
#############################################################################################
# Input: file, the filename of a JSON file.
#
# Output: info, a character vector containing the name of the congressional representative,   
#               the state and congressional district number (XX-#), and party affiliation.

parse_json = function(file) {
  json = suppressWarnings(readLines(file))
  fields = json %>% 
           str_split('\\{') %>%
           unlist %>%
           str_subset('chamber\\":\\"house') %>%
           str_split('\\",\\"|,\\"') %>% 
           unlist %>% 
           str_replace_all('\\":\\"', ':: ') %>% 
           str_replace_all('\\":', ':: ') %>% 
           str_c(collapse='   ')
  state = fields %>% 
          str_extract('state:: [A-Z][A-Z]') %>% 
          str_extract('[A-Z][A-Z]$')
  district = fields %>% 
             str_extract('district:: (\\d{1})+') %>% 
             str_extract('(\\d{1})+$') %>% 
             str_pad(2, side='left', pad='0') %>%
             paste(state, ., sep='-')
  party = fields %>% 
          str_extract('party:: [RDI]') %>% 
          str_extract('[RDI]')
  first_name = fields %>% 
               str_extract('first_name::.*gender::') %>% 
               str_split_fixed(' +', 3) %>% 
               .[,2]
  last_name = fields %>% 
              str_extract('last_name::.*middle_name::') %>% 
              str_split_fixed(' +', 3) %>% 
              .[,2]
  name = paste(first_name, last_name)
  info = c(representative=name, district=district, party=party)
  return(info)
}


# get_districts()
#############################################################################################
# Input: row, a data frame row containing the names of a data frame and a folder.
#
# Output: A list of character vectors containing congressional district information.

get_districts = function(row) {
  # Run query_sunlight on the rows of a data frame to download JSON files with congressional district information.
  a_ply(get(row$frame), 1, query_sunlight, row$folder)

  # Run parse_json on the downloaded JSON files to extract congressional district information into a character vector.
  return(llply(to_get(row$folder), parse_json))
}

# Run get_districts to create a two-element list containing congressional district information for each data frame.
districts = alply(to_do, 1, get_districts)
dennys_info = cbind(dennys_info, do.call(rbind, districts[[1]]), stringsAsFactors=F)
hotel_info = cbind(hotel_info, do.call(rbind, districts[[2]]), stringsAsFactors=F)

# Write the updated data frames to disk.
dir.create(paste0(to_put, 'Rdata'), recursive=T, showWarnings=F)
save(file=paste0(to_put, 'Rdata/districts.RData'), list=c('hotel_info', 'dennys_info'))
