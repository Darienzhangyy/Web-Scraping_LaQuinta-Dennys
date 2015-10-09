rm(list=ls())
library(rvest)
library(magrittr)
library(stringr)
library(plyr)

# Read in the xml files to a list.
directory = 'data/dennys/'
to_get = list.files(directory) %>% 
         paste0(directory, .) %>% 
         as.list(.)
# Name to_put as directory 'data/'
to_put = 'data/'

# Create a logical vector for subsetting the unique store ID numbers. 
store_ids = llply(to_get, function(set) { read_xml(set) %>% 
                                          xml_nodes(xpath='//clientkey') %>% 
                                          xml_text } ) %>% 
            unlist %>% 
            duplicated %>% 
            !.

# Extract the results nodes for the unique Dennys locations.
# llply takes each element of a list, apply function, and keeps results as a list
store_nodes = llply(to_get, function(set) { read_xml(set) %>%
                                            xml_nodes(xpath='//poi') } ) %>%
              unlist(recursive=F)
#subset store_nodes according to store_ids and rename it as store_nodes
store_nodes = store_nodes[store_ids]


# scrape_tag()
#############################################################################################
# Input: tag, a particular xml tag whose value is to be extracted;
#        result, a particular result node.
#
# Output: the value of the xml tag.

scrape_tag = function(tag, result) {
  result %>% 
    xml_node(xpath=tag) %>% 
    xml_text
}
#Name tags as a list of following elements
tags = as.list(c('clientkey', 'address1', 'city', 'state', 'postalcode', 'phone', 'latitude', 'longitude'))


# scrape_dennys()
#############################################################################################
# Input: result, a particular result node.
#
# Output: a character vector with five elements:
#         (1) clientkey, a unique store ID;
#         (2) address, the address of the Dennys location;
#         (3) phone, the phone number of the Dennys location;
#         (4) latitude, the latitude of the Dennys location;
#         (5) longitude, the longitude of the Dennys location.

scrape_dennys = function(result) {
  values = llply(tags, scrape_tag, result=result) %>% 
           unlist
  address = paste0(values[2:4], collapse=', ') %>% 
            paste(., values[5])
  return(c(values[1], address, values[6:8]))
}

# Extract the relevant information from the search results into a data frame.
dennys_info = ldply(store_nodes, scrape_dennys)
colnames(dennys_info) = c('clientkey', 'address', 'phone', 'latitude', 'longitude')

# Subset the data frame to include only US locations (those with a state abbreviation and ZIP code).
has_zip = str_detect(dennys_info$address, '([A-Z][A-Z] \\d{5}$)|([A-Z][A-Z] \\d{5}(-| )\\d{4}$)') %>% 
          llply(function(row) { row[length(row)] } ) %>% 
          unlist
# subset dennys_info according to has_zip and rename it as dennys_info
dennys_info = dennys_info[has_zip,]

# Write the data frame to disk and save it as 'dennys.Rdata'.
save(file=paste0(to_put, 'dennys.Rdata'), list=c('dennys_info'))
