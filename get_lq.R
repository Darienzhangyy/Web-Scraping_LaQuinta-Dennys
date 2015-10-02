rm(list=ls())
library(rvest)
library(magrittr)
library(stringr)
library(plyr)

to_get = read.csv('lq_states.csv', header=F)
to_put = 'data/lq/'
dir.create(to_put, recursive=T, showWarnings=F)
page = read_html('http://www.lq.com/en/findandbook/hotel-listings.html') 

#############################################################################################
# TO DO:                                                                                    #
# (1) For Denny's, use download.file(method='wget').                                        #
#############################################################################################

# scrape_list()
#############################################################################################
# Input: page, a parsed html page containing a list of all La Quinta hotels.
#
# Output: links, a character vector containing the URLs to every American La Quinta.

scrape_list = function(page) {
  # Match US state names from csv file to US state abbreviations on website to identify American hotels.
  places = page %>% 
           html_nodes(xpath='//option')
  states_rows = (places %>% html_text) %in% unlist(to_get)
  states = places[states_rows] %>% html_attr('value')
  
  # Extract <a> children of <div class="row"> nodes containing country abbreviations, 
  # state abbreviations, "Back To Top" links, and hotel links, among other things.
  rows = page %>% 
         html_nodes(xpath='//div[@class="row"]//a') 
  
  # Extract the country/state abbreviations and replace NA with "NA".
  rows_names = rows %>% 
              html_attr('name') %>% 
              str_replace_na

  # Extract the index from `rows` corresponding to the first entry after the US label.
  begin = str_detect(rows_names, '^US$') %>% 
          seq(length(rows))[.] %>% 
          sum(1)
  
  # Extract the index from `rows` corresponding to the last entry before the Mexico label.
  end = str_detect(rows_names, '^MX$') %>% 
        seq(length(rows))[.] %>% 
        sum(-1)
  
  # Extract the indexes from `rows` corresponding to the state abbreviations.
  state_names = seq(length(rows))[rows_names %in% states]
  
  # Extract the indexes from `rows` immediately following the state abbreviations, 
  # which correspond to the "Back To Top" links.
  state_names = sort(c(state_names, (state_names + 1)))
  
  # Extract the indexes from `rows` corresponding to all American hotels.
  state_rows = setdiff(seq(begin, end, 1), state_names)
  
  # Extract the links from the nodes indexed by `state_rows`.
  links = rows[state_rows] %>% 
          html_attr('href') %>% 
          paste0('http://www.lq.com', .) %>%
          as.list(.)
  
  return(links)
}


# html_download()
#############################################################################################
# Input: link, a character vector containing the URL to a La Quinta in the US.
#
# Output: Nothing; a webpage is downloaded and saved to disk as an html file.

html_download = function(link) {
  download.file(link, destfile=paste0(to_put, basename(link)), quiet=T)
  
  # Direct the computer to rest for a quarter of a second after downloading the webpage.
  Sys.sleep(0.25)
}


# Download the webpage for each La Quinta hotel in the US, at a rate of four per second.
scrape_list(page) %>% l_ply(.fun=html_download)
