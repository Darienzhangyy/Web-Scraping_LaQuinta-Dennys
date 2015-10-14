rm(list=ls())
library(rvest)
library(magrittr)
library(stringr)
library(plyr)

#Read data from 'lq_states.csv' 
to_get = read.csv('lq_states.csv', header=F)
#Name to_put as directory 'data/lq' 
to_put = 'data/lq/'
#Create the directory to_put 
dir.create(to_put, recursive=T, showWarnings=F)
#Save hotel websites into a variable named page
page = read_html('http://www.lq.com/en/findandbook/hotel-listings.html') 


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
  
  # Extract the index from `rows` corresponding to the Mexico label.
  end = str_detect(rows_names, '^MX$') %>% 
    seq(length(rows))[.]

  # Extract the indexes from `rows` corresponding to the start of each state's hotel list.
  state_starts = seq(begin, end, 1)[rows_names[seq(begin, end, 1)] %in% states] + 2
  
  # Extract the indexes from `rows` corresponding to the end of each state's hotel list.
  state_abbrs = rows_names[seq(begin, end, 1)][rows_names[seq(begin, end, 1)]!='NA']
  next_states = state_abbrs[which(state_abbrs %in% states) + 1]
  state_ends = seq(begin, end, 1)[rows_names[seq(begin, end, 1)] %in% next_states] - 1
  
  # Extract the indexes from `rows` corresponding to the indicated hotels.
  state_rows = apply(cbind(state_starts, state_ends), 1, function(row) { seq(row[1], row[2], 1) } ) %>%
                 unlist
  
  # Extract the links from the nodes indexed by `state_rows`.
  links = rows[state_rows] %>% 
          html_attr('href') %>% 
          paste0('http://www.lq.com', .) %>%
  # Return results as lists
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
  
  # Direct the computer to rest for 60 seconds after downloading the webpage.
  Sys.sleep(60)
}


# Download the webpage for each La Quinta hotel in the states listed in lq_states.csv.
# l_ply takes each element of a list, apply function and discard results
scrape_list(page) %>% l_ply(.fun=html_download)
