rm(list=ls())
library(rvest)
library(magrittr)
library(stringr)
library(plyr)

# Read in the html files to a list and remove the duplicate link if present.
to_get = list.files('data/lq/')
# Remove the null addresses if they are in 'to_get'
# Otherwise resave to_get as a list 
if('hotel-details.null.address.html' %in% to_get) {
  to_get = to_get[-match('hotel-details.null.address.html', to_get)] %>% as.list(.)
} else {
  to_get = as.list(to_get)
}
# Name variable to_put as directory 'data/'
to_put = 'data/'


# scrape_hotel()
#############################################################################################
# Input: page, the full html file for a particular La Quinta hotel's webpage.
#
# Output: info, a named list containing 8 character vectors:
#           $name, the name of the hotel;
#           $address, the address of the hotel;
#           $phone, the phone number of the hotel;
#           $fax, the fax number of the hotel;
#           $latitude, the latitude of the hotel location;
#           $longitude, the longitude of the hotel location;
#           $amenities, the features of the hotel and the room;
#           $details, the hotel details (# rooms, floors, etc.).

scrape_hotel = function(page) {
  # Parse the html.
  page = read_html(paste0('data/lq/', page))
  
  # Extract the hotel name.
  name = page %>% 
    html_nodes(xpath='//h1') %>% 
    html_text
  
  # Extract the raw address, phone number, and fax number into a vector, in that order.
  contact = page %>% 
    html_nodes(xpath='//div[contains(@class, "hotelDetailsBasicInfoTitle")]//p') %>% 
    html_text %>% 
    str_split(pattern='\n *\n *\n *') %>%
    unlist
  
  # Extract and clean the address, phone number, and fax number from the contact vector.
  address = str_split(contact[1], ' *\n *') %>% unlist %>% paste(collapse=' ')
  state   = suppressMessages(str_extract(address, perl('(?<=, )[A-Z][A-Z](?= \\d{5})')))
  zip     = suppressMessages(str_extract(address, perl('\\d{5}(?=(-\\d{4}|$))')))
  phone   = str_extract(contact[2], ' .*$') %>% str_trim
  fax     = str_extract(contact[3], ' .*$') %>% str_trim
  
  # Extract the latitude and longitude of the hotel and output as character vector.
  latlong = page %>% 
    html_nodes(xpath='//img[@class="minimap"]') %>% 
    html_attr('src') %>% 
    str_extract_all(pattern='-*(\\d{2}|\\d{3})\\.(\\d{1})*', simplify=T)
  
  # Extract the hotel and room features and output as character vector.
  amenities = page %>% 
    html_nodes(xpath='//div[contains(@class, "hotelDetailsFeaturesAmenity section")]//li') %>%
    html_text %>%
    str_extract_all(pattern='^ .* \\\n', simplify=T) %>%
    str_sub(start=2, end=-3)
  # Only extract the amenities that has string length greater than 0
  amenities = amenities[str_length(amenities)>0]
  
  # Extract the hotel details from lists and output as named character vector.
  details = page %>% 
    html_nodes(xpath='//div[contains(@class, "hotelFeatureList")]//li') %>%
    html_text %>%
    str_split(pattern=':') %>%
    lapply(FUN=str_trim) %>%
    lapply(FUN=function(x) { 
      detail = list(x[2:length(x)])
      names(detail) = x[1]
      return(detail)
    } ) %>%
    unlist(recursive=F) %>%
    lapply(FUN=paste, collapse=':') %>%
    unlist
  
  # Return info as a vector that contains previously saved variables
  return(info = list(name=name, 
                     address=address, 
                     state=state,
                     zip=zip,
                     phone=phone, 
                     fax=fax, 
                     latitude=latlong[1], 
                     longitude=latlong[2],
                     amenities=amenities,
                     details=details))
}

# Extract the relevant information from the hotel webpages into a data frame.
hotel_info = to_get %>% 
# Apply sapply function onto 'to_get', transpose and resave the resulting dataframe
  sapply(FUN=scrape_hotel) %>% 
  t %>% 
  as.data.frame(stringsAsFactors=F) 

# If scrape_hotel produces all 869 La Quinta hotels in the US, clean the data.
# But if hotel_info is produced by test_that, do not attempt to clean the data.
if(nrow(hotel_info)==869) {
  # Check NAs by column (summary won't cooperate with hotel_info).
  NA_count = unlist(lapply(hotel_info, function(col) { sum(is.na(unlist(col))) } ))
  
  # Check hotel names and addresses by visual inspection.
  unlist(hotel_info$name)
  unlist(hotel_info$address)
  
  # Check hotel phone numbers by adherence to "1-xxx-xxx-xxxx" pattern and correct as needed.
  phone_flaws = unlist(hotel_info$phone) %>% 
                str_detect(pattern='^1-\\d{3}-\\d{3}-\\d{4}$') %>% 
                !.
  sum(phone_flaws)
  unlist(hotel_info$phone)[phone_flaws]
  
  # Replacing " " with "-" corrects all flaws.
  hotel_info$phone[phone_flaws] = hotel_info$phone[phone_flaws] %>% 
                                  lapply(FUN=function(x) { str_replace(x, pattern=' ', replacement='-') } )
  phone_flaws = unlist(hotel_info$phone) %>% 
                str_detect(pattern='^1-\\d{3}-\\d{3}-\\d{4}$') %>% 
                !.
  sum(phone_flaws)
  
  # Check hotel fax numbers by adherence to "1-xxx-xxx-xxxx" pattern and correct as needed.
  fax_flaws = unlist(hotel_info$fax) %>% 
              str_detect(pattern='^1-\\d{3}-\\d{3}-\\d{4}$') %>% 
              !.
  sum(fax_flaws)
  unlist(hotel_info$fax)[fax_flaws]
  
  # Replacing " " with "-" corrects most - but not all - flaws.
  hotel_info$fax[fax_flaws] = hotel_info$fax[fax_flaws] %>% 
                              lapply(FUN=function(x) { str_replace(x, pattern=' ', replacement='-') } )
  fax_flaws = unlist(hotel_info$fax) %>% 
              str_detect(pattern='^1-\\d{3}-\\d{3}-\\d{4}$') %>% 
              !.
  sum(fax_flaws)
  unlist(hotel_info$fax)[fax_flaws]
  
  # Inserting "-" in the last seven digits corrects one more flaw.
  front = str_extract(hotel_info$fax[fax_flaws][[1]], '^1-\\d{3}-\\d{3}')
  back = str_extract(hotel_info$fax[fax_flaws][[1]], '\\d{4}$')
  hotel_info$fax[fax_flaws][[1]] = str_c(front, back, sep='-')
  
  # Trimming leading zeroes corrects one more flaw.
  hotel_info$fax[fax_flaws][[4]] = str_sub(hotel_info$fax[fax_flaws][[4]], 3)
  fax_flaws = unlist(hotel_info$fax) %>% 
              str_detect(pattern='^1-\\d{3}-\\d{3}-\\d{4}$') %>% 
              !.
  sum(fax_flaws)
  
  # Three remaining flaws involve missing data; convert to NAs.
  unlist(hotel_info$fax)[fax_flaws]
  to_get[fax_flaws] %>% unlist
  hotel_info$fax[fax_flaws] = NA
  
  # Check hotel names and addresses by summary inspection and correct as needed.
  unlist(hotel_info$latitude) %>% 
    as.numeric %>% 
    summary
  unlist(hotel_info$longitude) %>% 
    as.numeric %>% 
    summary
  
  # Check hotel amenities and details by summary inspection and correct as needed.
  hotel_info$amenities %>% 
    lapply(FUN=length) %>% 
    unlist %>% 
    summary
  hotel_info$details %>% 
    lapply(FUN=length) %>% 
    unlist %>% summary
  
  # The hotel details are not consistent.
  hotel_info$details %>% 
    lapply(FUN=names) %>% 
    unique
  details_names = hotel_info$details %>% 
                  lapply(FUN=names) %>% 
                  unlist %>% 
                  unique
  
  # Check the inconsistent entries by visual inspection.
  details = hotel_info$details %>% 
            lapply(FUN=names) %>% 
            lapply(FUN=function(x) { !identical(unlist(x), details_names) } ) %>%
            unlist
  hotel_info$details[details]
  
  # Define and call a function to standardize hotel details.
  standardize_details = function(hotel) {
    hotel = unlist(hotel)
    if(!identical(names(hotel), details_names)) {
      if(!('Floors' %in% names(hotel))) {
        hotel = c(hotel, Floors=NA)
      } else if(!('Suites' %in% names(hotel))) {
        hotel = c(hotel, Suites=NA)
      }
      hotel = hotel[details_names]
    }
    hotel = list(hotel)
  }
  hotel_info$details = lapply(hotel_info$details, standardize_details) %>% 
                       unlist(recursive=F)
  hotel_info$details[details]
}

# Unlist columns into character vectors.
to_characters = which(colnames(hotel_info) %in% c('amenities', 'details'))
hotel_info = apply(hotel_info[,-to_characters], 2, unlist) %>% 
  as.data.frame(stringsAsFactors=F) %>%
  cbind(hotel_info[,c('amenities', 'details')])

# Write the data frame to disk and save it as 'lq.Rdata'.
save(file=paste0(to_put, 'lq.Rdata'), list=c('hotel_info'))
