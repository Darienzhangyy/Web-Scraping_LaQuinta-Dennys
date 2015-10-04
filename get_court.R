rm(list=ls())

library(rvest)
library(stringr)
library(magrittr)
library(MASS)

if(!(file.exists('districts.csv'))) {
  legislators = read.csv('legislators.csv', stringsAsFactors=F)
  legislators = legislators[which(legislators$title=='Rep' & legislators$in_office==1),]
  districts = str_pad(legislators$district, 2, 'left', '0') %>% 
    str_c(legislators$state, rep('-', nrow(legislators)), .) %>% 
    sort %>% 
    as.data.frame(stringsAsFactors=F)
  districts$party = legislators$party
  colnames(districts) = c('districts', 'party')
  rownames(districts) = districts$districts
  write.csv(districts, file='districts.csv', row.names=F)
} else {
  districts = read.csv('districts.csv', stringsAsFactors=F)
}

load('data/districts/Rdata/districts.RData')
lq_table = hotel_info$district %>% 
           table %>%  
           as.data.frame(stringsAsFactors=F)
colnames(lq_table) = c('districts', 'hotels')
rownames(lq_table) = lq_table$districts

dennys_table = dennys_info$district %>% 
               table %>%  
               as.data.frame(stringsAsFactors=F)
colnames(dennys_table) = c('districts', 'dennys')
rownames(dennys_table) = dennys_table$districts

congressional_district = merge(districts, lq_table, by='row.names', by.x='districts', by.y='districts', all.x=T)
congressional_district = merge(congressional_district, dennys_table, by='row.names', by.x='districts', by.y='districts', all.x=T)
congressional_district[is.na(congressional_district)] = 0
congressional_district$party_dummy = congressional_district$party
congressional_district$party_dummy[which(congressional_district$party=='R')] = 1
congressional_district$party_dummy[which(congressional_district$party=='D')] = 0
congressional_district$party_dummy = as.numeric(congressional_district$party_dummy)
congressional_district$has_both = 0
congressional_district$has_both[which(congressional_district$hotels>0 & congressional_district$dennys>0)] = 1
congressional_district$has_one = 0
congressional_district$has_one[which(congressional_district$hotels>0 | congressional_district$dennys>0)] = 1
summary(congressional_district$has_both)
summary(congressional_district$has_one)
sum(congressional_district$hotels[which(congressional_district$has_both==0)]) / sum(congressional_district$hotels)
sum(congressional_district$dennys[which(congressional_district$has_both==0)]) / sum(congressional_district$dennys)

cor(congressional_district$hotels, congressional_district$dennys)
cor(congressional_district$hotels, congressional_district$party_dummy)
cor(congressional_district$dennys, congressional_district$party_dummy)

# Evidence of overdispersion, so negative binomial better.
apply(congressional_district[,c('hotels', 'dennys')], 2, function(x) { c(mean(x), var(x)) } )

m1 = glm.nb(hotels ~ dennys, data=congressional_district)
new_data1 = data.frame(dennys=seq(min(congressional_district$dennys), max(congressional_district$dennys), 1))
new_data1$lq_hat = predict(m1, new_data1, type='response')
new_data1

m2 = glm.nb(hotels ~ dennys + party, data=congressional_district)
new_data2 = data.frame(dennys=seq(min(congressional_district$dennys), max(congressional_district$dennys), 1),
                       party='R')
new_data2$lq_hat = predict(m2, new_data2, type='response')
new_data2

m3 = glm.nb(dennys ~ hotels, data=congressional_district)
new_data3 = data.frame(hotels=seq(min(congressional_district$hotels), max(congressional_district$hotels), 1))
new_data3$dennys_hat = predict(m3, new_data3, type='response')
new_data3

m4 = glm.nb(dennys ~ hotels + party, data=congressional_district)
new_data4 = data.frame(hotels=seq(min(congressional_district$hotels), max(congressional_district$hotels), 1),
                       party='R')
new_data4$dennys_hat = predict(m4, new_data4, type='response')
new_data4

###########################3
scrape_district_court = function(row) {
  court = str_extract(row, '([A-Z][A-Z] \\d{5}$)|([A-Z][A-Z] \\d{5}(-| )\\d{4}$)') %>%
          str_extract('\\d{5}') %>%
          paste0('http://www.uscourts.gov/court-locator/zip/',
                 .,
                 '/court/district') %>%
          read_html %>% 
          html_nodes(xpath='//p[@class="grouped-court"]//a') %>% 
          html_text %>% 
          .[1] %>% 
          str_sub(start=3) %>% 
          str_trim
  Sys.sleep(0.25)
  return(court)
}
hotel_info[,1:6] = apply(hotel_info[,1:6], 2, unlist) %>% as.data.frame(stringsAsFactors=F)
hotel_info$court = llply(hotel_info$address, scrape_district_court) %>% 
                   unlist
dennys_info$court = llply(dennys_info$address, scrape_district_court) %>% 
                    unlist

lq_table = table(hotel_info$court) %>% 
               data.frame(stringsAsFactors=F)
colnames(lq_table) = c('court', 'hotel')
rownames(lq_table) = lq_table$court

dennys_table = table(dennys_info$court) %>% 
               data.frame(stringsAsFactors=F)
colnames(dennys_table) = c('court', 'dennys')
rownames(dennys_table) = dennys_table$court

federal_courts = read.csv('us_federal_courts.csv', header=F, stringsAsFactors=F)
district_courts = federal_courts[which(str_detect(federal_courts[,1], 'district_court')),2]
states = district_courts %>% 
         str_extract(' of .+$') %>% 
         str_sub(start=5)
region = district_courts %>% 
         str_extract(' for .*District') %>% 
         str_sub(start=5, end=-10)
district_courts = suppressWarnings(str_c(states, region, ' District Court'))
territories = str_detect(district_courts, 'Guam|Mariana|Puerto|(Virgin Islands)')
district_courts = district_courts[!territories] %>% 
                  as.data.frame(stringsAsFactors=F)
colnames(district_courts) = 'court'
district_courts = merge(district_courts, lq_table, by='row.names', by.x='court', by.y='court', all.x=T)
district_courts = merge(district_courts, dennys_table, by='row.names', by.x='court', by.y='court', all.x=T)
district_courts[is.na(district_courts)] = 0

district_courts$has_both = 0
district_courts$has_both[which(district_courts$hotel>0 & district_courts$dennys>0)] = 1
district_courts$has_one = 0
district_courts$has_one[which(district_courts$hotel>0 | district_courts$dennys>0)] = 1
summary(district_courts$has_both)
summary(district_courts$has_one)
sum(district_courts$hotel[which(district_courts$has_both==0)]) / sum(district_courts$hotel)
sum(district_courts$dennys[which(district_courts$has_both==0)]) / sum(district_courts$dennys)

cor(district_courts$hotel, district_courts$dennys)

save(file='data/districts/Rdata/districts.RData', list=c('hotel_info', 'dennys_info', 'district_courts', 'frequency'))

#######################
circuit_courts = list(`1`=c('ME', 'NH', 'MA', 'RI', 'PR'),
                      `2`=c('VT', 'NY', 'CT'),
                      `3`=c('PA', 'NJ', 'DE', 'VI'),
                      `4`=c('MD', 'WV', 'VA', 'NC', 'SC'),
                      `5`=c('MS', 'LA', 'TX'),
                      `6`=c('TN', 'KY', 'OH', 'MI'),
                      `7`=c('IN', 'IL', 'WI'),
                      `8`=c('AR', 'MO', 'IA', 'MN', 'ND', 'SD', 'NE'),
                      `9`=c('AZ', 'CA', 'NV', 'ID', 'MT', 'WA', 'OR', 'HI', 'AK', 'GU'),
                      `10`=c('OK', 'KS', 'CO', 'NM', 'UT', 'WY'),
                      `11`=c('AL', 'GA', 'FL'),
                      DC=c('DC'))
