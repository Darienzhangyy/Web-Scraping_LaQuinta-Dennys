rm(list=ls())
library(data.table)
library(stringr)

load('data/dennys.Rdata')
load('data/lq.Rdata')
zip_to_cbsa = fread('zip07_cbsa06.txt')

lq_zips = str_extract_all(hotel_info$address, '\\d{5}') %>% llply(function(row) { row[length(row)] } ) %>% unlist
has_zip = str_detect(dennys_info$address, '\\d{5}') %>% llply(function(row) { row[length(row)] } ) %>% unlist
dennys_info = dennys_info[has_zip,]
d_zips = str_extract_all(dennys_info$address, '\\d{5}') %>% llply(function(row) { row[length(row)] } ) %>% unlist

