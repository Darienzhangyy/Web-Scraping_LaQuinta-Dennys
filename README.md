[![wercker status](https://app.wercker.com/status/da56170c671b5a9cb298c1b8bba4874b/m "wercker status")](https://app.wercker.com/project/bykey/da56170c671b5a9cb298c1b8bba4874b)


get_lq.R

this script goes to La Quinta listing page and download the hotel pages for the states specified in lq_states.txt (which is detailed below). All of these hotel pages should be saved into the data/lq/ directory. If these folders do not exist they should be created.

parse_lq.R

this script reads all of the saved hotel pages in data/lq/ and construct an appropriate data frame where each row is a hotel and the appropriate columns. This data frame should be saved as lq.Rdata in the data/ directory.

get_dennys.R

this script downloads xml data from the Where2GetIt API (for the locations and radii specified in dennys_coords.csv) and save the results to data/dennys/. If these folders do not exist they should be created.

parse_dennys.R - this script reads all of the saved xml files in data/dennys/ and construct an appropriate data frame where each row is a restaraunt with the appropriate columns. This data frame should be saved as dennys.Rdata in the data/ directory.

hw2.Rmd - this document details how your group has chosen to implement your various download and parsing scripts. Additionally, it should wholly contain your distance analysis which load the necessary data directly (and exclusively) from data/lq.Rdata and data/dennys.Rdata.



Background

This observation is a joke made famous by the late comedian Mitch Hedberg. Last year, John Reiser on his blog detailed an approach to assess how true this joke actually is by scraping location data for all US locations of La Quinta and Denny’s. Your goal for this homework will be to recreate this analysis within R and expand on both the data collection and analysis.

Task 1 - Scraping La Quinta

The original blog post states that the location of all the La Quinta’s was obtained via hotelMarkers.js from La Quinta’s website which contains JSON data with the latitude and longitude of each location. This is enough to answer our question about location relative to Denny’s restaurants, but we would also like to have more data available such as street addresses, phone numbers, amenities, etc.

Instead, your task is to write code to scrape this data from the hotel listings page which conveniently includes a list and links to every La Quinta in the USA, Mexico, and Canada. Your minimum scraped data set should include location name, address, phone number, fax number, latitude, and longitude. The addition of hotel amenities and details such as internet availability, swimming pools, number of rooms, floors, etc will be considered for extra credit.

This data collection must be constructed in a reproducible fashion - all web pages being scraped should be cached locally and each analysis step should be self contained in a separate R script. Setup an appropriate Makefile to connect your R scripts together.

Finally, note that you should not abuse this or any other web page or API, make sure to space out your requests to avoid getting your or saxon’s IP banned.

Your write up should include a discussion of your scraping approach.

Task 2 - Scraping Denny’s

Scraping the Denny’s site is somewhat more complicated as it relies on a 3rd party service to display its locations. For this scraping task it is ok to use the same approach used in the blog post and directly pull results from the Where2GetIt web API. We will discuss in class how to identify and work with this specific API. Your core task will be to fetch and parse the XML files that result from the API calls and combining their results in R. Note that it is important to verify that these calls are sufficient to obtain all Denny’s locations, and your write up should include some discussion of this.

Once again, any web page or API result used should be cached locally and all analyses should be in self contained R scripts connected by a single Makefile.

Task 3 - Distance Analysis

Using the results of your scraping you should analyze the veracity of Hedberg’s claim. This is left as an open ended exercise - there is not one correct approach. This can include anything from visualizations to tabulations, but will hopefully be more than just a list of the La Quinta and Denny’s pairs that happen to be within an arbitrary radius.

Note that this analysis depends on calculating the distance between two spatial locations on a sphere, as such using euclidean distances is not ok. Make sure you use an appropriate approach for calculating any distances you use and make sure the units of distance are clearly indicated in your analysis.
