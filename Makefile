all: hw2.html

hw2.html: hw2.Rmd data/dennys.Rdata data/lq.Rdata
	Rscript -e "library(rmarkdown);render('hw2.Rmd')"

data/dennys.Rdata: parse_dennys.R data/dennys/
	Rscript parse_dennys.R

data/dennys/: get_dennys.R dennys_coords.csv
	Rscript get_dennys.R

data/lq.Rdata: parse_lq.R data/lq/
	Rscript parse_lq.R

data/lq/: get_lq.R lq_states.csv
	Rscript get_lq.R

clean:
	rm -rf data/
	rm -f hw2.html

.PHONY: all clean
