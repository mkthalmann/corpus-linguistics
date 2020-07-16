.PHONY: all fresh cleanall
all: data/messages_cleaned.csv report.html scripts/report.R
cleanall: cleancache cleanfigs cleandata cleanreport
fresh: cleanall data/messages_cleaned.csv report.html scripts/report.R

scripts/report.R: scripts/purl.R scripts/report.Rmd
	Rscript $<

report.html: scripts/report.Rmd assets/* data_raw/*
	Rscript -e "library(here);rmarkdown::render('$<', output_dir = here())"

data/messages_cleaned.csv: scripts/parse.py data_raw/_chat.txt
	python3 -u scripts/parse.py

cleancache:
	-rm -rf cache/*

cleanfigs:
	-rm -rf figs/*

cleandata:
	-rm rf data/*.csv
	-rm rf data/*.txt

cleanreport:
	-rm rf report.html
