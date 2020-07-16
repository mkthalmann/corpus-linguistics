
# Corpus Linguistics and Text Mining

This project analyses a one-year encompassing WhatsApp chat log totalling about 33k messages using various methods related to
corpus linguistics and approaches to text mining. Though the main analysis was carried out in R (and
converted to `.html` using R Markdown <sup id="a1">[1](#f1)</sup> and Pandoc <sup
id="a2">[2](#f2)</sup>), the data were initially processed using Python 3 (`scripts/parse.py`).

The output of the analysis can be found in `report.html` and features both a description and
interpretation of the methods and results as well as the R code used (the underlying source file is
`scripts/report.Rmd`). Some regular expression patterns and word lists external to the actual code
are contained in the `assets` directory. All figures are also available separately (in the `figs`
directory) and some of the analysis results were exported to the `data` directory.

For privacy reasons, the underlying message log is not accessible here.

<b id="f1">1</b> https://rmarkdown.rstudio.com [↩](#a1)

<b id="f2">2</b> https://pandoc.org [↩](#a2)

-----

If you find any mistakes, or have suggestions for improvements or additions, please send me an
[e-mail](mailto:maik.thalmann@gmail.com?subject=%5BGitHub%5D%20Corpus).
