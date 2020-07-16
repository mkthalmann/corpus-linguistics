library(tidyverse)
library(knitr)
library(styler)
library(here)

# get all the files
results <- list.files(
  path = here("scripts/"), no.. = T,
  pattern = ".Rmd$"
)

# extract the R code
for (result in results) {
  tempfile <- gsub(".Rmd$", ".R", result)
  purl(here("scripts", result),
    output = here("scripts", tempfile)
  )
}

# grab all the scripts again to style the code
resultsstyle <- list.files(
  path = here("scripts"), no.. = T,
  pattern = ".R$"
)

# style the scripts -> add spaces and reindent
style_file(here("scripts", resultsstyle), scope = "tokens")
