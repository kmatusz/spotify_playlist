#packages installer

requiredPackages <- c(
  'spotifyr',
  'tidyverse',
  'shiny',
  'DT',
  'corrplot',
  'sortable',
  'shinythemes',
  'shinyjs',
  'wordcloud2'
)

for (i in requiredPackages) {
  if (!require(i, character.only = TRUE))
    install.packages(i)
}

# Installing from github (not from CRAN) to enable removing playlist function
# (From PR 105)
{
  if (!require('spotifyr', character.only = TRUE))
    devtools::install_github("https://github.com/charlie86/spotifyr")
}
