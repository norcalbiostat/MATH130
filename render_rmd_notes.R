x <- list.files("notes/", pattern='*.Rmd|*.md', full.names=TRUE)

x <- x[!grepl("_notes|_slides", x)]

for(i in 1:length(x)){
  rmarkdown::render(x[i], output_format=c('html_document', 'pdf_document'))
}

