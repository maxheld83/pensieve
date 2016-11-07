setwd("book")
bookdown::render_book(input = 'index.Rmd', output_format = 'bookdown::gitbook', output_dir = '../_book')
bookdown::render_book(input = 'index.Rmd', output_format = 'bookdown::pdf_book', output_dir = '../_book')
bookdown::render_book(input = 'index.Rmd', output_format = 'bookdown::epub_book', output_dir = '../_book')
