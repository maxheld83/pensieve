FROM rocker/verse:3.5.2

RUN apt-get update \
  && apt-get install -y \
  pdf2svg \
  libudunits2-dev \
  libpoppler-cpp-dev

RUN install2.r \
  checkmate \
  plotly \
  DT \
  ggrepel \
  grImport2 \
  ggraph \
  lettercase \
  printr \
  pdftools \
  shinyjs \
  shinyBS \
  shinydashboard \
  svglite \
  inline \
  qmethod
