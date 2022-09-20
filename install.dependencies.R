# install dependencies

if (!require(shiny)) {
  install.packages("shiny", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(shinydashboardPlus)) {
  install.packages("shinydashboardPlus", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(XML)) {
  install.packages("XML", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(shinyjs)) {
  install.packages("shinyjs", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(visNetwork)) {
  install.packages("visNetwork", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(threejs)) {
  install.packages("threejs", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(shinycssloaders)) {
  install.packages("shinycssloaders", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(DT)) {
  install.packages("DT", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(Cairo)) {
  install.packages("Cairo", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(RColorBrewer)) {
  install.packages("RColorBrewer", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(igraph)) {
  install.packages("igraph", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(multinet)) {
  install.packages("multinet", repos='https://cloud.r-project.org/', dependencies = TRUE)
}

if(!require(devtools)) {
  install.packages("devtools", repos='https://cloud.r-project.org/', dependencies = TRUE)
}

if (!require(MUNA)) {
  library(devtools)
  devtools::install_github("Issamfalih/MUNA", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(rCharts)) {
  library(devtools)
  devtools::install_github("ramnathv/rCharts", repos='https://cloud.r-project.org/', dependencies = TRUE)
}

# libraries for similarity module

if (!require(rje)) {
  install.packages("rje", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(netdiffuseR)) {
  install.packages("netdiffuseR", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(scales)) {
  install.packages("scales", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(networkD3)) {
  install.packages("networkD3", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
if (!require(plyr)) {
  install.packages("plyr", repos='https://cloud.r-project.org/', dependencies = TRUE)
}
