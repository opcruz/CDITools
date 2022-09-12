# install dependences

if(!require(devtools)) {
  install.packages("devtools")
}
if (!require(shiny)) {
  install.packages("shiny")
}
if (!require(shinydashboard)) {
  install.packages("shinydashboard")
}
if (!require(shinydashboardPlus)) {
  install.packages("shinydashboardPlus")
}
if (!require(XML)) {
  install.packages("XML")
}
if (!require(shinyjs)) {
  install.packages("shinyjs")
}
if (!require(visNetwork)) {
  install.packages("visNetwork")
}
if (!require(threejs)) {
  install.packages("threejs")
}
if (!require(shinycssloaders)) {
  install.packages("shinycssloaders")
}
if (!require(DT)) {
  install.packages("DT")
}
if (!require(Cairo)) {
  install.packages("Cairo")
}
if (!require(RColorBrewer)) {
  install.packages("RColorBrewer")
}
if (!require(igraph)) {
  install.packages("igraph")
}
if (!require(multinet)) {
  install.packages("multinet")
}
if (!require(MUNA)) {
  library(devtools)
  devtools::install_github("Issamfalih/MUNA")
}
if (!require(rCharts)) {
  library(devtools)
  devtools::install_github("ramnathv/rCharts")
}

# libraries for similarity module

if (!require(rje)) {
  install.packages("rje")
}
if (!require(netdiffuseR)) {
  install.packages("netdiffuseR")
}
if (!require(scales)) {
  install.packages("scales")
}
if (!require(networkD3)) {
  install.packages("networkD3")
}
if (!require(plyr)) {
  install.packages("plyr")
}
