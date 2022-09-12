source("install.dependences.R")

library(shiny)

# By default, the file size limit is 5MB. It can be changed by
# setting this option. Here we'll raise limit to 50 MB.
options(shiny.maxRequestSize = 50 * 1024 ^ 2)

#Enable Cairo
#Cairo is a 2D graphics library with support for multiple output devices
options(shiny.usecairo = TRUE)

# options(shiny.port = 8080)
# options(shiny.host = "0.0.0.0")

runApp(getwd())

