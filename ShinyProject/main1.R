
# this programme was running on R vers. R-4.0.4.

# set working directory
# save "SimpleRandomWalk.R" and "Shiny.R" in a folder and set the working directory
#  in the following line
setwd('folder direction')

### run lines
source('SimpleRandomWalk.R')
source('Shiny.R')
### run app
shinyApp(ui, server)
