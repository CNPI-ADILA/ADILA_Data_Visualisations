# HOTspots
The goal of HOTspots is to display the data about antimmicrobial resistance in the Top End in a way that is easy to understand and useable for the audience. The audience nicludes members of the public, medical professionals and policy/decision makers. 

## Outline of the project

### Content
- Patterns of drug resistance in space and time
- Rural and regional areas
- Local rates

### Audience
- Local stakeholder
- Prescribers and doctors
- Policy and guideline makers

### Adjectives
- user friendly
- Sophistocated

## The Specifications from Teresa
### General
- navigation bar
- Opening page as heat map
- region and age plots
- methodology
- reports
- links and news
- Terms of use, data disclaimer
- data
- options to share


### Heat map
- statistical area three
- grey out options of microbe/antimicrobe that dont exist or we have no data on

### Plots and tables
- region and age plots

### Download report
- pdf
- including summary statistics, plots, antibiogram

### Options to add later
Other options that might be important or interesting
- age
- social disadvantage
- remoteness index
- household structure
- gender
- international areas


## The app

### Dependencies
`shiny` for building the shiny app.  
`shinythemes` to provide a pre-made theme.  
`ggplot2` for the plots.  
`dplyr` for data cleaning and manipulation.  
`leaflet` for the maps.  
`rgdal` to handle shapefiles.  
`reshape2` to modify the strucutre of dataframes.  
`shinydashboard` for the shiny app.  
`shinyWidgets` for new shiny widgets.  
`shinycssloaders` to implement spinners when plots are loading.  
`shinyjs` to allow for extra modifcation in shiny without using javascript - e.g. `shinyjs::delay`.  

During development, the app will be hosted on http://alysy.shinyapps.io/hotspots/ and when finished it will be uploaded to a Menzie's server

## The files
`app.R` contains the shinny app ui and server.  
The images and logos are in the 'www' folders.  
The data is stored in www > data
The csv files of data contain variations on the data supplied by aggregating over different groups.  
The `data_manipulation.R` file takes the data provided by HOTspots, cleans and modifies it to create the multiple csv files.  
www > data > Australian_regions contains the shapefiles of the regions in Australia which are mapped. It also contains a csv file with the locations of Australia cities to add as points to the leaflet maps for more geographical context.  


## The data
The data is supplied by the HOTspots team; currently the member that supplies the data is Will Cunningham. The data is saved to a dropbox folder which is shared with Alys Young.  
The data is comprised of 2 csv files; `HOTspots_yearly_age&sex.csv` and `HOTspots_monthly.csv`. The `HOTspots_yearly_age&sex.csv` file contains annual antimicrobial resistance data split for each age and sex category. The `HOTspots_monthly.csv` file contains monthly resistance data. 
