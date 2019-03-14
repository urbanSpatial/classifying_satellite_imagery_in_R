# Classifying Satellite Imagery in R

<br>

Different materials reflect and absorb wavelengths differently. Remote sensing utilizes this phenomena to capture a picture of the Earth's surface by detecting how various forms of ground cover reflect solar radiation.

Raw satellite imagery, however, is not necessarily beneficial when performing various analyses. To get useful inputs about land cover in an area, we must transform the imagery. One way to do this is to classify the imagery into categories that we are interested in.

[This tutorial](https://urbanspatial.github.io/classifying_satellite_imagery_in_R/) introduces using rasters and classifying imagery in R. It is based on a [similar tutorial](https://gfc.ucdavis.edu/events/arusha2016/_static/labs/day4/day4_lab1_remote-sensing.pdf) from UC Davis. We will cover:

- [accessing the properties of a raster in R](#exploring-the-imagery)
- [calculating Normalized Difference Vegetation Index (NDVI)](#calculating-normalized-difference-vegetation-index)
- [supervised classification](#supervised-classification)

For this tutorial, we use Landsat 8 imagery from Calgary, which can be found on [here](https://github.com/urbanSpatial/classifying_satellite_imagery_in_R/tree/master/data). However, the process can be repeated with any Landsat 8 imagery downloaded from either [Earth Explorer](https://earthexplorer.usgs.gov/) or [other sites](https://remotepixel.ca/projects/satellitesearch.html).

We perform supervised classification on the imagery employing a decision tree algorithm. Supervised classification is the process of training a predictive model on ‘ground-truthed’ land cover observations (pixels we know definitively are forest or farmland) and subjecting that model onto new data to predict the land cover class.

Here are the results.

<img src="/images/finalclassification.png"></img>
