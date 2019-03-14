# Classifying Satellite Imagery in R

<br>

Different materials reflect and absorb wavelengths differently. Remote sensing utilizes this phenomena to capture a picture of the Earth's surface by detecting how various forms of ground cover reflect solar radiation.

In the diagram below for example, the built-up area, bare soil, and forest will absorb the sun's radiation differently and reflect it in a way that makes them unique to the sensors on the satellite. We can then view these differences in the spectral bands of satellite imagery.

<center>

<img src="/images/remoteSensingDiagram-01.png" style="width:75px; height:40px"> Source: [Centre for Remote Imaging, Sensing & Processing](https://crisp.nus.edu.sg/~research/tutorial/optical.htm)></img>

</center>

<br>

Raw satellite imagery, however, is not necessarily beneficial when performing various analyses. To get useful inputs about land cover in an area, we must transform the imagery. One way to do this is to classify the imagery into categories that we are interested in.

This tutorial introduces using rasters and classifying imagery in R. It is based on a [similar tutorial](https://gfc.ucdavis.edu/events/arusha2016/_static/labs/day4/day4_lab1_remote-sensing.pdf) from UC Davis. We will cover:

- [accessing the properties of a raster in R](#exploring-the-imagery)
- [calculating Normalized Difference Vegetation Index (NDVI)](#calculating-normalized-difference-vegetation-index)
- [supervised classification](#supervised-classification)

For this tutorial, we use Landsat 8 imagery from Calgary, which can be found on [here](https://github.com/urbanSpatial/classifying_satellite_imagery_in_R/tree/master/data). However, the process can be repeated with any Landsat 8 imagery downloaded from either [Earth Explorer](https://earthexplorer.usgs.gov/) or [other sites](https://remotepixel.ca/projects/satellitesearch.html).

## Loading packages and preparing the data

Let's begin by loading in the necessary libraries.

```{r packages}
library(raster)
library(tidyverse)
library(sf)
library(rpart)
library(rpart.plot)
library(rasterVis)
library(mapedit)
library(mapview)
library(caret)
```

Next, we read in the different bands that comprise the satellite imagery. Each band refers to a different spectrum:

```{r echo=FALSE}
data.frame(Band = c("Band 1", "Band 2", "Band 3", "Band 4", "Band 5", "Band 6", "Band 7", "Band 8"),
           Description = c("Coastal aersol", "Blue", "Green", "Red", "Near Infrared (NIR)", "Shortwave Infrared (SWIR) 1", "Shortwave Infrared (SWIR) 2", "Panchromatic")) %>%
  kable() %>%
  kable_styling()
```

```{r}
band1 <- raster("cgband1.tif")
band2 <- raster("cgband2.tif")
band3 <- raster("cgband3.tif")
band4 <- raster("cgband4.tif")
band5 <- raster("cgband5.tif")
band6 <- raster("cgband6.tif")
band7 <- raster("cgband7.tif")
band8 <- raster("cgband8.tif")
```

To perform any analysis, however, we need to combine the individual bands into one multi-band raster. The `stack` function in the `raster` package will create one raster from the layers that are inputted, similar to the composite bands function in ArcGIS.

Note that band 8, the panchromatic image, is calculated at a different resolution (15 meters rather than 30 meters) than the other bands.

```{r}
res(band8)
```

If we try to `stack` this band with the others, it returns an error. We `aggregate` the cell size to 30 meters prior to stacking the rasters into one image.

```{r}
band8 <- aggregate(band8, fact = 2)

image <- stack(band1, band2, band3, band4, band5, band6, band7, band8)
```

## Exploring the imagery

There are several properties of the raster we can access in R that would normally be found in the properties window in ArcGIS. Below, we look at the number of layers (or bands) the raster has, the coordinate system the imagery is projected in, and the resolution (or grid cell size) of the raster.

```{r}
nlayers(image)
crs(image)
res(image)
```

Now that we know a little more about the imagery we are using, let's plot it. Since `image` is a multi-band raster, we use the `plotRGB` function from the `raster` package, which allows us to specify what bands should be visualized.

There are two main composites that are normally used in remote sensing: the true color composite and the false color composite. As the name suggests, the true color composite makes the imagery appear how it would to the human eye. It uses the red band (4) for red, the green band (3) for green, and the blue band (2) for blue.

```{r}
par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 4, g = 3, b = 2, axes = TRUE, stretch = "lin", main = "True Color Composite")
box(col="white")
```

<img src="/images/truepositive.png"></img>

The false color composite is popular in remote sensing because it makes vegetation appear red. Below we use NIR (5) for red, red (4) for green, and green (3) for blue.

```{r}
par(col.axis="white",col.lab="white",tck=0)
plotRGB(image, r = 5, g = 4, b = 3, axes = TRUE, stretch = "lin", main = "False Color Composite")
box(col="white")
```

<img src="/images/falsepositive.png"></img>

The plot shows dense vegetation in red and areas of more developed land in blue. When assessing if this is an accurate representation, it is important to consider the time of year. This imagery was taken in the winter and there are several pockets of open space that appear brown. These areas could have more vegetation in other parts of the year.

## Calculating Normalized Difference Vegetation Index

NDVI provides another way to identify vegetation and is calculated on a scale of -1 to 1, where values closer to 1 indicate more vegetative cover. The calculation is based on how pigments in vegetation absorb sunlight compared to other ground cover.  

Here, we define a function, `vi`, that will calculate the vegetation index for a specified image using two bands (NIR and red) as inputs.

```{r}
vi <- function(img, i, k) {
  bi <- img[[i]]
  bk <- img[[k]]
  vi <- (bk - bi)/(bk + bi)
  return(vi)
}

ndvi <- vi(image, 4,5)
```

To plot the results with `ggplot`, we convert the raster into a data frame and use `geom_tile`.

```{r fig.width = 4.5, fig.height=5, fig.align="center"}
ndvi_forplotting <- as(ndvi, "SpatialPixelsDataFrame") %>% as.data.frame()

ggplot() +
  geom_tile(data = ndvi_forplotting, aes(x = x, y = y, fill = layer)) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "NDVI for Calagary",
       x = " ",
       y = " ") +
  scale_fill_gradient(low = "#CEE50E",
                      high = "#087F28",
                      name = "NDVI")
```

<img src="/images/ndvi.png"></img>

If we compare the result to the false color composite created above, the areas with a higher NDVI correspond to the pockets of red that indicate more vegetation.

## Supervised Classification

Classifying the Landsat 8 imagery will transform the satellite imagery into useable information. Once we know what grid cells represent what land cover, we will be able to feature engineer several variables, such as whether a grid cell is water or not and distance from forested areas. These features could prove helpful in various land use models.

Supervised classification is the process of training a predictive model on 'ground-truthed' land cover observations (pixels we know definitively are forest or farmland) and subjecting that model onto new data to predict the land cover class.

We use `mapedit` and `mapview` to create the training dataset of ground-truthed points. `mapview` provides an interactive way to view spatial data in R. In the code below, `viewRGB` allows us to view the true composite of the satellite imagery in mapview and `editMap` will provide the tools to drop points down on the map. When we click 'done' in the mapview window the points we create will be saved as `points`.

Note that there is a 'show in new window' button at the top of the Viewer pane in R that will open mapview in an internet browser. This will make it easier to zoom in and drop points at the correct locations.

We will repeat this process four times, each time ground-truthing a different category we are interested in. First, we will classify cloud cover, then developed land, next undeveloped land, and finally water.

As we create the classification points, it is important to keep a few things in mind:

1. In the end, we should have a similar number of classification points for each category.

2. The points should be distributed relatively equally over the study area.

For this example, I have created about 150 points for each category. When creating the points for undeveloped land, it may helpful to view the false color composite to make vegetation stand out as red.

```{r eval=FALSE}
points <- viewRGB(image, r = 4, g = 3, b = 2) %>% editMap()
```

`points` is returned as a list. The code below extracts the final classification points for each type, converts the training points to an sf object, and adds two fields, `class` and `id`.

```{r eval = FALSE}
clouds <- points$finished$geometry %>% st_sf() %>% mutate(class = "clouds", id = 1)
developed <- points$finished$geometry %>% st_sf() %>% mutate(class = "developed", id = 2)
undeveloped <- points$finished$geometry %>% st_sf() %>% mutate(class = "undeveloped", id = 3)
water <- points$finished$geometry %>% st_sf() %>% mutate(class = "water", id = 4)
```

Next, we `rbind` the different land cover categories together to create a final `training_points` sf object.

```{r eval = FALSE}
training_points <- rbind(clouds, developed, undeveloped, water)
```

You can write these points as a shapefile with `write_sf(training_points, "calgary_trainingPoints.shp", driver = "ESRI shapefile", getwd())` to be read back in later with `training_points <- st_read("calgary_trainingPoints.shp")`.

```{r include = FALSE}
training_points <- st_read("calgary_trainingPoints.shp")
```

### Extracting spectral values from the raster

Now that we have our training data, we will extract the spectral values from the imagery at the point locations. This requires that the points be formatted as a `SpatialPointsDataFrame`.

```{r}
training_points <- as(training_points, 'Spatial')

df <- raster::extract(image, training_points) %>%
  round()
```

The result is a matrix of values for each spectral band.

```{r}
head(df)
```

### Exploratory analysis: Plotting the spectral profile

As discussed above, different land covers will reflect and absorb the sun's radiation uniquely. A 'spectral profile' explores these differences by examining how the reflectance value for each band (spectrum) changes for different land cover types. Each land classification will have a unique spectral profile, with land cover like water having lower reflectance and features such as clouds having greater reflectance.

Here, we are looking at the spectral profile of the ground-truthed points. If we ground-truthed the data appropriately, we should see that each land classification has a different spectral profile. This has important implications for our model. If the spectral profiles of our training data are not unique, it is unreasonable to expect the model to identify differences between land cover.

The spectral profiles are created by taking the mean reflectance values for each land classification for every band. To do this, we create a blank matrix that we then fill with the mean values.

```{r matrix}
matrix <- matrix(nrow = length(unique(training_points$id)), ncol = nlayers(image))

for (i in unique(training_points$id)){
  x <- df[training_points$id==i,]
  matrix[i,] <- colMeans(x)
}

rownames(matrix) <- unique(training_points$class)
colnames(matrix) <- names(image)

head(matrix)
```

We then transform the matrix into a data frame and reshape the data to plot our results with `ggplot`.

```{r fig.height=4, fig.width = 8.5}
matrix %>%
  as.data.frame() %>%
  gather() %>%
  mutate(class = rep(c("clouds", "developed", "undeveloped", "water"), 8)) %>%
  ggplot(data = ., aes(x = as.factor(key), y = value,
                           group=class, color = class)) +
  geom_point(size = 2.5) +
  geom_line(lwd = 1.2) +
  scale_color_manual(values=c('cyan', 'burlywood', 'darkgreen', 'blue')) +
  labs(title = "Spectral Profile from Landsat 8 Imagery",
       x = "Bands",
       y = "Reflectance") +
  scale_y_continuous(limits=c(5000, 15000)) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray", size = 0.5),
        panel.grid.minor = element_line(color = "gray", size = 0.5),
        axis.ticks = element_blank())
```

<img src="/images/spectralprofile.png"></img>

### Classifying the imagery

We perform supervised classification on the imagery employing a decision tree algorithm. This type of algorithm uses a ‘tree’ structure to identify the best model that fits the data.

Decision trees start with the best predictor and continually subset the data. The if/else ‘decisions’ are represented as ‘nodes’ and the outcomes from the decisions become ‘leaves’ on the tree. The algorithm will use these rules to determine what the predicted outcome should be.

First, combine the class and the extracted values into a dataframe. Then using `rpart`, we train the model.

```{r}
df <- data.frame(training_points$class, df)

model.class <- rpart(as.factor(training_points.class)~., data = df, method = 'class')
```

Next, we plot the decision tree.

```{r}
rpart.plot(model.class, box.palette = 0, main = "Classification Tree")
```

Using the model, we predict on the entire image and set the classes to our four categories.

```{r results='hide'}
pr <- predict(image, model.class, type ='class', progress = 'text') %>%
  ratify()

levels(pr) <- levels(pr)[[1]] %>%
  mutate(legend = c("cloud","developed","undeveloped","water"))
```

Below are the results.

```{r}
levelplot(pr, maxpixels = 1e6,
          col.regions = c('cyan', 'burlywood', 'darkgreen', 'blue'),
          scales=list(draw=FALSE),
          main = "Supervised Classification of Imagery")
```

<img src="/images/finalclassification.png"></img>

Are these results reasonable? We evaluate how well the model predicted by creating a confusion matrix that compares the predicted class to the observed ('ground-truthed') class.

```{r}
test <- raster::extract(pr, training_points) %>%
  as.data.frame() %>%
  rename(id = ".")

testProbs <- data.frame(
  obs = as.factor(training_points$id),
  pred = as.factor(test$id)
) %>%
  mutate(correct = ifelse(obs == pred, 1, 0))

confMatrix <- confusionMatrix(testProbs$obs, testProbs$pred)
confMatrix
```

In the confusion matrix, _Sensitivity_ refers to the grid cells that were predicted to be a specific land cover and were actually that land cover. On average, across all four categories, we predicted `r  round((mean(confMatrix$byClass[[1]]) * 100),1)`% of ground-truthed points as such.

The confusion matrix also tells us that overall, we predicted `r confMatrix$overall[[1]]`, or 89.3%, of the sample points correctly.
