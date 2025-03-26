## Nest Sites

Nest locations for training the model consist of all Goshawk nest sites in NRM in Region 2 where `site_type=="Nest" & geo_type=="Point"` was last visited within the last 8 years and is recorded as "CURRENT".fWe also excluded all nests from the Nebraska National Forests and Grasslands because most of the nests there were in plantations and we figured that those data did not represent the types of nests on the San Juan National Forest.

## Covariates

- [LandFire Existing Vegetation (EVT_240)](https://landfire.gov/vegetation/evt) (30m)- Used a grouped "EVT_GP_N" variable that groups all non-tree levels into "Non-tree" and all tree levels into a handfull of common tree groups important to AMGO.
- [LandFire Succession Class (SClass)](https://www.landfire.gov/vegetation/sclass) (30m) - Not grouped in any way.
- [LandFire Existing Vegetation Height (EVH)](https://landfire.gov/vegetation/evh) (30m) - Heights are converted from Categorical to numerical by extracting meter values.
- Elevation, Slope, Aspect, Terrain Roughness Index, and Terrain Position Index (57m reprojected to 30m) all derived from Elevation.
- [NLCD Canopy Cover](https://www.mrlc.gov/data/nlcd-2021-usfs-tree-canopy-cover-conus) (30m)
- NLCD Canopy Cover Focal Area with a window of 11 30m pixels. That equals just under 30 acres in size, or the area of a typical American Goshawk nest stand.

## Model Specification

The model specification is defined in R using both [tidymodels](https://tidymodels.org/) and [tidysdm](https://evolecolgroup.github.io/tidysdm/) packages. The model is an ensamble of machine learning models. All numeric variables are normalized and all categorical data are converted to dummy variables with the [recipes](https://recipes.tidymodels.org/) package. Data are split into 5 spatial blocks using the `spatial_block_cv` function from the [spatialsample](https://spatialsample.tidymodels.org/reference/spatial_block_cv.html) package.

Four machine learning algorithms are used: general linear model, random forest, gradient boosted tree, and maxent. Some model versions exclude general linear models.

The models are cross validated and tuned with 25 resamples. And an ensemble of models was selected using the "boyce count" metric, which picks the best model from each algorithm. We may work with the [stacks]() package to blend results in a less random way in the future.

## Potential Data sources

- [Individual tree species parameters produced by the forest service](https://www.fs.usda.gov/foresthealth/applied-sciences/mapping-reporting/indiv-tree-parameter-maps.shtml)
- [USFS Tree Canopy Cover Dataset](https://data.fs.usda.gov/geodata/rastergateway/treecanopycover/)
