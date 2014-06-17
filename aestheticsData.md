Data for Analsysis of the Subjective Assessments for the 2007 NLA
========================================================

Build data.frame "Lakes" for analysis by merging data from the following sources

1.  NLCD = NLCDdata for multiple buffers (300m, 1500m, 3000m and Buffer=max linear distance within lake)
2.  NLA = NLA 2007 Data.  Note the Visual assessment categories ("SWIMMABILITY", "PRISTINE", "APPEALING", "BIOTIC_INTEGRITY","RECREATIONAL_VALUE") were converted to ordered factors.  Any categories with less than 100 observations (< 10% of sites) were added to the next highest category.
3.  SHP = lake centroids, area, perimeter, and shoreline development index from the NLA Lake Poly shapefile
4.  Morpho = Jeff's estimates of lake morphology.













