Housing Price Prediction Data Story
================
Jason Wilcox
July 6, 2018

Introduction
------------

Housing prices are always changing and anyone who has bought or sold a home knows that the true value can differ greatly depending on a large number of factors. Knowing which traits and qualities can add or detract from a home's value could help in not only determining the true value, but also aid potential buyers and sellers. Analyzing housing data can grant exceptional insight to the best way a seller could increase the value of the house they have put up for sale. Increasing square footage or the size of the garage may be easy ways for a seller to raise the value before the house does sell. Potential buyers can better understand the actual value of a house before they decide to purchase one and know if they are getting a good deal. Predicting the sales price of different homes is possible with the data set sourced from Kaggle (<https://www.kaggle.com/c/house-prices-advanced-regression-techniques>).

Data
----

The dataset comprises of 2919 houses having 80 variables (81 initially). These variables are:

     [1] "Id"            "MSSubClass"    "MSZoning"      "LotFrontage"  
     [5] "LotArea"       "Street"        "Alley"         "LotShape"     
     [9] "LandContour"   "Utilities"     "LotConfig"     "LandSlope"    
    [13] "Neighborhood"  "Condition1"    "Condition2"    "BldgType"     
    [17] "HouseStyle"    "OverallQual"   "OverallCond"   "YearBuilt"    
    [21] "YearRemodAdd"  "RoofStyle"     "RoofMatl"      "Exterior1st"  
    [25] "Exterior2nd"   "MasVnrType"    "MasVnrArea"    "ExterQual"    
    [29] "ExterCond"     "Foundation"    "BsmtQual"      "BsmtCond"     
    [33] "BsmtExposure"  "BsmtFinType1"  "BsmtFinSF1"    "BsmtFinType2" 
    [37] "BsmtFinSF2"    "BsmtUnfSF"     "TotalBsmtSF"   "Heating"      
    [41] "HeatingQC"     "CentralAir"    "Electrical"    "X1stFlrSF"    
    [45] "X2ndFlrSF"     "LowQualFinSF"  "GrLivArea"     "BsmtFullBath" 
    [49] "BsmtHalfBath"  "FullBath"      "HalfBath"      "BedroomAbvGr" 
    [53] "KitchenAbvGr"  "KitchenQual"   "TotRmsAbvGrd"  "Functional"   
    [57] "Fireplaces"    "FireplaceQu"   "GarageType"    "GarageYrBlt"  
    [61] "GarageFinish"  "GarageCars"    "GarageArea"    "GarageQual"   
    [65] "GarageCond"    "PavedDrive"    "WoodDeckSF"    "OpenPorchSF"  
    [69] "EnclosedPorch" "X3SsnPorch"    "ScreenPorch"   "PoolArea"     
    [73] "PoolQC"        "Fence"         "MiscFeature"   "MiscVal"      
    [77] "MoSold"        "YrSold"        "SaleType"      "SaleCondition"
    [81] "SalePrice"    

Most are pretty obvious, but some are less clear. Condition 1 and 2 represent the houses proximity to various conditions such as railroads, parks, arterial street, etc. OverallQual is a rating given to the house based on material and finish. BsmtExposure refers to walkout or garden level walls. GrLivArea is the above ground living area in sqft.Further information about the details of each variable can be found on the Kaggle website (<https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data>). The data\_description.txt file gives a quick idea of what each variable represents and the possible levels if it is a factor variable.

Data Wrangling
--------------

Initially the raw test and train data were loaded as a CSV file and bound together to create a full dataset of 2919 observations. I then went systematically through the variables looking for missing values to replace and any other erroneous data points that needed to be addressed. Initially there were 15424 NA's among 37 variables (including the 1459 missing from SalePrice).

For many categories, it was simply easiest and best suited to just replace any NA's with the most common occuring value. This was done with MSZoning \[4\], Functional \[2\], Exterior1st and 2nd \[1, 1\], Electrical \[1\], KitchenQual \[1\], and SaleType \[1\]. This was done like this:

``` r
combined$MSZoning[is.na(combined$MSZoning)] <- names(sort(-table(combined$MSZoning)))[1] #replace the 4 missing values with most common factor levels
```

Also very common was simply replacing NA with "None" or some default. This was done in MiscFeature \[2814\], Alley \[2721\], GarageYrBlt \[159\] (Na were converted to match HouseYrBlt since these houses appeared to have no garage or a garage built when the house was built), GarageType \[157\], GarageCond \[159\], GarageFinish \[159\], GarageQual \[159\], BsmtExposure \[82\], BsmtCond \[82\], BsmtQual \[81\], and MasVnrArea \[23\]. BsmtFinType 1 and 2 \[79, 80\], BsmtUnfSF, TotalBsmtSF, BsmtFullBath, and BsmtHallfBath \[1, 1, 2, 2\] were all replaced with 0's.

``` r
combined$MiscFeature <- factor(ifelse(is.na(combined$MiscFeature), "None", paste(combined$MiscFeature)), levels = c(levels(combined$MiscFeature), "None"))
```

Some values had ordinality naturally or needed to be assigned ordinality. NA's were also present and needed to be converted to "None". PoolQC \[2909\] and FireplaceQual \[1420\] already had some ordinality but I wanted to convert them from their system of ratings using letters (Fa = Fair, Gd = Good, etc) to that of numbers so I could treat it as numeric. I assigned a value to each rating and then converted the variable from a factor to numeric.

``` r
print(fct_count(combined$PoolQC)) # show the number and levels appearing
combined$PoolQC <- factor(ifelse(is.na(combined$PoolQC), "None", paste(combined$PoolQC)), levels = c(levels(combined$PoolQC), "None"))
# Converts the level from NA to None and adds the level None

combined$PoolQC <- factor(combined$PoolQC, levels = c("None", "Po", "Fa", "TA", "Gd", "Ex")) #Reorder levels in ascending order

# Ordinal Values for use later with other variables as well
Quality <- c("None" = 0, "Po" = 1, "Fa" = 2, "TA" = 3, "Gd" = 4, "Ex" = 5)

combined$PoolQC <- (plyr::revalue(combined$PoolQC, Quality)) 
table(combined$PoolQC)

combined$PoolQC <- as.numeric(levels(combined$PoolQC))[combined$PoolQC]
```

Others showed ordinality but didn't use this same system. MasVnrType \[24\] is an example where I needed to check ordinality. It was clear that stone was better that brick face, which was still better than nothing. I then assigned these levels order and converted to integers. I did something similar with Street and Alley (they had no missing values), where I needed to assign ordinality due to paved roads being significantly better than gravel and dirt roads.

``` r
# View(combined[is.na(combined$MasVnrType), c("MasVnrType", "MasVnrArea", "YearBuilt", "ExterCond")])
# House 2611 missing MasVnrType. Impute with most common 
combined$MasVnrType[2611] <- names(sort(-table(combined$MasVnrType)))[2] #most common is none, so chose 2nd most common

# Determine Ordinality for the 23 houses without MasVnrType missing and fill in with None
combined$MasVnrType[is.na(combined$MasVnrType)] <- "None"
exterier <- combined[!is.na(combined$SalePrice),] %>% group_by(MasVnrType) %>% dplyr::summarise(median = median(SalePrice), counts = n()) %>%arrange(median)

print(exterier)

# Clearly ordinal, Stone > BrkFace = None
masonry <- c("None" = 0, "BrkCmn" = 0, "BrkFace" = 1, "Stone" = 2)
combined$MasVnrType<- factor(combined$MasVnrType, levels = c("None", "BrkCmn", "BrkFace", "Stone")) #Reorder levels
combined$MasVnrType <- as.integer(plyr::revalue(combined$MasVnrType, masonry))
table(combined$MasVnrType) #increases the value of everything by 1 why?
```

For LotFrontage \[486\], GarageCars and GarageArea \[1, 1\] I used Knn nearest neighbor with a k value of 5 to impute the missing values. This was done after grouping by neighborhood as homes in the same neighborhoods would have similar lot sizes and though it best to fill in the missing values based around that similarity.

``` r
impute <- kNN(combined, k = 5)

imputeNeighborhood <- impute %>% select(Neighborhood, LotFrontage) %>%
  dplyr::group_by(Neighborhood) %>% 
  dplyr::summarize(LotFrontage = mean(LotFrontage))

imputeNeighborhood

combined$LotFrontage <- impute$LotFrontage
```

Lastly miscellaneous odds and ends were tied up. For PoolArea \[3\] I graphed the data and extrapolated to fill in the three missing values. I dropped Utilities since nearly all the values were the same, it provided no useful information. There was a typo I noticed with GarageYrBlt where there was a value of 2207 which I changed back to 2007.

``` r
test <- combined %>% subset(PoolQC > 0)

ggplot(test, aes(x = OverallQual, y = PoolQC)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(0, 10), labels = scales::comma)

combined$PoolQC[2600] <- 2
combined$PoolQC[2421] <- 2
combined$PoolQC[2504] <- 3
```

Exploratory Data Analysis
=========================

SalePrice
---------

Having thousands of data points on a diverse group of houses, there are many interesting data points to discuss. There were some very obvious ways to initially look at the data but I also discovered more abstract ways to see it too. Some important findings to my data analysis included simple comparison of sale price with basic things such as house size, age, etc but also some interested things were seen when comparing houses by neighborhoods to see trends across the city.

Initially, the very first thing I wanted to see was sale price shown by count. This gives a quick idea of how the data is represented. As can be seen below, the sale price is roughly normal, skewed to the right slightly which makes sense, considering there isn't an upper limit on housing prices and expensive houses can be built anywhere. There appears to be a noticeable dip in value near where the mean and median area. One goal is to understand why this occurs.Also of note, there are 1459 NA's, which are the missing values to be predicted for SalePrice.

![](Data_Story_6.1_Report_files/figure-markdown_github/SalePrice%20Histogram-1.png)

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##   34900  129975  163000  180921  214000  755000    1459

House Size
----------

The first predictable analysis I did was sale price vs house size. I used the sum of the basement, 1st, and 2nd floor square footage to create a scatter plot of the various house sizes and what they sold for. As expected, there was a strong trend for the larger the house, the more it sold for. There appear to be a few exceptions (houses 1299 and 524) which appear to have sold for significantly less than expected for the size. I plan on looking into this more to determine the reasons that these houses could be valued so low, perhaps they are old, or the quality is very low. Those were my initial thoughts and a quick look at the data shows they were both recently built (2008 and 2007) and have been given an "OverallQual" value of 10, the highest (and also the most influential variable for SalePrice based on correlation). So it will be interesting to determine why those two houses are much lower in price.

![](Data_Story_6.1_Report_files/figure-markdown_github/unnamed-chunk-2-1.png)

![](Data_Story_6.1_Report_files/figure-markdown_github/Correlation-1.png)

Overall Quality
---------------

Using a correlation plot, I was able to determine that OverallQual had the largest effect on SalePrice, having a correlation value of 0.79. I wanted to better understand just how much impact OverallQual had so I decided to make a graph. I was surprised to actually see that OverallQual which "Rates the overall materiel and finish of the houses", which would also be subjective bias from whoever was giving the ratings, was so strongly related. As quality rating goes up, so does the value of the house (which makes sense). There also appears to be a large change in rate between 6 and 7, suggesting that materials with quality of 7 or higher have greater impact on increasing a home's value per unit.

![](Data_Story_6.1_Report_files/figure-markdown_github/unnamed-chunk-3-1.png)

Neighborhood
------------

Lastly, to look at things a little differently I decided to group everything by neighborhood to see what trends there were by different areas of the city, and wasn't surprised when I noticed some significant differences. Most people know that cities have both good and bad neighborhoods which can be determined by a number of things, including the type and quality of houses, distance to and quality of schools, hospitals, churches, police, and crime rates. First, I ranked the neighborhoods in ascending order by average SalePrice for the homes for each and marked the mean with a dashed line. I then used the same ascending order to show each neighborhood's OverallQual, SquareFootage, LotArea, and average age of house. Stitching the plots together then allows for simple and easy comparison. Looking at OverallQual, there are some neighborhoods that appear to have houses which have higher quality relative to their SalePrice, such as in Blueste. This means that some homes are selling for less than their true value and would be a great purchase. With the house size graph, some neighborhoods have houses larger than those of similar value, such as in SWISU neighborhood and could provide a large family a lower priced home than potentially elsewhere in the city. Lot Area and house age provided some insight to the other graphs by having less apparent trends due to more outliers but still providing useful information. Neighborhoods that had more expensive homes tend to also have larger plots of land and be newer, but not always.

![](Data_Story_6.1_Report_files/figure-markdown_github/unnamed-chunk-4-1.png)![](Data_Story_6.1_Report_files/figure-markdown_github/unnamed-chunk-4-2.png)![](Data_Story_6.1_Report_files/figure-markdown_github/unnamed-chunk-4-3.png)

Conclusion
==========

There is quite a lot of data still to be looked at and understand what is going on. I have some questions already to look deeper for the answers and know many more will show up as I dig deeper. There will be more need to analyze the factor variables as well for the influences they may have on the value of homes, which can't be as easily quantified as the initial data was since it was numeric.
