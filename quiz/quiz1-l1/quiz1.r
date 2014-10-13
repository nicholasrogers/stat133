# Please load in the dataset included in the quiz1-l1 directory. It will be
# required to perform the following tasks. The dataset includes data for houses
# in the city of Alameda.



# calculate the mean and median price of houses in Alameda. Store these as the
# variables <mean.price> and <med.price> respectively.

# mean.price <- your code here
# med.price <- your code here



# For each house in the dataset, calculate the absolute difference between its
# price and the mean price of houses in Alameda. Store this as the variable
# <price.diffs>. Note that this should be a numeric vector with length equal to
# the number of observations in the dataset

# price.diffs <- your code here


# The variable br indicates the number of bedrooms in each house. Please create
# the following two subsets and store them with the indicated names:
# 1) houses with more than 3 rooms <rooms.greater>
# 2) houses with up to and including 3 rooms <rooms.less>

# rooms.greater <- your code here
# rooms.less <- your code here




# For each of your subsets, create a vector giving the price of each house. Name
# these variables <rooms.greater.price> and <rooms.less.price>.

# rooms.greater.price <- your code here
# rooms.less.price <- your code here




# Please implement the function sqftByPrice. Your function should take the
# following arguments:
#
# <price.cutoff>: a numeric constant indicating the price cutoff used for houses
# <prices>: a numeric vector of housing prices
# <lsqft>: a numeric vector giving lsqft for each of the observations in
#   <prices> (i.e. this vector will need to be the same length as prices)
#
# Your function should return the average of <lsqft> for all observations with <price>
# stricly greater than <price.cutoff>.
sqftByPrice <- function(price.cutoff, prices, lsqft) {

    # your code here

}


# Please create a plot of house price (y-axis) against bsqft (x-axis). Your plot
# should include the following features:
# 1) a title "Housing price vs Building sqft"
# 2) a red line with intercept=169500 and slope=275
# 3) plotting character set to 20

