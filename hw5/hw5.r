# Implement the function "listLengths". Your function should take the
# follwoing arguments:
#
# <data.list>: a list whose elements are vectors of varying length
#
# Your function should return the following:
#
# <element.lengths>: a numeric vector whose entries are the lengths of each
#   element of <data.list>

listLengths <- function(data.list) {

    apply(data.list, 2, length)
}

#tryCatch(checkEquals(list.lengths.t, listLengths(ex3.test2)),
         #error=function(err) errMsg(err))


# Implement the function "standMatrixVariables". Your function should take
# the folowing arguments:
#
# <data.matrix>: a numeric matrix whose columns correspond to variables
#
# Your function should return the following:
#
# <standardized.matrix>: an nxn matrix (where n is the number of variables
#   i.e. columns of <data.matrix). Entry (i,j) of this matrix should contain
#   the following value:
#
#      (mean(col.i) - mean(col.j)) / sd(col.i, col.j)
#
# where sd(col.i, col.j) is the standard deviation of all values from both
# column i and j.

standMatrixVariables <- function(data.matrix) {
    mean.col <- apply(data.matrix, 2, mean)
    mean.differnece.matrix <- sapply(mean.col, function(x) sapply(mean.col, function(y) x-y))
    sd.both.columns <- NULL
    for(i in 1:ncol(ex3.test4)){
      for(j in 1:ncol(ex3.test4)){
        print(sd(c(data.matrix[, i], data.matrix[, j])) 
      }
    } #I don't know how to use the output of the for loop and put it into a matrix. Fun. Times. 
}

#tryCatch(checkEquals(stand.matrix.variables.t,
                     #standMatrixVariables(ex3.test4)),
         #error=function(err) errMsg(err))


# Load in the "babies.csv" dataset for this problem. Implement the function
# "testGroupsGestation" that takes the following arguments:
#
# <data>: any subset of the babies.csv dataset
# <group1.idcs>: a numeric vector giving the indices of some subset of <data>
# <group2.idcs>: a numeric vector giving the indices of some other subset of <data>. 
#   NOTE: the two idcs vectors should contain no overlapping elements.
# <test.alternative>: a character string equal to one of the following
#   c("two.sided, "less", "greater") specifying the directionalty of the t.test
#
# Your function should return the following output:
#
# <t.test.output> the entire output of the t.test comparing gestation
#   period for the two given groups. Use the group1 subset the first
#   argument, group 2 as the second argument and the alternative direction
#   specified by <test.alternative>


testGroupsGestation <- function(data, group1.idcs, group2.idcs,
                                test.alternative='two.sided') {

    stopifnot(!any(group1.idcs %in% group2.idcs))

    print(t.test(data[group1.idcs, "gestation"], data[group2.idcs, "gestation"], test.alternative))
}

#tryCatch(checkEquals(test.groups.gestation.t$p.value,
                   #  testGroupsGestation(test.data, g1, g2,
                                       #  test.alternative='greater')$p.value),
        # error=function(err) errMsg(err))

# Use your function to perform a one-sided t-test comparing the gestation
# period for babies of smoking mothers and non-smoking mothers. Store this
# variable as <smoking.test>

# your code here
smoke.idcs <- row.names(subset(babies, babies$smoke == 1))
non.smoke.idcs <- row.names(subset(babies, babies$smoke == 0))
smoking.test <- testGroupsGestation(babies, smoke.idcs, non.smoke.idcs, test.alternative = "less")

