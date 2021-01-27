#' Plot of "Orange" tree data.
#'
#' Given orange tree data "Orange", the function creates a scatter plot of data with line of best fit for each tree (age on x-axis, circumference on y-axis).
#' Packages used: roxygen2, devtools, ggplot2, dplyr
#'
#' @return Scatter Plot of Tree data with line of best fit.
#' @examples
#' plotOrange(Orange)
#' @export
plotOrange <- function(orangeDS){
  # plot tree circumference
  scatter <<- ggplot(orangeDS, aes(x=age, y=circumference, color=Tree)) + geom_point() + geom_smooth(method="lm", size = 0.5) #line of best fit
  return(scatter)
}


#' Correlations in Orange tree data
#'
#' Given orange tree data "Orange", the function returns the correlation between a Tree's age and circumference for each tree in the dataset.
#' Packages used: roxygen2, devtools, ggplot2, dplyr
#'
#' @return Correlation calculation for each tree.
#' @examples
#' corOrange(Orange)
#' @export
corOrange <- function(orangeDS){
  corList <<- list()
  treeNum <- unique(orangeDS[,1])
  for(tree in treeNum){
    treeData <- filter(orangeDS, Tree == tree) # filter is a dplyr function.
    print(paste0("Correlation for tree ", tree, ":"))
    corTable <- cor(treeData[,2:3])
    print(corTable[2,1])
    corList[tree] <<- corTable[2,1]
  }
  return(invisible(corList))
}
