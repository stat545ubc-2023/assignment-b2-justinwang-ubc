#' Group the data by variable and summarize
#'
#' This function can group the data by a variable and calculate the max, min, mean, and the number of a numeric value in each group.
#'
#' @param data A data frame contain the data
#' @param group_var A variable to be group by
#' @param na.rm A Boolean value for NA to be removed, which applied to function min,max, etc. Default value is true.
#' @param ... Other parameter pass to the min(),max(),max()
#'
#' @return A Table contain the result of min, max, mean, and the the number of numeric value in each group.
#' @export
#'
#' @examples
#' class_545_running_score <- tibble(group = c(1,2,3,1,2,3,1),result = c(13.5,14,11.5,13.6,12.6,16,15.5),gender= c("male","female","male","male","female","female","male"),name = c("justin","lily","james","henry","helen","lucy","kimberly"))
#' result <- groupSummarize(class_545_running_score, group, result, na.rm = TRUE) result

groupSummarize <- function(data, group_var,var, na.rm = true, ...){
  data %>%
    group_by({{ group_var }}) %>%
    summarize(
      min = min({{ var }}, na.rm = na.rm, ...),
      mean = mean({{ var }}, na.rm = na.rm, ...),
      max = max({{ var }},na.rm = na.rm, ...),
      n = n()
    )
}
