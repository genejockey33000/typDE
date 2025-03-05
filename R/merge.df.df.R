#' Merge two data frames containing some common samples in rows
#'
#' @param df1 Data frame 1
#' @param df2 Data frame 2
#' @param key name of column shared between df1 and df2 that has unique values and can be used to synchronize position and existence of rows in output.
#'
#' @return
#' @export
#'
#' @examples
#' d1 <- data.frame(sample = paste0("PG", 1:6),
#' geno = sample(c("wt", "het", "ko"), 6, replace = TRUE),
#' batch = c(1,1,2,2,3,3))
#' d2 <- cbind.data.frame(d1[c(1,3,6,4,2),],
#' patho = sample(c("high", "med", "low"), 5, replace = TRUE))
#' d1.2 <- merge.df.df(df1 = d1, df2 = d2, key = "sample")
#'
merge.df.df <- function(df1, df2, key) {
     temp <- sync.df.df(df1 = df1, df2 = df2, key = key)
     d1 <- temp[["df1"]]
     d2 <- temp[["df2"]]

     d1.cols <- colnames(d1)[!colnames(d1) == key]
     d2.cols <- colnames(d2)[!colnames(d2) == key]
     if (sum(d2.cols %in% d1.cols) > 0) {
          cat("There are column names in df2 that are also in df1 (not including the key column).\n
        I'm assuming those are identical duplicated measures.\n
        The duplicated measures in df2 will be removed prior to combining\n")
          d2.dup.columns <- d2.cols[d2.cols %in% d1.cols]
          cat("duplicated columns in df2 are: \n")
          cat(d2.dup.columns)
          d2 <- d2[,!colnames(d2) %in% d2.dup.columns]
     } else {cat("All all non-key columns are unique. Dataframes will be merged as is.\n")}

     if(all(d1[,key] == d2[,key])) {
          output <- cbind.data.frame(d1, d2[,-1])
     } else {stop("Failure during merge. all(df1$key == df2$key) returned FALSE.")}
}
