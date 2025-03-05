#' Sync dataframe1 and dataframe2
#'
#' @description
#' This function simply takes in 2 separate data frames (with samples in rows and measures in columns), as input and synchronizes the rows such that the samples in both are synchronized. The function uses a key column (i.e. sampleID), that is common to both and denotes unique IDs for each row.
#'
#' @param df1 dataframe1
#' @param df2 dataframe2
#' @param key name of column shared between df1 and df2 that has unique values and can be used to synchronize position and existence of rows in output.
#'
#' @return A list object with synchronized data frames
#' @export
#'
#' @examples
#' d1 <- data.frame(sample = paste0("PG", 1:6),
#' geno = sample(c("wt", "het", "ko"), 6, replace = TRUE),
#' batch = c(1,1,2,2,3,3))
#' d2 <- cbind.data.frame(d1[c(1,3,6,4,2),],
#' patho = sample(c("high", "med", "low"), 5, replace = TRUE))
#' d1.2 <- sync.df.df(df1 = d1, df2 = d2, key = "sample")
sync.df.df <- function(df1, df2, key) {
     d1 <- df1[!is.na(df1[,key]),]
     d1[,key] <- as.character(d1[,key])
     d1 <- d1[!duplicated(d1[,key]),]
     d2 <- df2[!is.na(df2[,key]),]
     d2 <- d2[!duplicated(d2[,key]),]
     d2[,key] <- as.character(d2[,key])
     d1 <- d1[d1[,key] %in% d2[,key],]
     d2 <- d2[d2[,key] %in% d1[,key],]
     d1 <- cbind.data.frame(d1[,key], d1[,c(!colnames(d1) == key)])
     colnames(d1)[1] <- key
     d2 <- cbind.data.frame(d2[,key], d2[,c(!colnames(d2) == key)])
     colnames(d2)[1] <- key

     if (nrow(d1) == nrow(d2) & all(d1[,key] %in% d2[,key])) {
          d2 <- d2[match(d1[,key], d2[,key]),]
          row.names(d1) <- NULL
          row.names(d2) <- NULL
          return(list(df1 = d1, df2 = d2))
     } else {stop("Dataframes did not sync using 'sync.df.df'.")}
}
