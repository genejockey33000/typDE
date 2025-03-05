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
