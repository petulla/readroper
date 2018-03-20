# generate fixed-width dataset
# in single and multi-card

if (require(gdata)) {

  val1 <- rpois(1000,0.8)
  val1 <- Filter(function(d) { d < 4 }, val1)

  val2 <- rpois(1000,6)
  val2 <- Filter(function(d) { d < 13 }, val2)
  val2 <- sprintf("%02d", val2)

  if (length(val2) > length(val1)) {
    val2 <- head(val2, length(val1) - length(val2))
  }

  if (length(val2) < length(val1)) {
    val1 = val1[c(1:length(val2))]
  }

  n <- length(val1)
  val3 <- rep(1, n)

  # codebook for single card
  df <- data.frame(val1,val2,val3)
  testSingleCard <- df
  # codebook for multiple-card
  df2 <- data.frame(val2,val1,val3)

  write.fwf(colnames=FALSE, x=df,file='testSingleCard.txt', sep="")

  df[] <- lapply(df, as.character)
  df2[] <- lapply(df2, as.character)

  df3 <- do.call(interleave, lapply(list(df, df2), setNames, paste0("V", 1:ncol(df))))
  testMultiCard <- do.call(paste0, df3)

  write(testMultiCard,file="testMultiCard.txt")
}
