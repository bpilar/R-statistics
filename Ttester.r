confidence_lvl = 0.05

cat("Choose .csv file with data\n")
data <- read.csv(file.choose())

col_names <- colnames(data)
for ( col_id in c( 1 : length(col_names) ) ) {
  cat( format( as.character(col_id), width = nchar(col_names[col_id])+3, justify = "c" ) )
}
cat("\n")
cat( paste(col_names, collapse = "   ") )
cat("\nChoose base column: ")
base_col_id <- ifelse (interactive(), readline(), readLines(file("stdin"), 1) )
base_col_id <- as.integer(base_col_id)

cat("Choose columns for analysis: ")
res_col_ids <- ifelse (interactive(), readline(), readLines(file("stdin"), 1) )
res_col_ids <- unlist(strsplit(res_col_ids, ","))
res_col_ids <- as.integer(res_col_ids)


base_col_factor <- factor(data[[base_col_id]])
if(length(levels(base_col_factor)) != 2) {
  cat("improper base column\n")
  stop()
}
cat("factor levels:\n")
print(levels(base_col_factor))


splited_data <- split.data.frame(data, f = base_col_factor)
A_data <- splited_data[[1]]
B_data <- splited_data[[2]]

for (res_col_id in res_col_ids) {
  A_sample <- A_data[[res_col_id]]
  B_sample <- B_data[[res_col_id]]
  current_samples <- list(A_sample, B_sample)
                                                                                                                                               
  current_summary <- data.frame(
    group = c(A_data[[base_col_id]][1], B_data[[base_col_id]][1]),
    n = unlist(lapply(current_samples, length)),
    mean = unlist(lapply(current_samples, mean)),
    med = unlist(lapply(current_samples, median)),
    min = unlist(lapply(current_samples, min)),
    max = unlist(lapply(current_samples, max)),
    stdDev = unlist(lapply(current_samples, sd))
  )
  current_summary$esd <- unlist(lapply(current_samples, 
                                function(x) sqrt(length(x)*(mean(x^2)-mean(x)^2)/(length(x)-1))))
  cat("\n")
  print(current_summary)
  
  X_summ <- current_summary[1,]
  Y_summ <- current_summary[2,]
  if (X_summ$mean < Y_summ$mean) {
    X_summ <- current_summary[2,]
    Y_summ <- current_summary[1,]
  }
  
  t <- (X_summ$mean - Y_summ$mean) / sqrt(X_summ$esd/X_summ$n + Y_summ$esd/Y_summ$n)
  df <- X_summ$n + Y_summ$n - 2
  
  cat(paste("t = ", t, "\n"))
  cat(paste("df = ", df, "\n"))
  
  p_val <- min(pt(t, df), pt(t, df, lower.tail = FALSE))
  
  cat(paste("p-value = ", p_val, "\nresult: "))
  cat(ifelse(p_val<confidence_lvl, "h1\n", "h0\n"))
}


