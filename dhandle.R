library(dplyr)
library(purrr)

dmerge <- function(waves, w_names, var_names) {
  #' Merges data across waves into single data.frame with standard variable 
  #' names. Returns a data.frame of #observations rows and |waves|*|var_names| 
  #' columns.
  #' @param waves A list of data.frames each with wave-specific data.
  #' @param w_names A vector of character[] of desired labels for the waves.
  #' @param var_names A vector of character[] of desired labels for the variables.
  for (i in 1:length(as.list(waves))) {
    eval(parse(text=paste(w_names[i],'<-',waves[i], sep='')))  # split waves into diff vars
  }
  # Map* the true variable names to the wave-specific variable names (*in a table):
  waves_n_vars <- as.data.frame(bind_rows(as.data.frame(map(waves,colnames))), row.names=var_names)
  colnames(waves_n_vars) <- w_names

  # Copy data into a single merger frame:
  result_list <- list()

  for (i in 1:ncol(waves_n_vars)) {
    wave_i <- colnames(waves_n_vars)[i]
    for (j in 1:nrow(waves_n_vars)) {
      var_j <- waves_n_vars[j,i]
      eval(parse(text=paste('result_list$', wave_i, '_', var_names[j], '<-', wave_i, '$', var_j, sep='')))
    }
  }

  return(as.data.frame(result_list))
}

dapply <- function(operation, x, to_cols=NULL, result_col_name) {
  #' Applies an operation along columns to_cols, for each observation in x.
  #' @param operation A vector of character[] indicating the desired operation; e.g.: [c("any", "eq", 1)].
  #' @param x The data.frame to operate on.
  #' @param to_cols A vector of the names of the columns to operate on.
  #' @param result_col_name The name of the column to store the result.
  if (tolower(operation[1]) == 'any') {
    if (tolower(operation[2]) == 'eq') {
      # Construct a string like 'wave3==1|wave4==1|wave5==1':
      conditional_str = ''  # e.g.: wave3==1|wave4==1|wave5==1
      i=1
      while (i <= length(to_cols)) {
        conditional_str = paste(conditional_str,'x$',to_cols[i],'==',as.integer(operation[3]), sep='')
        if (i != length(to_cols)) {
          conditional_str = paste(conditional_str,'|', sep='')
        }
        i = i+1
      }
      # https://stackoverflow.com/a/8419993:
      eval(parse(text=paste(substitute(x),'<<-mutate(x,',result_col_name,'=case_when(',conditional_str,'~1,TRUE~0))', sep='')))
    }
  } else print('Didn\'t understand requested operation!')
}