# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Helper funcitons for transforming treatment data for swimplot #
# Katrina Hueniken, first added to swimplot October 2024        # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# getIntersection() finds all occurrences of overlapping treatments.
# note that treatment must be present in dataset 1.

getIntersection <- function(dt1, dt2=NULL, id, Tx, start, end, retain_vars=NULL){
  if (is.null(start) | is.null(end) | is.null(Tx) | is.null(id)) stop("ID, Start/end times, and treatment variable must be specified")
  if (!all(c(id,Tx,start,end) %in% names(dt1))) stop("Specified variable is not found in dataset 1")
  if (!is.null(dt2) & !all(c(id,start,end) %in% names(dt2))) stop("Specified variable is not found in dataset 2")
  if (!all(retain_vars %in% c(names(dt1), names(dt2)))) stop("Retained variables not found in either dataset.")
  
  # sort by id, start date, end date:
  dt1 <- dt1 |> dplyr::arrange(!!dplyr::sym(id), !!dplyr::sym(start), !!dplyr::sym(end))
  dt1 <- dt1[,names(dt1) %in% c(id, Tx, start, end, retain_vars)]
  
  # if a second dataset is not passed in, use dt1 twice in the merge step.
  if (is.null(dt2)) {
    same_dataset <- TRUE
    dt1[,"Index"] <- 1:nrow(dt1)
    dt2 <- dt1
  } else {
    same_dataset <- FALSE
    dt2 <- dt2[,names(dt2) %in% c(id, Tx, start, end, retain_vars)]
  }
  
  startxy <- paste0(start, c("_1","_2"))
  endxy <- paste0(end, c("_1","_2"))
  Txxy <- paste0(Tx, c("_1","_2"))
  
  # Merge dt1 and dt2 and find all instances of overlap
  tmp <- dt1 |> 
    dplyr::inner_join(dt2, by=id, relationship="many-to-many", suffix=c("_1","_2")) |> 
    dplyr::mutate(overlap_start = pmax(!!dplyr::sym(startxy[1]), !!dplyr::sym(startxy[2])),
                  overlap_end = pmin(!!dplyr::sym(endxy[1]), !!dplyr::sym(endxy[2]))) |>
    dplyr::filter(overlap_start < overlap_end)
  
  # if we merged dt1 with itself, need to get rid of instances where a single
  # treatment is overlapping with itself:
  if (same_dataset){
    tmp <- tmp |> dplyr::filter(Index_1 != Index_2) |> dplyr::select(-c(Index_1, Index_2))
  }
  
  # fix names (we'll need to keep the names consistent for the merge to work
  # properly with two unique datasets)
  names(tmp)[names(tmp) == "overlap_start"] <- start
  names(tmp)[names(tmp) == "overlap_end"] <- end
  
  # if we merged dt1 with itself, remove duplicates (e.g. treatment AB and BA)
  if (same_dataset){
    tx_check <- apply(tmp[,Txxy], 1, function(x) paste(x[order(x)], collapse=" & "))
    tmp <- tmp[!duplicated(cbind(tmp[,c(id, start, end)],tx_check)),]
  }
  
  # check if more than two treatments overlap:
  check_morethantwo <- tmp[,c(id, start, end)] |> 
    dplyr::mutate(row_no = dplyr::row_number()) |> 
    dplyr::inner_join(tmp[,c(id, start, end)] |> dplyr::mutate(row_no = dplyr::row_number()), 
                      by=id, relationship = "many-to-many", suffix=c("_1","_2")) |> 
    dplyr::mutate(overlap_start = pmax(!!dplyr::sym(startxy[1]), !!dplyr::sym(startxy[2])),
                  overlap_end = pmin(!!dplyr::sym(endxy[1]), !!dplyr::sym(endxy[2]))) |>
    dplyr::filter(row_no_1 != row_no_2 & overlap_start < overlap_end)
  
  if (nrow(check_morethantwo) > 0) stop(paste0(
    "Dataset has three or more treatments overlapping in time. 
    More than two overlapping treatments is not currently supported by swimplot. IDs: ", 
    paste(collapse=", ", unique(check_morethantwo[,id]))))
  
  # retain variables for stratification. 
  if (!is.null(retain_vars)){
    for (i in 1:length(retain_vars)){
      
      
      if (!paste0(retain_vars[i],"_2") %in% names(tmp)) next
      if (!identical(tmp[,paste0(retain_vars[i],"_1")], tmp[,paste0(retain_vars[i],"_2")])) {
        stop("Error in merging stratification variables in getIntersection(). Check that all observations from a given patient belong to the same stratum.")
      }
      
      which_rename_tmp <- which(names(tmp) == paste0(retain_vars[i],"_1"))
      if (length(which_rename_tmp) > 0) names(tmp)[which_rename_tmp] <- retain_vars[i]
    }
  }
  
  return(tmp[,names(tmp) %in% c(id, start, end, Tx, Txxy, retain_vars)]) 
}

# invertedIntervals() takes the original treatment data and the output from
# getIntersection, and finds the complement of the set of overlapping treatment
# times for each patient. 

invertedIntervals <- function(dt, intersection, id, start, end){
  # sort by id, start date, end date:
  intersection <- intersection |> dplyr::arrange(
    !!dplyr::sym(id), !!dplyr::sym(start), !!dplyr::sym(end))
  
  if (class(dt[,end]) != class(dt[,start])) stop("Start and end dates/times must be the same object class.")
  boundaries <- c(min(c(dt[,start], dt[,end]), na.rm=T)-1, max(c(dt[,start], dt[,end]))+1)
  
  if (nrow(intersection) > 0){
  # get first inverted interval:
  tmp_first <- intersection[,c(id, start)] |> dplyr::filter(!duplicated(!!dplyr::sym(id))) 
  names(tmp_first)[names(tmp_first) == start] <- end
  tmp_first[,start] <- -Inf
  
  # get all inverted intervals after the first one:
  if (class(intersection[,start]) == "Date" | class(intersection[,end]) == "Date"){
    stop("Swimplot helper functions do not currently support start and end times formatted as dates. Please input start and end times formatted as double or integer.")
  }
  
  first_id <- intersection[1,id]
  tmp_subsequent <- intersection[,c(id, start, end)] |> 
    tidyr::pivot_longer(2:3, values_to="date") |> 
    dplyr::mutate(date_lead = ifelse(
      dplyr::lead(!!dplyr::sym(id), default=first_id) == !!dplyr::sym(id), 
      dplyr::lead(date, default=Inf), Inf)) |> 
    dplyr::filter(name == end) |> 
    dplyr::select(!name)
  
  names(tmp_subsequent)[names(tmp_subsequent) == "date"] <- start
  names(tmp_subsequent)[names(tmp_subsequent) == "date_lead"] <- end
  } else {
    tmp_first <- NULL
    tmp_subsequent <- NULL
  }
  
  # get inverted intervals for patients with no overlap (the whole set space):
  if (sum(!dt[,id] %in% intersection[,id]) > 0){
    tmp_no_overlap <- data.frame(unique(dt[!dt[,id] %in% intersection[,id],id]), -Inf, Inf)
    names(tmp_no_overlap) <- c(id, start, end)
  } else tmp_no_overlap <- NULL
  
  tmp <- data.frame(rbind(tmp_subsequent, tmp_first, tmp_no_overlap))
  tmp <- tmp[order(tmp[,id], tmp[,start]),]
  
  return(tmp)
}

# transform_for_swimplot() uses both of the above functions to create a disjoint
# set of treatment intervals (overlapping or non-overlapping) for each patient.
# 
# Right now this only works for maximum two overlapping treatments. However,
# this could potentially be extended to an arbitrary number of overlapping
# treatments by finding overlap between two treatments at a time, and looping
# through all possible treatments.

transform_for_swimplot <- function (df, id, Tx, start, end, retain_vars=NULL){
  # Find overlapping intervals for each patient:
  intersect_dat <- getIntersection(dt1=df, id=id, Tx=Tx, start=start, end=end, retain_vars=retain_vars)
  
  # Getting inverted intervals (complement of the set of overlapping intervals):
  inverted_dat <- invertedIntervals(intersection=intersect_dat, dt=df, id=id, start=start, end=end)
  
  # recycling getIntersection function above, but this time passing in both the
  # original data and the inverted intervals:
  non_intersect_dat <- getIntersection(
    dt1=df, dt2=inverted_dat, id=id, Tx=Tx, start=start, end=end, retain_vars=retain_vars)
  names(non_intersect_dat)[names(non_intersect_dat) == Tx] <- paste0(Tx, "_1")
  
  # Putting intersection and non-intersection together:
  dplyr::bind_rows(intersect_dat, non_intersect_dat) |> 
    dplyr::arrange(!!dplyr::sym(id), !!dplyr::sym(start), !!dplyr::sym(end))
}
