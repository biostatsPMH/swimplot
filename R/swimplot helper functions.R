# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Helper funcitons for transforming treatment data for swimplot #
# Katrina Hueniken, first added to swimplot October 2024        # 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# getIntersection() finds all occurrences of overlapping treatments. 

getIntersection <- function(dt1, dt2=NULL, id, Tx, start, end){
  # sort by id, start date, end date:
  dt1 <- dt1 |> dplyr::arrange(!!dplyr::sym(id), !!dplyr::sym(start), !!dplyr::sym(end))
  
  # if a second dataset is not passed in, use dt1 twice in the merge step.
  if (is.null(dt2)) {
    same_dataset <- TRUE
    dt2 <- dt1
  } else same_dataset <- FALSE
  
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
    tmp <- tmp |> dplyr::filter(!!dplyr::sym(Txxy[1]) != !!dplyr::sym(Txxy[2]))
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
  
  if (nrow(check_morethantwo) > 0) stop(
    "Dataset has three or more treatments overlapping in time. 
    More than two overlapping treatments is not currently supported by swimplot.")
  
  return(tmp[,names(tmp) %in% c(id, start, end, Tx, Txxy)])
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
  tmp_no_overlap <- data.frame(unique(dt[!dt[,id] %in% intersection[,id],id]), -Inf, Inf)
  names(tmp_no_overlap) <- c(id, start, end)
  
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

transform_for_swimplot <- function (df, id, Tx, start, end){
  # Find overlapping intervals for each patient:
  intersect_dat <- getIntersection(dt1=df, id=id, Tx=Tx, start=start, end=end)
  
  # Getting inverted intervals (complement of the set of overlapping intervals):
  inverted_dat <- invertedIntervals(intersection=intersect_dat, dt=df, id=id, start=start, end=end)
  
  # recycling getIntersection function above, but this time passing in both the
  # original data and the inverted intervals:
  non_intersect_dat <- getIntersection(
    dt1=df, dt2=inverted_dat, id=id, Tx=Tx, start=start, end=end)
  names(non_intersect_dat)[names(non_intersect_dat) == Tx] <- paste0(Tx, "_1")
  
  # Putting intersection and non-intersection together:
  dplyr::bind_rows(intersect_dat, non_intersect_dat) |> 
    dplyr::arrange(!!dplyr::sym(id), !!dplyr::sym(start), !!dplyr::sym(end))
}
