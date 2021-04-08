# swim_plot_pattern -------------------------------------------------------------

#' Creating the base of a swimmers plot
#'
#' This function allows you to create swimmers plots with bars, includes options to
#' have the bars change colours and create stratified plots
#' @param df a data frame
#' @param id  column name for id, default is 'id'
#' @param end column name with the bar lengths (or bar end positions if bars change colour), default is 'end'
#' @param start column name with the bar start positions (only required when there are gaps between sections of bars, or bars which do not start at zero), default is 'start'
#' @param name_fill a column name to map the bar fill
#' @param name_col a column name to map the bar colour
#' @param name_pattern a column name to map the pattern
#' @param name_pattern_fill a column name to map the pattern fill
#' @param name_alpha a column name to map the bar transparency
#' @param id_order order of the bars by id, can input a column name to sort by, or the ids in order.
#' @param increasing Binary to specify bars in increasing order (Default is TRUE)
#' @param stratify a list of column names to stratify by
#' @param base_size the base size for the plot, default is 11
#' @param identifiers Binary to specify patient identifiers are included in the y axis (default is TRUE)
#' @param ... additional geom_col_pattern arguments
#'  @details geom_col_pattern arguments include the arguments from geom_col (eg. col, alpha,..) in addition to
#' \itemize{
#'  \item{pattern: }{Pattern name string e.g. 'stripe' (default), 'crosshatch', 'point', 'circle', 'none'}
#'  \item{pattern_colour: }{Colour used for strokes and points. default: 'black'}
#'  \item{pattern_fill: }{Fill colour. default: 'black'}
#'  \item{pattern_angle: }{Orientation of the pattern in degrees. default: 45}
#'  \item{pattern_density: }{Approximate fill fraction of the pattern. Usually in range [0, 1], but can be higher. default: 0.2}
#'  \item{pattern_spacing: }{Spacing of the pattern as a fraction of the plot size. default: 0.05}
#'  \item{pattern_xoffset,pattern_yoffset: }{Offset the origin of the pattern. Range [0, 1]. default: 0. Use this to slightly shift the origin of the pattern. For most patterns, the user should limit the offset value to be less than the pattern spacing.}
#'  \item{pattern_alpha: }{Alpha transparency for pattern. default: 1}
#'  \item{pattern_linetype: }{Stroke linetype. default: 1}
#'  \item{pattern_size: }{Stroke line width. default: 1}
#' }
#' @return a swimmer plot with bars
#' @seealso \code{\link{swimmer_points}} \code{\link{swimmer_lines}}  \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}} \code{\link{swimmer_arrows}} \code{\link{swimmer_text}}
#' @examples
#'
#'
#'
#'
#'# install.packages("remotes")
#'# remotes::install_github("coolbutuseless/ggpattern")
#'
#'library(ggpattern)
#'library(ggplot2)
#'
#'df <- data.frame(id=c(1,2,3,4,5,6,6,6),
#'                 Treatment = c('a','b','a&b','a','a&b','b','a&b','a'),
#'                 end=c(1,2,3,4,5,2,4,6))
#'
#'
#'swim_plot_pattern(df = df,name_pattern = 'Treatment',name_pattern_fill = 'Treatment',
#'                  name_fill = 'Treatment',col='black',pattern_density=0.5)+
#'  scale_pattern_manual(values = c('a'='none','b'='none','a&b'='stripe'))+
#'  scale_pattern_fill_manual(values = c('a'='lightblue','b'='pink','a&b'='pink'),breaks = c('a','b'))+
#' scale_fill_manual(values = c('a'='lightblue','b'='pink','a&b'='lightblue'),breaks = c('a','b'))+
#'  guides(fill = guide_legend(override.aes = list(pattern = 'none' )),pattern=FALSE)
#'

#' @export
swim_plot_pattern <- function(df,id='id',end='end',start='start',name_fill=NULL,name_col=NULL,name_pattern=NULL,name_pattern_fill=NULL
                              ,name_alpha=NULL,increasing=TRUE,id_order = NULL,
                              stratify=FALSE,base_size=11,identifiers=TRUE,...)
{

  #Check deprecated id_order = increasing or decreasing
  if(!is.null(id_order)) {
    if(id_order[1] %in% c("increasing",'decreasing')){
      warning("Increasing/decreasing have been deprecated as options for id_order use increasing=TRUE or increasing=FALSE instead",
              call. = FALSE)

      if(id_order[1]=="increasing") increasing = TRUE
      if(id_order[1]=="decreasing") increasing = FALSE

      id_order = NULL
    }
  }


  df[,id] <- as.character(df[,id])


  if(is.null(id_order)){

    max_df <- stats::aggregate(df[,end]~df[,id],FUN=max,na.rm=T)
    names(max_df) <- c(id,'MAX_TIME_FOR_EACH_ID')
    if(increasing) {id_order <-  max_df[order(max_df$MAX_TIME_FOR_EACH_ID),id]

    }else id_order <-  max_df[order(max_df$MAX_TIME_FOR_EACH_ID,decreasing = T),id]

  }



  if (id_order[1] %in% names(df)) {
    max_df <- stats::aggregate(df[,end]~df[,id],FUN=max,na.rm=T)
    names(max_df) <- c(id,'MAX_TIME_FOR_EACH_ID')
    merged_df_with_max <- merge(max_df,df,all=F)
    starting_df <-  stats::aggregate(df[,end]~df[,id],FUN=min,na.rm=T)
    names(starting_df) <- c(id,end)
    starting_information <- merge(starting_df,merged_df_with_max,all=F)
    if(increasing) {id_order <- starting_information[order(starting_information[,id_order[1]], -rank(starting_information$MAX_TIME_FOR_EACH_ID), decreasing = TRUE),id]
    }else id_order <- starting_information[order(starting_information[,id_order[1]], rank(starting_information$MAX_TIME_FOR_EACH_ID), decreasing = TRUE),id]

  }

  df <- df[order(df[,id],df[,end]),]
  ##Filling in any gaps (Adding empty bars at 0 and between sections)
  if(start %in% names(df)){


    negative_start <- df[,id][df[,start]<0]
    if(length(negative_start)>0) {stop(paste0(    paste0("There is(are) ", length(negative_start)," id(s) that have negative start times ",id ,"=(",paste (negative_start,sep="", collapse=","),")")))}


    ##Checking there are not overlapping sections
    check_for_overlap <- function(data,id,start,end,x){
      single <- data[data[,id]==x,]
      if(dim(single)[1]>1){
        single <- single[order(single[,start]),]
        check_val <- min(single[,start]-dplyr::lag(single[,end]),na.rm = T)
        if(check_val<0) return(x)
      }
    }


    overlap <- unlist(sapply(unique(df[,id]), check_for_overlap,start=start,end=end,data=df,id=id))


    if(length(overlap)>0) {stop(paste0(    paste0("There is(are) ", length(overlap)," id(s) with overlap between bars, they are ",id ,"=(",paste (overlap,sep="", collapse=","),")")))}


    add_in <- function(id_fix,df,start,end){
      df_fix <- df[df[,id]==id_fix,]
      end_blank <- df_fix[,start][c(0,dplyr::lag(df_fix[,end])[-1]) != df_fix[,start]]
      start_blank <- c(0,dplyr::lag(df_fix[,end])[-1])[c(0,dplyr::lag(df_fix[,end])[-1]) != df_fix[,start]]
      df_fixed <- data.frame(id_fix,start_blank,end_blank)
      names(df_fixed) <- c(id,start,end)
      merge(df_fixed,df_fix,all=T)
    }
    df <- do.call(rbind.data.frame,sapply(unique(df[,id]), add_in,df=df,start=start,end=end,simplify = F))


  }else {
    start = 'starting_bars_variable'
    df$starting_bars_variable <- stats::ave(df[,end], df[,id], FUN=dplyr::lag)
    df$starting_bars_variable[is.na(df$starting_bars_variable)] <- 0
  }

  temp_end <- df[,end] - stats::ave(df[,end], df[,id], FUN=dplyr::lag)
  df[,end][!is.na(temp_end)] <- temp_end[!is.na(temp_end)]



  df <- data.frame(df)

  starting_times <- sort(unique(df[,start]),decreasing = TRUE)
  df[,start] <- factor(df[,start],starting_times)
  df[, id] <- factor(df[, id], levels = id_order)

  plot <-
    ggplot2::ggplot(df) +
    ggpattern::geom_col_pattern(position = "stack",
                                ggplot2::aes_string(fill = name_fill,col = name_col,pattern=name_pattern,pattern_fill=name_pattern_fill, group = start,x = id, y = end),...) + ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),panel.grid.major = ggplot2::element_blank())


  if(stratify[1]!=FALSE) plot <-  plot + ggplot2::facet_wrap(stats::as.formula(paste("~",paste(stratify,collapse = "+"))),scales = "free_y")+
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="black", fill="white"))

  return(plot)

}

