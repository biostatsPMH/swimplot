

# swim_plot -------------------------------------------------------------

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
#' @param name_alpha a column name to map the bar transparency
#' @param id_order order of the bars by id, can input a column name to sort by, or the ids in order.
#' @param increasing Binary to specify bars in increasing order (Default is TRUE)
#' @param stratify a list of column names to stratify by
#' @param base_size the base size for the plot, default is 11
#' @param identifiers Binary to specify patient identifiers are included in the y axis (default is TRUE)
#' @param ... additional geom_col() arguments
#' @return a swimmer plot with bars
#' @seealso \code{\link{swimmer_points}} \code{\link{swimmer_lines}}  \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}} \code{\link{swimmer_arrows}} \code{\link{swimmer_text}}
#' @examples
#'
#'
#'
#'
#'swim_plot <-
#'swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order='Arm')
#'
#'# Add ggplot layers to improve the plot's aesthetic
#'
#' swim_plot +
#' ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#' ggplot2::ylab('Time (Days)')
#'
#'
#'
#'#Example with Stratification
#'
#'swim_plot_stratify <- swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',
#'col="black",alpha=0.75,width=.8,base_size = 18,stratify= c('Age','Sex'))
#'
#'swim_plot_stratify +
#'ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#'ggplot2::ylab('Time (Days)')
#'
#'#Example when there are gaps between the bars and bars do not start at zero
#'
#'#Both a start and end time need to be specified when there are gaps between sections of bars
#'
#'Gap_data <- data.frame(patient_ID=c('ID:3','ID:1','ID:1','ID:1','ID:2',
#'                                    'ID:2','ID:2','ID:3','ID:3','ID:2'),
#'                       start=c(10,1,2,7,2,10,14,5,0,22),
#'                       end=c(20,2,4,10,7,14,22,7,3,26),
#'                       treatment=c("A","B","C","A","A","C","A","B","C",NA))
#'
#'swimmer_plot(df=Gap_data,id='patient_ID',name_fill="treatment",col=1,identifiers=FALSE,
#'id_order = c('ID:1','ID:2','ID:3')) +
#' ggplot2::theme_bw()+ggplot2::scale_fill_manual(name="Treatment",
#' values=c("A"="#e41a1c", "B"="#377eb8","C"="#4daf4a",na.value=NA),breaks=c("A","B","C"))+
#'  ggplot2::scale_y_continuous(breaks=c(0:26))
#'

#' @export
swimmer_plot <- function(df,id='id',end='end',start='start',name_fill=NULL,name_col=NULL,name_alpha=NULL,increasing=TRUE,id_order = NULL,
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

    ##Removing rows that start before 0
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
      if(length(end_blank)==0) return(df_fix)
      start_blank <- c(0,dplyr::lag(df_fix[,end])[-1])[c(0,dplyr::lag(df_fix[,end])[-1]) != df_fix[,start]]
      df_fixed <- data.frame(id_fix,start_blank,end_blank)
      names(df_fixed) <- c(id,start,end)
      return(merge(df_fixed,df_fix,all=T))
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
    ggplot2::geom_col(position = "stack",
                      ggplot2::aes_string(fill = name_fill,col = name_col,alpha=name_alpha, group = start,x = id, y = end),...) + ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),panel.grid.major = ggplot2::element_blank())


  if(stratify[1]!=FALSE) plot <-  plot + ggplot2::facet_wrap(stats::as.formula(paste("~",paste(stratify,collapse = "+"))),scales = "free_y")+
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="black", fill="white"))


  if(identifiers==FALSE) plot <-  plot + ggplot2::theme(axis.title.y=ggplot2::element_blank(),
                                                        axis.text.y=ggplot2::element_blank(),
                                                        axis.ticks.y=ggplot2::element_blank())

  return(plot)

}


# swimmer_points ------------------------------------------------------------



#' Adding points to a swimmers plot
#'
#' This function allows you to add points to a swimmers plot created with \code{\link{swimmer_plot}}
#' @param df_points a data frame
#' @param id  column name for id, default is 'id'
#' @param time column name with the point locations
#' @param adj.y amount to adjust the point within the box vertically (default is 0, point is in the centre of each bar)
#' @param name_shape a column name to map the point shape
#' @param name_col a column name to map the point colour
#' @param name_size a column name to map the point size
#' @param name_fill a column name to map the point fill
#' @param name_stroke a column name to map the point stroke
#' @param name_alpha a column name to map the point transparency
#' @param ... additional geom_point() arguments
#' @return a swimmer plot with points
#' @seealso \code{\link{swimmer_plot}} \code{\link{swimmer_lines}}  \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}} \code{\link{swimmer_arrows}} \code{\link{swimmer_text}}
#' @examples
#'
#'
#'
#' #Start with a base swimmer plot
#'
#' swim_plot <-
#'  swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order='Arm')
#'
#'
#' # Then add points to the plot
#'
#' swim_plot_with_points <- swim_plot + swimmer_points(df_points=
#' ClinicalTrial.AE,id='id',time='time',name_shape =
#' 'event',size=3,fill='white',col='black')
#'
#'
#'
#' # Add ggplot layers to improve the plot's aesthetic
#'
#' swim_plot_with_points + ggplot2::scale_shape_manual(name="Adverse
#' event",values=c(21,24,17),breaks=c('AE','SAE','Death'))+
#' ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#' ggplot2::ylab('Time (Days)')
#'
#'
#'
#'##Another example with the colour and shape mapped to different columns
#'
#'#Start with a base swimmer plot
#'
#'swim_plot <-
#'  swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order='Arm')
#'
#'
#'swim_plot +
#'  swimmer_points(df_points=ClinicalTrial.AE,id='id',time='time',name_shape =
#'                  'event',fill='white',name_col = 'Related',size=5)+
#'  ggplot2::scale_shape_manual(name="Adverse event",values=c(16,17,18),breaks=c('AE','SAE','Death'))+
#'  ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#'  ggplot2::ylab('Time (Days)') +
#'  ggplot2::scale_color_manual(name="Likelihood related to treatment",values=c(1,'grey52','grey90'))
#'
#'
#'
#'
#' @export
swimmer_points <-function(df_points,id='id',time='time',adj.y=0,name_shape=NULL,name_col=NULL,name_size=NULL,name_fill=NULL,name_stroke=NULL,name_alpha=NULL,...)
{

  df_points[,id] <- as.character(df_points[,id])


  plot.pt <-
    ggplot2::geom_point(
      data = df_points,
      ggplot2::aes_string(
        id,time,
        shape = name_shape,
        col = name_col,
        size = name_size,
        fill=name_fill,
        stroke=name_stroke,
        alpha=name_alpha),position = ggplot2::position_nudge(x = adj.y, y = 0),...)

  return(plot.pt)
}


# swimmer_lines -----------------------------------------------------------



#' Adding lines to a swimmers plot
#'
#' This function allows you to add lines to a swimmers plot created with \code{\link{swimmer_plot}}
#' @param df_lines a data frame
#' @param id  column name for id, default is 'id'
#' @param start column name with the line start locations
#' @param end column name with the line end locations
#' @param adj.y amount to adjust the line within the box vertically (default is 0, line is in the centre of each bar)
#' @param name_linetype a column name to map the line type
#' @param name_col a column name to map the line colour
#' @param name_size a column name to map the line size
#' @param name_alpha a column name to map the line transparency
#' @param ... additional geom_segment() arguments
#' @return a swimmer plot with lines
#' @seealso  \code{\link{swimmer_plot}} \code{\link{swimmer_points}} \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}} \code{\link{swimmer_arrows}} \code{\link{swimmer_text}}
#' @examples
#'
#'
#'#Start with a base swimmer plot
#'swim_plot <-
#'swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order='Arm')
#'
#'
#'# Then add lines to the plot
#'
#'swim_plot_with_lines <- swim_plot +
#'swimmer_lines(df_lines=ClinicalTrial.Response,id='id',start =
#''Response_start',end='Response_end',name_col='Response',size=3)
#'
#'
#'# Add ggplot layers to improve the plot's aesthetic
#'
#'swim_plot_with_lines +
#'ggplot2::scale_color_manual(name="Response",values=c("grey20","grey80"))+
#'ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#'ggplot2::ylab('Time (Days)')
#'
#' @export
swimmer_lines <- function(df_lines,id='id',start='start',end='end',adj.y=0,name_linetype=NULL,name_col=NULL,name_size=NULL,
                          name_alpha=NULL,...){

  df_lines[,id] <- as.character(df_lines[,id])


  plot.lines <-
    ggplot2::geom_segment(
      data = df_lines,
      ggplot2::aes_string(
        x = id ,
        xend = id,
        y = start,
        yend = end,
        linetype =name_linetype,
        col = name_col,
        size = name_size,
        alpha = name_alpha
      ),position = ggplot2::position_nudge(x = adj.y, y = 0),...
    )
  return(plot.lines)


}



# line_df_to_point_df -----------------------------------------------------


#' Formats a dataframe of line to add points
#'
#' This function formats a dataframe; used with \code{\link{swimmer_lines}}
#' @param df_lines a dataframe
#' @param start start column name
#' @param end end column name
#' @param cont continue column name
#' @return a dataframe in a format for adding points to a swimmers plot
#'
#' @export
#'
line_df_to_point_df <-function(df_lines,start = 'start',end = 'end',cont = NULL) {

  df_points_lines <- tidyr::gather_(data=df_lines, key_col = "type",
                             value_col = "x", gather_cols=c(start,end))

  if(!is.null(cont)){
    df_points_lines <- df_points_lines[!(!is.na(df_points_lines[,cont]) & df_points_lines$type== end),]
  }

  return(df_points_lines)
}


# swimmer_points_from_lines -----------------------------------------------



#' Adding points to a swimmers plot which match up with lines
#'
#'This function will create points at the beginning and end of line to match with  \code{\link{swimmer_lines}}.
#' @param df_lines a data frame
#' @param id column name for id, default is 'id'
#' @param start column name where the line starts, default is 'start'
#' @param end column name where the line ends, default is 'end'
#' @param cont a column name of which lines continue (NA is does not continue) these will not have a point at the end of the line
#' @param adj.y amount to adjust the point within the box vertically (default is 0, point is in the centre of each bar)
#' @param name_shape a column name to map the point shape
#' @param name_col a column name to map the point colour
#' @param name_size a column name to map the point size
#' @param name_fill a column name to map the point fill
#' @param name_stroke a column name to map the point stroke
#' @param name_alpha a column name to map the point transparency
#' @param ...  additional geom_point() arguments
#' @return a swimmer plot with points matching the lines
#' @seealso \code{\link{swimmer_plot}} \code{\link{swimmer_points}} \code{\link{swimmer_lines}}  \code{\link{swimmer_lines}}  \code{\link{swimmer_arrows}} \code{\link{swimmer_text}}
#' @examples
#'
#'
#'#Start with a base swimmer plot
#'
#'
#'swim_plot <-swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black"
#',id_order= 'Arm')
#'
#'
#'# Then add lines to the plot
#'
#'swim_plot_with_lines <- swim_plot +
#'swimmer_lines(df_lines=ClinicalTrial.Response,id='id',start =
#''Response_start',end='Response_end',name_col='Response',size=3)
#'
#'# Add points to the start and end of the lines
#'
#'swim_plot_with_lines_and_points <- swim_plot_with_lines+
#'swimmer_points_from_lines(df_lines=ClinicalTrial.Response,id='id',start =
#''Response_start',end = 'Response_end', cont =
#''Continued_response',name_col='Response',size=4)
#'
#'# Add ggplot layers to improve the plot's aesthetic
#'
#'swim_plot_with_lines_and_points +
#'ggplot2::scale_color_manual(name="Response",values=c("grey20","grey80"))+
#'ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#'ggplot2::ylab('Time (Days)')+
#'ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape =
#'NA)))+
#'ggplot2::scale_shape_manual(name='',values=c(17,15),breaks =
#'c('Response_start','Response_end'),labels=c('Response Start','Response End'))
#' @export

swimmer_points_from_lines <- function(df_lines,id='id',start = 'start',end = 'end',cont = NULL,adj.y=0,name_shape='type',name_col=NULL,name_size=NULL,name_fill=NULL,name_stroke=NULL,name_alpha=NULL,...){

  df_points <- line_df_to_point_df(df_lines=df_lines,start = start,end = end,cont = cont)
  plot.lines.points <-  swimmer_points(df_points=df_points,id=id,time='x',adj.y=adj.y,name_shape=name_shape,name_col=name_col,name_size=name_size,name_fill=name_fill,name_stroke=name_stroke,name_alpha=name_alpha,...)
  return(plot.lines.points)
}


# swimmer_arrows ----------------------------------------------------------


#' Adding arrows to a swimmers plot
#'
#' This function allows you to add arrows to a swimmers plot created with
#' \code{\link{swimmer_plot}}
#' @param df_arrows a data frame
#' @param id  column name for id, default is 'id'
#' @param arrow_start column name with the arrow locations default is "end"
#' @param cont a column name including an indicator of which ids have an arrow (NA is no arrow); when  NULL will use
#'all use all of df_arrows
#' @param adj.y amount to adjust the line within the box vertically (default is 0, line is in the centre of each bar)
#' @param name_col a column name to map the arrow colour
#' @param arrow_positions a vector of the distance from the arrow start to end,
#'   default is c(0.1,1)
#' @param angle the angle of the arrow head in degrees (smaller numbers produce
#'   narrower, pointier arrows). Essentially describes the width of the arrow
#'   head. Default is 30
#' @param length a unit specifying the length of the arrow head (from tip to
#'   base in inches (default is 0.1)'
#' @param type one of "open" or "closed" indicating whether the arrow head
#'   should be a closed triangle. Default is 'closed'
#' @param ... additional geom_segment() arguments
#' @return a swimmer plot with arrows
#' @seealso \code{\link{swimmer_plot}} \code{\link{swimmer_points}} \code{\link{swimmer_lines}}  \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}}  \code{\link{swimmer_text}}
#' @examples
#'
#' #Mapping the arrows to the bars
#'
#'
#Start with a base swimmer plot
#'

#'
#'swim_plot <-
#'swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order
#'= 'Arm')
#'
#'
# Then add arrows to the plot
#'
#'swim_plot_with_arrows <- swim_plot+
#'swimmer_arrows(df_arrows=ClinicalTrial.Arm,id='id',arrow_start='End_trt',
#'cont = 'Continued_treatment',name_col='Arm',show.legend = FALSE,type =
#'"open",cex=1.25)
#'
#'
# Add ggplot layers to improve the plot's aesthetic
#'
#'  swim_plot_with_arrows+
#'ggplot2::scale_color_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"),drop=FALSE)+
#'ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#'ggplot2::ylab('Time (Days)')
#'
#'
#'
#'
#'
#'
#' #Mapping the arrows to lines
#'
#' #Start with a base swimmer plot with lines and points
#'
#'
#' swim_plot <-
#' swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order
#' = 'Arm')+ swimmer_lines(df_lines=ClinicalTrial.Response,id='id',start =
#' 'Response_start',end='Response_end',name_col='Response',size=3)+
#' swimmer_points_from_lines(df_lines=ClinicalTrial.Response,id='id',start =
#' 'Response_start',end = 'Response_end',cont =
#' 'Continued_response',name_col='Response',size=4)
#'
#'
#' # Then add arrows to the plot
#'
#'   swim_plot_with_arrows <- swim_plot+
#' swimmer_arrows(df_arrows=ClinicalTrial.Response,id='id',arrow_start='Response_end',
#' cont = 'Continued_response',name_col='Response',show.legend = FALSE,type =
#' "open",cex=1.25)
#'
#'
#' # Add ggplot layers to improve the plot's aesthetic
#'
#' swim_plot_with_arrows+
#' ggplot2::scale_color_manual(name="Response",values=c("grey20","grey80"))+
#' ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#' ggplot2::ylab('Time (Days)')+
#' ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(shape =
#' NA)))+
#' ggplot2::scale_shape_manual(name='',values=c(17,15),breaks =
#' c('Response_start','Response_end'),labels=c('Response Start','Response End'))
#'
#'
#'@export

swimmer_arrows <- function(df_arrows,id='id',arrow_start='end',cont=NULL,adj.y=0,name_col=NULL,arrow_positions=c(0.1,1),angle=30,
                           length = 0.1,type='closed',...){

  df_arrows[,name_col] <- factor(df_arrows[,name_col])

  if(!is.null(cont)){
    df_arrows <- df_arrows[!is.na(df_arrows[,cont]),]
  }


  df_arrows[,id] <- as.character(df_arrows[,id])

  df_arrows$start <- df_arrows[,arrow_start] + arrow_positions[1]
  df_arrows$end <- df_arrows[,arrow_start] + arrow_positions[2]


  plot.arrow <-
    ggplot2::geom_segment(
      data = df_arrows,
      ggplot2::aes_string(
        x = id,
        xend = id,
        y = 'start',
        yend = 'end',
        col = name_col
      ),arrow=ggplot2::arrow(angle = angle, length = ggplot2::unit(length, "inches"),
                    type = type),position = ggplot2::position_nudge(x = adj.y, y = 0),...)


  return(plot.arrow)
}







# swimmer_text ------------------------------------------------------------



#' Adding text to a swimmers plot
#'
#' This function allows you to add text to a swimmers plot created with \code{\link{swimmer_plot}}
#' @param df_text a data frame
#' @param id  column name for id, default is 'id'
#' @param start column name with the text start locations (if there is no start column will default 0 for all text)
#' @param adj.y amount to adjust the text within the box vertically (default is 0, text is in the centre of each bar)
#' @param adj.x amount to adjust the text within the box horizontally (default is 0, text starts at the origin)
#' @param label a column with the text to be added to the plot
#' @param name_col a column name to map the text colour
#' @param name_size a column name to map the text size
#' @param name_alpha a column name to map the text transparency
#' @param name_fontface a column name to map the text fontface ("plain", "bold", "italic", "bold.italic" can all be used)
#' @param ... additional geom_text() arguments
#' @return a swimmer plot with text on the bars
#' @seealso  \code{\link{swimmer_plot}} \code{\link{swimmer_points}} \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}} \code{\link{swimmer_arrows}}
#' @examples
#' #Start with a base swimmer plot
#'
#' swim_plot <-
#'  swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',
#'  name_fill='Arm',col="black",id_order='Arm',alpha=0.6)
#'
#'
#' # Then add text to the plot
#'
#'
#'
#' swim_plot_with_text <- swim_plot +   swimmer_text(df_text =
#'ClinicalTrial.Stage,label = 'Stage',size=3,
#'fontface=ifelse(ClinicalTrial.Stage$Stage=="Early Stage","bold","plain"))
#'
#'
#' # Add ggplot layers to improve the plot's aesthetic
#'
#' swim_plot_with_text +
#' ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#' ggplot2::ylab('Time (Days)')
#'
#' @export

swimmer_text <- function(df_text,id='id',start='start',label='label',name_col=NULL,name_size=NULL,
                         name_alpha=NULL,name_fontface=NULL,adj.y=0,adj.x=0,...){


  if(!start %in% colnames(df_text))  df_text[,start] <- 0

  df_text[,id] <- as.character(df_text[,id])

  plot.lines <-
    ggplot2::geom_text(
      data = df_text,
      ggplot2::aes_string(
        label=label,
        x = id,
        y = start,
        col = name_col,
        size = name_size,
        alpha = name_alpha,
        fontface =name_fontface
      ),hjust=0,position = ggplot2::position_nudge(x = adj.y, y = adj.x),...
    )
  return(plot.lines)


}





