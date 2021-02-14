

# swim_plot -------------------------------------------------------------

#' Creating the base of a swimmers plot
#'
#' This function allows you to create swimmers plots with bars, includes options to
#' have the bars change colours and create stratified plots
#' @param df a data frame
#' @param id  column name for id, default is 'id'
#' @param end column name with the bar sizes (or bar end positions if bars change colour)
#' @param name_fill a column name to map the bar fill
#' @param name_col a column name to map the bar colour
#' @param id_order order of the bars by id, default is "increasing", can also input
#'   "decreasing", a column name, or the ids in an order.
#' @param stratify a list of column names to stratify by
#' @param base_size the base size for the plot, default is 11
#' @param ... additional geom_bar() arguments
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
#'id_order ='increasing',col="black",alpha=0.75,width=.8,base_size = 18,stratify= c('Age','Sex'))
#'
#'swim_plot_stratify +
#'ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
#'ggplot2::ylab('Time (Days)')
#'

#' @export
swimmer_plot <- function(df,id='id',end='end',name_fill=NULL,name_col=NULL,id_order = 'increasing',stratify=FALSE,base_size=11,...)
{


  df[,id] <- as.character(df[,id])


  max_time <- stats::aggregate(df[,end], by = list(df[,id]), max)
  names(max_time) <- c(id,end)

  if (id_order[1] == 'increasing') {
    id_order <-  suppressMessages(unlist(c(df %>%
                            dplyr::group_by(!!dplyr::sym(id))  %>%
                            dplyr::summarise(max_time=max(!!dplyr::sym(end)))  %>%
                            dplyr::arrange(max_time)  %>%
                            dplyr::select(!!dplyr::sym(id)))))
  }

  if (id_order[1] == 'decreasing') {
    id_order <-  suppressMessages(unlist(c(df %>%
                            dplyr::group_by(!!dplyr::sym(id))  %>%
                            dplyr::summarise(max_time=max(!!dplyr::sym(end)))  %>%
                            dplyr::arrange(dplyr::desc(max_time))  %>%
                            dplyr::select(!!dplyr::sym(id)))))
  }


  if (id_order[1] %in% names(df)) {
    id_order <- suppressMessages(unlist(c(df %>%
                            dplyr::group_by(!!dplyr::sym(id))  %>%
                            dplyr::mutate(max_time=max(!!dplyr::sym(end))) %>%
                            dplyr::top_n(-1,!!dplyr::sym(end))%>%
                            dplyr::arrange( dplyr::desc(!!dplyr::sym(id_order)),max_time) %>%
                            dplyr::select(!!dplyr::sym(id)))))
  }


  start = 'starting_bars_variable'
  df <- df %>%
    dplyr::arrange(!!dplyr::sym(id),!!dplyr::sym(end)) %>%
    dplyr::group_by(!!dplyr::sym(id))%>%
    dplyr::mutate(temporary_end=!!dplyr::sym(end)-dplyr::lag(!!dplyr::sym(end)))%>%
    dplyr::mutate(starting_bars_variable= dplyr::lag(!!dplyr::sym(end)))%>%
    dplyr::mutate(!!dplyr::sym(end):=ifelse(is.na(temporary_end),!!dplyr::sym(end),temporary_end))%>%
    dplyr::mutate(starting_bars_variable=ifelse(is.na(starting_bars_variable),0,starting_bars_variable))%>%
    dplyr::select(-temporary_end)




  # if(is.null(name_fill) & is.null(name_col)) {
  #   df <- max_time
  #   df[,start] <- 0
  #
  # }

  df <- data.frame(df)

  starting_times <- sort(unique(df[,start]),decreasing = TRUE)
  df[,start] <- factor(df[,start],starting_times)
  df[, id] <- factor(df[, id], levels = id_order)

  plot <-
    ggplot2::ggplot(df) +
    ggplot2::geom_bar(position = "stack",
                      stat = "identity",
                      ggplot2::aes_string(fill = name_fill,col = name_col, group = start,x = id, y = end),...) + ggplot2::coord_flip() +
    ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),panel.grid.major = ggplot2::element_blank())


  if(stratify[1]!=FALSE) plot <-  plot + ggplot2::facet_wrap(stats::as.formula(paste("~",paste(stratify,collapse = "+"))),scales = "free_y")+
    ggplot2::theme(strip.background = ggplot2::element_rect(colour="black", fill="white"))

  return(plot)

}


# swimmer_points ------------------------------------------------------------



#' Adding points to a swimmers plot
#'
#' This function allows you to add points to a swimmers plot created with \code{\link{swimmer_plot}}
#' @param df_points a data frame
#' @param id  column name for id, default is 'id'
#' @param time column name with the point locations
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
swimmer_points <-function(df_points,id='id',time='time',name_shape=NULL,name_col=NULL,name_size=NULL,name_fill=NULL,name_stroke=NULL,name_alpha=NULL,...)
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
        alpha=name_alpha),...)

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
swimmer_lines <- function(df_lines,id='id',start='start',end='end',name_linetype=NULL,name_col=NULL,name_size=NULL,
                          name_alpha=NULL,...){

  df_lines[,id] <- as.character(df_lines[,id])

  plot.lines <-
    ggplot2::geom_segment(
      data = df_lines,
      ggplot2::aes_string(
        x = id,
        xend = id,
        y = start,
        yend = end,
        linetype =name_linetype,
        col = name_col,
        size = name_size,
        alpha = name_alpha
      ),...
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

swimmer_points_from_lines <- function(df_lines,id='id',start = 'start',end = 'end',cont = NULL,name_shape='type',name_col=NULL,name_size=NULL,name_fill=NULL,name_stroke=NULL,name_alpha=NULL,...){

  df_points <- line_df_to_point_df(df_lines=df_lines,start = start,end = end,cont = cont)
  plot.lines.points <-  swimmer_points(df_points=df_points,id=id,time='x',name_shape=name_shape,name_col=name_col,name_size=name_size,name_fill=name_fill,name_stroke=name_stroke,name_alpha=name_alpha,...)
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
#' @param name_col a column name to map the arrow colour
#' @param cont a column name including an indicator of which ids have an arrow (NA is no arrow); when  NULL will use
#'all use all of df_arrows
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

swimmer_arrows <- function(df_arrows,id='id',arrow_start='end',name_col=NULL,cont=NULL,arrow_positions=c(0.1,1),angle=30,
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
                    type = type),...)


  return(plot.arrow)
}







# swimmer_text ------------------------------------------------------------



#' Adding text to a swimmers plot
#'
#' This function allows you to add text to a swimmers plot created with \code{\link{swimmer_plot}}
#' @param df_text a data frame
#' @param id  column name for id, default is 'id'
#' @param start column name with the text start locations (if there is no start column will default 0 for all text)


#' @param name_col a column name to map the text colour
#' @param name_size a column name to map the text size
#' @param name_alpha a column name to map the text transparency
#' @param ... additional geom_text() arguments
#' @return a swimmer plot with text on the bars
#' @seealso  \code{\link{swimmer_plot}} \code{\link{swimmer_points}} \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}} \code{\link{swimmer_arrows}}
#' @examples
#' #Start with a base swimmer plot
#'
#' swim_plot <-
#'  swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order='Arm',alpha=0.6)
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
                         name_alpha=NULL,name_fontface=NULL,hjust =0,...){


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
      ),hjust=hjust,...
    )
  return(plot.lines)


}





