% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swimplot.R
\name{swimmer_points}
\alias{swimmer_points}
\title{Adding points to a swimmers plot}
\usage{
swimmer_points(
  df_points,
  id = "id",
  time = "time",
  adj.y = 0,
  name_shape = NULL,
  name_col = NULL,
  name_size = NULL,
  name_fill = NULL,
  name_stroke = NULL,
  name_alpha = NULL,
  ...
)
}
\arguments{
\item{df_points}{a data frame}

\item{id}{column name for id, default is 'id'}

\item{time}{column name with the point locations}

\item{adj.y}{amount to adjust the point within the box vertically (default is 0, point is in the centre of each bar)}

\item{name_shape}{a column name to map the point shape}

\item{name_col}{a column name to map the point colour}

\item{name_size}{a column name to map the point size}

\item{name_fill}{a column name to map the point fill}

\item{name_stroke}{a column name to map the point stroke}

\item{name_alpha}{a column name to map the point transparency}

\item{...}{additional geom_point() arguments}
}
\value{
a swimmer plot with points
}
\description{
This function allows you to add points to a swimmers plot created with \code{\link{swimmer_plot}}
}
\examples{



#Start with a base swimmer plot

swim_plot <-
 swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order='Arm')


# Then add points to the plot

swim_plot_with_points <- swim_plot + swimmer_points(df_points=
ClinicalTrial.AE,id='id',time='time',name_shape =
'event',size=3,fill='white',col='black')



# Add ggplot layers to improve the plot's aesthetic

swim_plot_with_points + ggplot2::scale_shape_manual(name="Adverse
event",values=c(21,24,17),breaks=c('AE','SAE','Death'))+
ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
ggplot2::ylab('Time (Days)')



##Another example with the colour and shape mapped to different columns

#Start with a base swimmer plot

swim_plot <-
 swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',name_fill='Arm',col="black",id_order='Arm')


swim_plot +
 swimmer_points(df_points=ClinicalTrial.AE,id='id',time='time',name_shape =
                 'event',fill='white',name_col = 'Related',size=5)+
 ggplot2::scale_shape_manual(name="Adverse event",values=c(16,17,18),breaks=c('AE','SAE','Death'))+
 ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
 ggplot2::ylab('Time (Days)') +
 ggplot2::scale_color_manual(name="Likelihood related to treatment",values=c(1,'grey52','grey90'))




}
\seealso{
\code{\link{swimmer_plot}} \code{\link{swimmer_lines}}  \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}} \code{\link{swimmer_arrows}} \code{\link{swimmer_text}}
}
