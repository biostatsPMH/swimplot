% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swimplot.R
\name{swimmer_text}
\alias{swimmer_text}
\title{Adding text to a swimmers plot}
\usage{
swimmer_text(
  df_text,
  id = "id",
  start = "start",
  label = "label",
  name_col = NULL,
  name_size = NULL,
  name_alpha = NULL,
  name_fontface = NULL,
  adj.y = 0,
  adj.x = 0,
  ...
)
}
\arguments{
\item{df_text}{a data frame}

\item{id}{column name for id, default is 'id'}

\item{start}{column name with the text start locations (if there is no start column will default 0 for all text)}

\item{label}{a column with the text to be added to the plot}

\item{name_col}{a column name to map the text colour}

\item{name_size}{a column name to map the text size}

\item{name_alpha}{a column name to map the text transparency}

\item{name_fontface}{a column name to map the text fontface ("plain", "bold", "italic", "bold.italic" can all be used)}

\item{adj.y}{amount to adjust the text within the box vertically (default is 0, text is in the centre of each bar)}

\item{adj.x}{amount to adjust the text within the box horizontally (default is 0, text starts at the origin)}

\item{...}{additional geom_text() arguments}
}
\value{
a swimmer plot with text on the bars
}
\description{
This function allows you to add text to a swimmers plot created with \code{\link{swimmer_plot}}
}
\examples{
#Start with a base swimmer plot

swim_plot <-
 swimmer_plot(df=ClinicalTrial.Arm,id='id',end='End_trt',
 name_fill='Arm',col="black",id_order='Arm',alpha=0.6)


# Then add text to the plot



swim_plot_with_text <- swim_plot +   swimmer_text(df_text =
ClinicalTrial.Stage,label = 'Stage',size=3,
fontface=ifelse(ClinicalTrial.Stage$Stage=="Early Stage","bold","plain"))


# Add ggplot layers to improve the plot's aesthetic

swim_plot_with_text +
ggplot2::scale_fill_manual(name="Treatment",values=c("#e41a1c", "#377eb8","#4daf4a"))+
ggplot2::ylab('Time (Days)')

}
\seealso{
\code{\link{swimmer_plot}} \code{\link{swimmer_points}} \code{\link{swimmer_lines}}  \code{\link{swimmer_points_from_lines}} \code{\link{swimmer_arrows}}
}
