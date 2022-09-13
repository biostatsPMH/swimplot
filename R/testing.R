




# No changes all positive -------------------------------------------------
df <- data.frame(id=c(1,2,3,4,5),start=c(0,0,0,0,0),end=c(1,2,3,4,5),group1=c(5,5,5,10,10),group2=c('A',"B","B","A","C"))


##BASIC
  swimmer_plot(df)
##FILL BY ID
  swimmer_plot(df,name_fill='id')
##FILL BY CONTINUOUS
  swimmer_plot(df,name_fill='group1')
##FILL BY CATEGORIC
  swimmer_plot(df,name_fill='group2')
##ALPHA
  swimmer_plot(df,name_alpha='group1')
  swimmer_plot(df,name_alpha='group2')
##COL
  swimmer_plot(df,name_col='group2')
##ORDER BY VARIABLE
  swimmer_plot(df,id_order='group2',name_fill='group2')
  swimmer_plot(df,id_order='group2',increasing = F,name_fill='group2')
##ORDER
  swimmer_plot(df,id_order=c(4,5,1,2,3),name_fill='group2')

##STRATIFY
  swimmer_plot(df,name_fill='group2',stratify = 'group1')
  swimmer_plot(df,name_fill='group2',stratify = c('group1','group2'))


# No changes all negative, end positive -----------------------------------
df <- data.frame(id=c(1,2,3,4,5),start=c(-5,-4,-3,-2,-1),end=c(1,2,3,4,5),group1=c(5,5,5,10,10),group2=c('A',"B","B","A","C"))

##BASIC
swimmer_plot(df)
##FILL BY ID
swimmer_plot(df,name_fill='id')
##FILL BY CONTINUOUS
swimmer_plot(df,name_fill='group1')
##FILL BY CATEGORIC
swimmer_plot(df,name_fill='group2')
##ALPHA
swimmer_plot(df,name_alpha='group1')
swimmer_plot(df,name_alpha='group2')
##COL
swimmer_plot(df,name_col='group2')
##ORDER BY VARIABLE
swimmer_plot(df,id_order='group2',name_fill='group2')
swimmer_plot(df,id_order='group2',increasing = F,name_fill='group2')
##ORDER
swimmer_plot(df,id_order=c(4,5,1,2,3),name_fill='group2')

##STRATIFY
swimmer_plot(df,name_fill='group2',stratify = 'group1')
swimmer_plot(df,name_fill='group2',stratify = c('group1','group2'))



# No changes all negative end at zero -------------------------------------
df <- data.frame(id=c(1,2,3,4,5),start=c(-5,-4,-3,-2,-1),end=c(0,0,0,0,0),group1=c(5,5,5,10,10),group2=c('A',"B","B","A","C"))

##BASIC
swimmer_plot(df)
##FILL BY ID
swimmer_plot(df,name_fill='id')
##FILL BY CONTINUOUS
swimmer_plot(df,name_fill='group1')
##FILL BY CATEGORIC
swimmer_plot(df,name_fill='group2')
##ALPHA
swimmer_plot(df,name_alpha='group1')
swimmer_plot(df,name_alpha='group2')
##COL
swimmer_plot(df,name_col='group2')
##ORDER BY VARIABLE
swimmer_plot(df,id_order='group2',name_fill='group2')
swimmer_plot(df,id_order='group2',increasing = F,name_fill='group2')
##ORDER
swimmer_plot(df,id_order=c(4,5,1,2,3),name_fill='group2')

##STRATIFY
swimmer_plot(df,name_fill='group2',stratify = 'group1')
swimmer_plot(df,name_fill='group2',stratify = c('group1','group2'))

# No changes all end negative ---------------------------------------------
df <- data.frame(id=c(1,2,3,4,5),start=c(-5,-4,-3,-2,-1),end=c(-1,-1,-1,-1,-0.5),col=c(1,2,3,4,5),group1=c(5,5,5,10,10),group2=c('A',"B","B","A","C"))

##BASIC
swimmer_plot(df)
##FILL BY ID
swimmer_plot(df,name_fill='id')
##FILL BY CONTINUOUS
swimmer_plot(df,name_fill='group1')
##FILL BY CATEGORIC
swimmer_plot(df,name_fill='group2')
##ALPHA
swimmer_plot(df,name_alpha='group1')
swimmer_plot(df,name_alpha='group2')
##COL
swimmer_plot(df,name_col='group2')
##ORDER BY VARIABLE
swimmer_plot(df,id_order='group2',name_fill='group2')
swimmer_plot(df,id_order='group2',increasing = F,name_fill='group2')
##ORDER
swimmer_plot(df,id_order=c(4,5,1,2,3),name_fill='group2')

##STRATIFY
swimmer_plot(df,name_fill='group2',stratify = 'group1')
swimmer_plot(df,name_fill='group2',stratify = c('group1','group2'))

# No changes some positive some negative ----------------------------------

df <- data.frame(id=c(1,2,3,4,5),
                 start=c(-5,-4,0,1,2),end=c(-1,2,3,4,5),col=c(1,2,3,4,5),group1=c(5,5,5,10,10),group2=c('A',"B","B","A","C"))

##BASIC
swimmer_plot(df)+ggplot2::scale_fill_manual(name="Treatment",
                                            values=c("#e41a1c",na.value=NA))+ ggplot2::theme(legend.position = "none")
##FILL BY ID
swimmer_plot(df,name_fill='id')
##FILL BY CONTINUOUS
swimmer_plot(df,name_fill='group1')
##FILL BY CATEGORIC
swimmer_plot(df,name_fill='group2')
##ALPHA
swimmer_plot(df,name_alpha='group1')
swimmer_plot(df,name_alpha='group2')
##COL
swimmer_plot(df,name_col='group2')
##ORDER BY VARIABLE
swimmer_plot(df,id_order='group2',name_fill='group2')
swimmer_plot(df,id_order='group2',increasing = F,name_fill='group2')
##ORDER
swimmer_plot(df,id_order=c(4,5,1,2,3),name_fill='group2')

##STRATIFY
swimmer_plot(df,name_fill='group2',stratify = 'group1')
swimmer_plot(df,name_fill='group2',stratify = c('group1','group2'))

# Changes all positive ----------------------------------------------------



# Changes crossing 0 ------------------------------------------------------






