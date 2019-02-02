#' not used
reach_style_barchart<-function(group,percent,error_min=NULL,error_max=NULL,horizontal=T){
  require('extrafont')
  require('ggplot2')
  require("ggthemes")
  require("dplyr")
  require('grid')

  percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}

  df<-data.frame(group=group,percent=percent,ymin=error_min,ymax=error_max)

  colnames(df)<-c('group','percent','ymin','ymax')

  theplot<-

    ggplot(df)+
    geom_bar(aes(y=1,x=group),
             stat='identity',
             fill=reach_style_color_lightgrey()) +
    geom_bar(aes(y=percent,x=group),
             stat='identity',
             fill=reach_style_color_red()) +

    geom_errorbar( aes(x=group,
                       y=percent,
                       ymin=ymin,
                       ymax=ymax),
                   stat='identity',
                   width=0) +

    geom_text(aes(x=group, y=(1), label=paste0("  ", round(percent*100),"%"," ",group)),size=4,
              family="Arial Narrow",
              col='#000000',
              hjust=0,
              vjust=0.5) +

    theme_tufte()+reachR:::reach_theme() +
    theme(text=element_text(family="Arial Narrow"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(x = c(0,0,0,0),'null')) +

    scale_y_continuous(labels = percent_formats,limits=c(0,3))

  if(horizontal){
    theplot<-theplot+coord_flip()}
  return(list(ggplot=theplot))
}



#' not used
barchart_with_error_bars <- function(hypothesis.test.results,summary.statistics){
  test_name <- hypothesis.test.results$name
  p_value <- hypothesis.test.results$result$p.value

  chart <- reach_style_barchart(group = summary.statistics$independent.var.value,
                                percent = summary.statistics$numbers,
                                error_min = summary.statistics$min,
                                error_max =  summary.statistics$max)

  chart + geom_text(aes(x =4,
                        y = 2,
                        label= paste0("p value: ", round(p_value,6)," (",test_name,")")),
                    size=3,
                    family="Arial Narrow",
                    col='#000000',
                    hjust=0,
                    vjust=0.5)
  return(list(ggplot=theplot))
  }








#' Grouped barchart for percentages
grouped_barchart_percent<-function(result){
  if(is.null(result$summary.statistic)){return(NULL)}
  if(length(result$summary.statistic)==0){return(NULL)}
  if(nrow(result$summary.statistic)==0){return(NULL)}
  attach(result)
  if(length(unique(summary.statistic$dependent.var.value))>12){
    warning("I don't do grouped barcharts with more than 12 responses. that's madness! there isn't even 12 colours!")
    return(NULL)}
  percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}




  theplot<-ggplot(summary.statistic,aes(x=independent.var.value,y=numbers,fill=dependent.var.value))+geom_bar(stat = "identity",position='dodge')+theme_tufte()+
    xlab(NULL)+ylab(NULL)+
    theme(text=element_text(family="Arial Narrow")
          # axis.title.x=element_text(summary.statistic$dependent.var.value"),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          # axis.title.y=element_blank(),
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
    )+scale_fill_reach_categorical(n=length(unique(summary.statistic$dependent.var.value)),name="")+
    scale_y_continuous(limits = c(0,1),labels = scales::percent_format())+
    geom_errorbar( aes(x=summary.statistic$independent.var.value,
                       ymin=as.numeric(summary.statistic$min),
                       ymax=as.numeric(summary.statistic$max)),
                   position=position_dodge(width=0.9),
                   stat='identity',
                   width=.1)+ theme(axis.text.x = element_text(angle = 45, hjust = 1))

  number_of_bars<-(length(unique(summary.statistic$dependent.var.value))*length(unique(summary.statistic$independent.var.value)))
  plotwidth<-5+number_of_bars*1.5
  # map_to_file(theplot,filename,height=12,width=plotwidth,unit="cm")
  detach(result)
  hg_vis<-list(ggplot=theplot,
               ggsave_parameters=list(height=12,
                                      width=plotwidth,
                                      unit="cm")
               )
  class(hg_vis)<-"hypegrammar_visualisation"
  return(hg_vis)

  }




barchart_average<-function(result){
  attach(result)
  summary.statistic$min[summary.statistic$min<0]<-0
  theplot<-ggplot(summary.statistic,aes(x=independent.var.value,y=numbers),fill=reach_style_color_darkgrey(1))+geom_bar(stat = "identity")+theme_tufte()+
    xlab("")+ylab(summary.statistic$dependent.var[1])+
    theme(text=element_text(family="Arial Narrow"),
          axis.title.x=element_blank(),
          # axis.text.x=element_text(angle=90),
          # axis.ticks.x=element_blank(),
          axis.title.y=element_blank()
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
    )+
    geom_errorbar( aes(x=summary.statistic$independent.var.value,
                       ymin=as.numeric(summary.statistic$min),
                       ymax=as.numeric(summary.statistic$max)),
                   stat='identity',
                   width=.1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

  number_of_bars<-(length(unique(summary.statistic$independent.var.value)))
  plotwidth<-5+(number_of_bars*1.5)
  # map_to_file(theplot,filename,height=12,width=plotwidth,unit="cm")
  detach(result)
  hg_vis<-list(ggplot=theplot,
               ggsave_parameters=list(height=12,width=plotwidth,unit="cm")
               )
  class(hg_vis)<-"hypegrammar_visualisation"
  return(hg_vis)
}


barchart_percent<-function(result){
  attach(result)
  theplot<-ggplot(summary.statistic,aes(x=dependent.var.value,y=numbers),fill=reach_style_color_darkgrey(1))+geom_bar(stat = "identity")+theme_tufte()+
    xlab("")+ylab(summary.statistic$dependent.var[1])+
    theme(text=element_text(family="Arial Narrow"),
          axis.title.x=element_blank(),
          # axis.text.x=element_blank(),
          # axis.ticks.x=element_blank(),
          axis.title.y=element_blank()
          # axis.text.y=element_blank(),
          # axis.ticks.y=element_blank(),
          # plot.margin = unit(x = c(0,0,0,0),'null')
    )+scale_y_continuous(limits = c(0,1),labels = scales::percent_format())+
    geom_errorbar( aes(x=summary.statistic$dependent.var.value,
                       ymin=as.numeric(summary.statistic$min),
                       ymax=as.numeric(summary.statistic$max)),
                   stat='identity',
                   width=.1) + theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5))

  number_of_bars<-(length(unique(summary.statistic$dependent.var.value)))
  plotwidth<-5+(number_of_bars*1.5)
  # map_to_file(theplot,filename,height=12,width=plotwidth,unit="cm")
  detach(result)
  hg_vis<-list(ggplot=theplot,
               ggsave_parameters=list(height=12,
                                      width=plotwidth,
                                      unit="cm")
               )
  class(hg_vis)<-"hypegrammar_visualisation"
  return(hg_vis)
  }





visualisation_barchart_percent_nogroups_FS<-function(data){
  # some parameters: make sure it's a quarter of an a4 page wide (as in the factsheets):
  A4widthincm<-27.94
  smallFSplotwdith<-(A4widthincm/4)
  # set the font size:
  fonsize=8
  # how much plot height per bar?
  heightperbarcm<-0.6

  data$prop<-data$numbers*100

  # to make sure the labels, numbers and bars behave nicely and don't overlap, I've split the plot in three, then arranging them with a grid.
  #### only bars with no labels at all
  plot_bars <- function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+theme_tufte()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.length = unit(0, "mm"))+
      coord_flip()+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))+
      ylim(c(0,100))

  }

  # font size units different in theme() and in geom_text(): factor 1/0.35
  # only labels with no bars at all
  plot_labels<-function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      # geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+
      theme_tufte()+
      theme(axis.text.x = element_blank(), axis.ticks.x=element_blank(),axis.title.x=element_blank(),text =element_text(family="Arial Narrow"))+
      theme(axis.title.y=element_blank(),axis.ticks.y=element_blank(),text =element_text(family="Arial Narrow"))+
      theme(text=element_text(family="Arial Narrow"))+
      coord_flip()+
      theme(axis.text.y =
              element_text(size  = fonsize,
                           angle = 0,
                           hjust = 0,
                           vjust = 0.5,
                           colour = "black",
                           family = "Arial Narrow"),
            axis.ticks.length = unit(0, "mm"))+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))

  }
  # only numbers with none of the other stuff
  plot_numbers<-function(data){
    ggplot(data, aes(x = reorder(dependent.var.value,prop), y = prop, width=0.5)) +
      # geom_bar(stat = "identity", fill= reach_style_color_red(),position = position_nudge(y = 5,x=0))+
      theme_tufte()+
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.ticks.length = unit(0, "mm"))+
      geom_text(aes(label=paste0(round(prop),"%")),size = fonsize * 0.352777,family="Arial Narrow",position = position_stack(vjust = 0),hjust=0)+
      coord_flip()+
      theme(plot.margin = unit(c(0,0,0,0), "cm"))
  }

  fullplot<-grid.arrange(plot_labels(data),
                         plot_numbers(data),
                         plot_bars(data), ncol=3,widths=smallFSplotwdith*c(0.4,0.1,0.5))
  detach(result)
  # ggsave(file=filename, plot=fullplot,width =smallFSplotwdith, height=heightperbarcm*length(unique(data$dependent.var.value)),units = "cm",device = "jpg",limitsize = F)

  hg_vis<-list(ggplot=fullplot,
               ggsave_parameters=list(width =smallFSplotwdith, height=heightperbarcm*length(unique(data$dependent.var.value)),units = "cm",device = "jpg",limitsize = F)
               )
  class(hg_vis)<-"hypegrammar_visualisation"
  return(hg_vis)
  return(fullplot)
}











