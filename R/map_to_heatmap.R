#' Heatmaps from `result` objects
#' @param result a hypegrammaR result object (can be made with map_to_result())
#' @details to add labels, use `myresult %>% map_to_labeled %>% map_to_result`. The easiest way to save this to a file is with `map_to_file()`. The first elemnent of the returned list is a ggplot object, so you can add/overwrite ggplot elements to it.
#' @return A hypegrammaR visualisation object, which is a list with two elements, 1) a ggplot object and 2) recommended parameters to pass to ggsave.
#' @export
map_to_visualisation_heatmap<-function(result){

  # iscat<-grepl("categorical",case)
  # list_all_cases(implemented_only = T) %>% paste0("if(grepl(\"",,"\"","){return()}\n") %>% cat
  if(grepl("_categorical_categorical",result$parameters$case)){
    return(heatmap_categorical(result))
    }
  if(grepl("_numerical_",result$parameters$case)){
    return(heatmap_numerical(result))
    }
  if(grepl("categorical_$",result$parameters$case)){
    return(heatmap_categorical_nogroups(result))
    }

  return(function(...){return(NULL)})
}




gg_heatmap_generic<-function(summary.statistic){
  summary.statistic<-summary.statistic[order(summary.statistic$numbers),]
  theplot<-ggplot(summary.statistic)+
    geom_tile(aes(x=dependent.var.value,y=independent.var.value,fill=numbers))+
    xlab("")+ylab("")+coord_fixed()
  theplot %<>%  gg_reach_style()
  return(theplot)
}


heatmap_categorical<-function(result){
attach(result)
  vis<-  list(ggplot=gg_heatmap_generic(summary.statistic)+reach_style_scale_fill_gradient_percent(),
              ggsave_parameters = list(
                width = 10+0.5*length(unique(summary.statistic$dependent.var.value)),
                height = 15+0.5*length(unique(summary.statistic$independent.var.value)),
                units = "cm",
                limitsize = "cm"))
    # ggsave(filename = filename,
    #        plot = theplot,
    #        units = "cm",
    #        width = 10+0.5*length(unique(summary.statistic$dependent.var.value)),
    #        height = 15+0.5*length(unique(summary.statistic$independent.var.value)),
    #        limitsize = F)
    # )
  class(vis)<-"hypegrammar_visualisation"
  detach(result)
  return(vis)
}



heatmap_categorical_nogroups<-function(result){
  attach(result)
  vis<-list(ggplot = gg_heatmap_generic(summary.statistic)+reach_style_scale_fill_gradient_percent()+
    theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    # coord_flip()+
    theme(aspect.ratio = length(unique(summary.statistic$independent.var.value))/
            length(unique(summary.statistic$dependent.var.value))),
    ggsave_parameters = list(          units = "cm",
                                       width = 10+0.5*length(unique(summary.statistic$dependent.var.value)),
                                       height = 15+0.5*length(unique(summary.statistic$independent.var.value)),
                                       limitsize = F))

  class(vis)<-"hypegrammar_visualisation"
  return(vis)
  }


heatmap_numerical<-function(result){
  attach(result)
  vis<-list(ggplot = gg_heatmap_generic(summary.statistic)+
    reach_style_scale_fill_gradient_numeric()+
    theme(axis.text.y = element_blank(),axis.ticks.y = element_blank())+
    coord_flip()+
    theme(aspect.ratio = length(unique(summary.statistic$dependent.var.value))/length(unique(summary.statistic$independent.var.value))
    ),

  ggplot_parameters = list(
    units = "cm",
    width = 10+0.5*length(unique(summary.statistic$dependent.var.value)),
    height = 15+0.5*length(unique(summary.statistic$independent.var.value)),
    limitsize = F))
  class(vis)<-"hypegrammar_visualisation"
  return(vis)
}


reach_style_scale_fill_gradient_numeric<-function(...){
  scale_fill_gradient(
    name="",
    position="top",
    low=reach_style_color_darkgrey(1),
    high=reach_style_color_red(1),
    guide = "legend",
    ...
  )
}


reach_style_scale_fill_gradient_percent<-function(...){
  scale_fill_gradient(
    name="",
    position="top",
    low=reach_style_color_darkgrey(1),
    high=reach_style_color_red(1),
    limits=c(0,1),
    guide = "legend",
    labels = scales::percent_format(),
    ...
  )
}

gg_reach_style<-function(ggobject){
  return(ggobject+theme_tufte()+theme(axis.text.x = element_text(angle = 90, hjust = 1,vjust=0.5),text = element_text(family = "Arial Narrow")))
}

