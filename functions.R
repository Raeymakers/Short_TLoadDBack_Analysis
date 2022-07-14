#############################
#                           #
##  Function corresponding ##
#     to TLoadDback    #
#############################


library(ggplot2) # figures

##### Functions for preprocessingData.R #####
# A function that takes the last n characters of a string
substrRight <- function(x, n){ 
  substr(x, nchar(x)-n+1, nchar(x))}


##### Visualisations #####
### Violin plot ####
geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim, scale = scale, ...)
    )
  }

GeomFlatViolin <- ggproto(
  "GeomFlatViolin",
  Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    data %>%
      group_by(group) %>%
      mutate(
        ymin = min(y),
        ymax = max(y),
        xmin = x,
        xmax = x + width / 2
      )
  },
  draw_group = function(data, panel_scales, coord) {
    # Find the points for the line to go all the way around
    data <-
      transform(data,
                xminv = x,
                xmaxv = x + violinwidth * (xmax - x))
    # Make sure it's sorted properly to draw the outline
    newdata <-
      rbind(plyr::arrange(transform(data, x = xminv), y),
            plyr::arrange(transform(data, x = xmaxv), -y))
    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    ggplot2:::ggname("geom_flat_violin",
                     GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },
  draw_key = draw_key_polygon,
  default_aes = aes(
    weight = 1,
    colour = "grey20",
    fill = "white",
    size = 0.5,
    alpha = NA,
    linetype = "solid"
  ),
  required_aes = c("x", "y")
)

#function for split violin
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))}
                             else {ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...)) }})

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))}


one_way_plotf <-function(data, emmean_dataframe, var){
  ggplot(data, aes(x= Condition, y = .data[[var]]))+
    geom_flat_violin(aes(fill=Condition),position = position_nudge(x =.2, y = 0), alpha=.5, adjust = 1.5, colour = NA)+
    geom_boxplot(aes(x = Condition, y = .data[[var]], fill = Condition), outlier.shape=NA, alpha= .45, width = .1, colour = "black") +
    geom_point(data= emmean_dataframe, aes(x = Condition, y = emmean, fill=Condition), size=4)+
    theme(
      legend.key.size=unit(1.3, 'cm'),
      legend.text=element_text(size=13),
      plot.title = element_text(size=rel(2)),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major.y = element_line( size=.1, color="#dedede" ),
      axis.text.x=element_text(size=rel(1.5)),
      axis.title.y=element_text(size=rel(1.4)),
      axis.title.x = element_blank())
}

two_way_plotf <- function(data, emmean_dataframe, var){
  ggplot()+
    geom_flat_violin(data= data, aes(x= Day, y= .data[[var]], fill=Condition),position = position_nudge(x =.3, y = 0), adjust = 1.5, alpha = .5, colour = NA)+
    geom_boxplot(data= data, aes(x=Day, y=.data[[var]], fill=Condition), outlier.shape=NA, alpha=.5, width=.3, colour='black')+
    geom_point(data= emmean_dataframe, aes(x = Day, y = emmean, fill=Condition), position= position_dodge(0.3), size=4)+
    theme(
      legend.text=element_text(size=13), # text legend bigger
      panel.border = element_blank(), # no border panel (APA)
      panel.background = element_blank(), #white simple background
      axis.line = element_line(colour = "black"), # axis lines black
      panel.grid.major.y = element_line( size=.1, color="#dedede") #slight grey horizontal lines
    )
      }

one_w_plot <- function(data, Dataframe, xvar, yvar, Mean, title){
  ggplot(EXP, aes(x=  .data[[xvar]], y = .data[[yvar]]))+
    geom_flat_violin(aes(fill= .data[[xvar]]),position = position_nudge(x =.2, y = 0), alpha=.5, adjust = 1.5, colour = NA)+
    geom_boxplot(aes(x =  .data[[xvar]], y =  .data[[yvar]], fill =  .data[[xvar]]), outlier.shape=NA, alpha= .45, width = .1, colour = "black") +
    geom_point(data= Dataframe, aes(x = .data[[xvar]], y = .data[[Mean]], fill=.data[[xvar]]), position= position_dodge(0.1), size=4)+
    ggtitle(title)+
    theme(
      legend.text=element_text(size=13), # text legend bigger
      panel.border = element_blank(), # no border panel (APA)
      panel.background = element_blank(), #white simple background
      axis.line = element_line(colour = "black"), # axis lines black
      panel.grid.major.y = element_line( size=.1, color="#dedede") #slight grey horizontal lines
    )
}

plotti <- function(data, Dataframe, xvar, yvar, fillvar, Mean, title){
  Dataframe$n= sum$n
  Dataframe$ic = sum$ic
  ggplot()+ 
    geom_flat_violin(data= data, aes(x= .data[[xvar]], y= .data[[yvar]], fill=.data[[fillvar]]),position = position_nudge(x =.2, y = 0), alpha=.5, adjust = 1.5, colour = NA)+
    geom_boxplot(data= data, aes(x= .data[[xvar]], y= .data[[yvar]], fill=.data[[fillvar]]), outlier.shape=NA, alpha= .45, width = .1, colour = "black")+
    geom_point(data= Dataframe, aes(x =.data[[xvar]], y = .data[[Mean]], fill=.data[[fillvar]]), position= position_dodge(0.1), size=4)+
    geom_errorbar( data= Dataframe, aes(x=.data[[xvar]], ymin=.data[[Mean]]-SE, ymax=.data[[Mean]]+SE, fill=.data[[fillvar]]),position= position_dodge(0.3), width=0.1, colour="black", alpha=0.9) + #SD error bar
    geom_errorbar(data= Dataframe, aes(x=.data[[xvar]], ymin=.data[[Mean]]-ic, ymax=.data[[Mean]]+ic, fill=.data[[fillvar]]), position= position_dodge(0.4), width=0.1, colour="red", alpha=0.9)+ #C.I.
    scale_fill_manual(values = c("blue", 'red'))+ #labels names with amount
    ggtitle(title)+
    theme(
      legend.text=element_text(size=13), # text legend bigger
      panel.border = element_blank(), # no border panel (APA)
      panel.background = element_blank(), #white simple background
      axis.line = element_line(colour = "black"), # axis lines black
      panel.grid.major.y = element_line( size=.1, color="#dedede") #slight grey horizontal lines
    )
}

plotty <- function(data, Dataframe, xvar, yvar, fillvar, Mean, title){
  Dataframe$n= sum$n
  Dataframe$ic = sum$ic
  ggplot()+ 
    geom_flat_violin(data= data, aes(x= .data[[xvar]], y= .data[[yvar]], fill=.data[[fillvar]]),position = position_nudge(x =.2, y = 0), alpha=.5, adjust = 1.5, colour = NA)+
    geom_boxplot(data= data, aes(x= .data[[xvar]], y= .data[[yvar]], fill=.data[[fillvar]]), outlier.shape=NA, alpha= .45, width = .1, colour = "black")+
    geom_point(data= Dataframe, aes(x =.data[[xvar]], y = .data[[Mean]], fill=.data[[fillvar]]), position= position_dodge(0.1), size=4)+
    geom_errorbar( data= Dataframe, aes(x=.data[[xvar]], ymin=.data[[Mean]]-SE, ymax=.data[[Mean]]+SE, fill=.data[[fillvar]]),position= position_dodge(0.3), width=0.1, colour="black", alpha=0.9) + #SD error bar
    geom_errorbar(data= Dataframe, aes(x=.data[[xvar]], ymin=.data[[Mean]]-ic, ymax=.data[[Mean]]+ic, fill=.data[[fillvar]]), position= position_dodge(0.4), width=0.1, colour="red", alpha=0.9)+ #C.I.
    scale_fill_manual(values = c("blue", 'red'), #colours used in plot
                      name='', #legend gets no name
                      labels=c(
                        paste0('HCL \n n=', as.character(length(unique(data$ID[data$Condition=='HCL'])))), 
                        paste0('LCL \n n=', as.character(length(unique(data$ID[data$Condition=='LCL'])))) 
                      ))+ #labels names with amount
    ggtitle(title)+
    theme(
      legend.text=element_text(size=13), # text legend bigger
      panel.border = element_blank(), # no border panel (APA)
      panel.background = element_blank(), #white simple background
      axis.line = element_line(colour = "black"), # axis lines black
      panel.grid.major.y = element_line( size=.1, color="#dedede") #slight grey horizontal lines
    )
}

### summary
sum_3 <- function (data, var1, var2, var3, depvar){
  data %>%
    group_by(.data[[var1]], .data[[var2]], .data[[var3]]) %>%
    summarise(
      n = n(), 
      mean = mean(.data[[depvar]]),
      sd = sd(.data[[depvar]], na.rm=TRUE)
    ) %>%
    mutate( se = sd/sqrt(n)) %>%
    mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
}

sum_2 <- function (data, var1, var2, depvar){
  data %>%
    group_by(.data[[var1]], .data[[var2]]) %>%
    summarise(
      n = n(), 
      mean = mean(.data[[depvar]]),
      sd = sd(.data[[depvar]])
    ) %>%
    mutate( se = sd/sqrt(n)) %>%
    mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
}

sum_1 <- function (data, var1, depvar){
  data %>%
    group_by(.data[[var1]]) %>%
    summarise(
      n = n(), 
      mean = mean(.data[[depvar]]),
      sd = sd(.data[[depvar]])
    ) %>%
    mutate( se = sd/sqrt(n)) %>%
    mutate( ic=se * qt((1-0.05)/2 + .5, n-1))
}

# distribution plot (e.g. for RT)
distribution_plot <- function (data, var_name){
  ggplot(data, aes(x = variable, y = value, fill=variable))+
    geom_flat_violin(position = position_nudge(x = .2, y = 0),adjust =2)+
    coord_flip()+
    theme_cowplot()+
    geom_point(position=position_jitter(width=.15), size=2)+
    geom_boxplot(width= .10, outlier.shape = NA)+
    theme(legend.position=var_name)
}


### Correlation plot ####

corrplot <- function (xvar, yvar, xlab, ylab){
  dataframe <- data.frame(xvar, yvar)
  ggscatter(dataframe, x = 'xvar', y= 'yvar',
            add='reg.line', fullrange=TRUE,
            conf.int=TRUE,
            cor.coef=TRUE, cor.method='pearson')+
    xlab(toString(xlab))+
    ylab(toString(ylab))
  }
