# Author: Crista Moreno
# Description: Plot Functions 

plot_logistic_model_3 <- function(m, d, x, y, p, x_breaks, y_label, title) {
  x <- enquo(x)
  y <- enquo(y)
  y_label <- enquo(y_label)
  
  m$finalModel$coefficients[1]
  
  x_0_5 = (log(p/(1-p)) - m$finalModel$coefficients[1])/ m$finalModel$coefficients[2]
  
  ggplot(d, aes(x = !!x, y = !!y)) +
    
    stat_smooth(method="glm", method.args=list(family="binomial"),
                se=FALSE, color = "black", cex = 1.6) +
    
    # , guide = TRUE
    scale_fill_manual(values=c("darkorchid", "deeppink", "#e08795", "blue")) + 
    scale_fill_hue(labels = c("class 1", "class 2")) + 
    scale_colour_manual(values=c("white", "white", "white", "white"), guide = FALSE) +
    
    geom_point(shape = 24,  size = 6, stroke = 0.6, alpha = 0.7,
               aes(fill = !!y_label, colour = !!y_label)) +
    
    scale_x_continuous(breaks = pretty(x_breaks, n = 10)) +
    
    theme(axis.title = element_text(size=18),
          axis.text.y = element_text(size=16), 
          axis.text.x = element_text(size=16), 
          legend.text = element_text(size = 16), 
          legend.title = element_text(size = 16), 
          plot.background = element_rect(fill = "transparent", colour = NA), 
          panel.border = element_blank(),
          legend.key = element_blank(), 
          plot.title = element_text(color = "black", size = 20, face = NULL)) +
    
    
    geom_vline(xintercept = x_0_5, linetype="dashed",
               color = "red", cex = 1) + 
    labs(x = "Variable 1", y = "Probability", fill = "Classes")
}


plot_logistic_model_2 <- function(m, d, x, y, p, x_breaks, y_label, title) {
  x <- enquo(x)
  y <- enquo(y)
  y_label <- enquo(y_label)
  
  m$finalModel$coefficients[1]
  
  x_0_5 = (log(p/(1-p)) - m$finalModel$coefficients[1])/ m$finalModel$coefficients[2]
  
  ggplot(d, aes(x = !!x, y = !!y)) +
    
    stat_smooth(method="glm", method.args=list(family="binomial"),
                se=FALSE, color = "black", cex = 1.6) +
    
    # , guide = TRUE
    scale_fill_manual(values=c("darkorchid", "#e08795", "#e08795", "blue")) + 
    scale_colour_manual(values=c("white", "white", "white", "white"), guide = FALSE) +
    
    geom_point(shape = 24,  size = 6, stroke = 0.6, alpha = 0.7,
               aes(fill = !!y_label, colour = !!y_label)) +
    
    scale_x_continuous(breaks = pretty(x_breaks, n = 10)) +
    
    theme(axis.title = element_text(size=18),
          axis.text.y = element_text(size=16), 
          axis.text.x = element_text(size=16), 
          legend.text = element_text(size = 16), 
          legend.title = element_text(size = 16), 
          plot.background = element_rect(fill = "transparent", colour = NA), 
          panel.border = element_blank(),
          legend.key = element_blank(), 
          plot.title = element_text(color = "black", size = 20, face = NULL)) +

    
    geom_vline(xintercept = x_0_5, linetype="dashed",
               color = "red", cex = 1) + 
    labs(y = "Probability") 
}


plot_logistic_model <- function(m, d, x, y, p, x_breaks, y_label, title) {
  x <- enquo(x)
  y <- enquo(y)
  y_label <- enquo(y_label)
  
  x_0_5 = (log(p/(1-p)) - coef(m)[1])/coef(m)[2]
  
  ggplot(d, aes(x = !!x, y = !!y)) +
    
    stat_smooth(method="glm", method.args=list(family="binomial"),
                se=FALSE, color = "black", cex = 1.6) +
    
    scale_fill_manual(values=c("hotpink1", "turquoise2", "purple", "green")) + 
    scale_colour_manual(values=c("hotpink3", "turquoise4", "purple", "green")) +
    
    geom_point(shape = 24,  size = 5, stroke = 2, alpha = 0.6,
               aes(fill = !!y_label, colour = !!y_label)) +
    
    scale_y_continuous(breaks=c(0, 1),
                       labels=c("0" = "Normal", "1" = "Keratoconus")) +
    # scale_x_continuous(breaks = pretty(x_breaks, n = 8)) +
    
    theme(axis.title = element_text(size=20),
          axis.text.y = element_text(size=18), 
          axis.text.x = element_text(size=18), 
          legend.text = element_text(size = 18), 
          legend.title = element_text(size = 20), 
          plot.background = element_rect(fill = "transparent", colour = NA), 
          panel.border = element_blank(),
          legend.key = element_blank(), 
          plot.title = element_text(color = "black", size = 26, face = NULL)) +
    
    ggtitle(title) + 
    
    geom_vline(xintercept=x_0_5, linetype="dashed",
               color = "olivedrab3", cex = 1.5) + 
    labs(y = NULL) 
}