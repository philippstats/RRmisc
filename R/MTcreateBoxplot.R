#' MT: Create Boxplots with scaling or cutting of y axis
#' æ
#' @param data dataæ
#' @param metric mlr-metric
#' @param opp.bound lower or upper bound for y axis depending if metric should 
#' be minimzed or maximized. 
#' @param scale scales "log10" or "sqrt" are allowed.
#' @param main title
#' @import ggplot2
#' @import dplyr
#' @import checkmate 
#' @import mlr
#' @import plotly
#' @export

createBoxplotMT = function(data, metric = mmce, opp.bound = NULL, 
  scale = NULL, main = NULL, plotly = TRUE) {
  assertClass(data, "data.frame")
  
  to.min = metric$minimize
  metric.name = paste0(metric$id, ".test.mean")
  
  g = data %>% 
    ggplot() + geom_boxplot(aes_string(x = "uniq.name", y = metric.name)) +
    theme(axis.text.x = element_text(angle=90, vjust=0.5), 
    axis.title.x = element_blank()) + 
    geom_vline(xintercept = 1.5, color = "black", linetype = "longdash") + 
    geom_vline(xintercept = 2.5, color = "black", linetype = "longdash") + 
    geom_vline(xintercept = 5.5, color = "black", linetype = "longdash") + 
    geom_vline(xintercept = 14.5, color = "black", linetype = "longdash") 
  if (is.null(main)) {
    g = g + ggtitle(data$prob[1])
  } else {
    assertClass(main, "character")
    g = g + ggtitle(main)
  }
  if (!is.null(scale)) {
    assertChoice(scale, choices = c("log", "sqrt"))
    if (scale == "log") g = g + scale_y_log10()
    if (scale == "sqrt") g = g + scale_y_sqrt()
  }
  if (to.min) {
    data2 = data %>% group_by(idx) %>% arrange_(metric.name) %>% slice(1)
  } else {
    data2 = data %>% group_by(idx) %>% arrange_(metric.name) %>% slice(n())
  }
  #fct = paste0("median(", metric.name, ")")
  #best.bl.med = data %>% filter(algo == "BEST_BL") %>% summarise(median(metric.name))
  data3 = data %>% filter(algo == "BEST_BL") 
  best.bl.med = median(data3[, metric.name])
  
  g2 = g + geom_point(data = data2, aes_string("uniq.name", metric.name), alpha = .75, shape = 21, size = 4, fill = "yellow") 
  g2
  if (!is.null(opp.bound)) {
    if (to.min) {
      lower.bound = min(data2[metric.name])
      upper.bound = opp.bound
    } else {
      lower.bound = opp.bound
      upper.bound = max(data2[metric.name])
    }
    g2 = g2 + coord_cartesian(ylim = c(lower.bound, upper.bound)) 
  } 
  g3 = g2 + geom_hline(aes(yintercept=best.bl.med), col = "red")
  if (plotly) {
    ggplotly(g3)
  } else {
    g3
  }
} 
