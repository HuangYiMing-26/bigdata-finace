# 用于初始整个r语言的设置
# created @ 2021.4.9 By HYM

par(family='STKaiti') # 显示中文

# 设定ggplot2的主题
mytheme <- theme(plot.title = element_text(hjust = 0.5),
                 panel.grid.major.y = element_line(color = "grey",linetype = 1),
                 panel.grid.minor.y = element_line(color = "grey",linetype = 2),
                 panel.grid.minor.x = element_blank(),
                 text = element_text(family = "STKaiti")
                 )

#panel.background = element_rect(fill = "white",color = "black")
