# Results, main figures
## Figure 1, birthweight
BW_figure1 <- ggarrange(plot_BW_time, plot_BW_SEP, plot_BW_nationality, plot_BW_language_region,
                         labels = c("A", "B", "C", "D"),
                         ncol = 2, nrow = 2) 
BW_figure1
ggsave("BW_figure1.pdf", BW_figure1, path = here("output/figures_paper"))
ggsave("BW_figure1.jpg", BW_figure1, path = here("output/figures_paper"))

## Figure 2, Preternbirth
PTB_figure2 <- ggarrange(plot_PTB, ggplot_PTB_SEP, ggplot_PTB_nationality, ggplot_PTB_Language_region,
                    labels = c("A", "B", "C", "D"),
                    ncol = 2, nrow = 2)
PTB_figure2
ggsave("PTB_figure2.pdf", PTB_figure2, path = here("output/figures_paper"))
ggsave("PTB_figure2.pdf", PTB_figure2, path = here("output/figures_paper"))



## Figure 3, stillbirth (no stratification)
ggsave("SB_figure3.pdf", plot_SB_time, path = here("output/figures_paper"))
ggsave("SB_figure3.jpg", plot_SB_time, path = here("output/figures_paper"))

# Appendix
  ## S1 : smoothed variables, BW model
BW_supplfig_1 <- ggarrange(plot_BW_GA, plot_BW_mat_age, plot_BW_month, plot_BW_ssep, plot_BW_altitude,
                          labels = c("A", "B", "C", "D", "E"),
                          ncol = 3, nrow = 2) 
BW_supplfig_1
ggsave("BW_supplfig_1.pdf", BW_supplfig_1, path = here("output/figures_paper"))
ggsave("BW_supplfig_1.jpg", BW_supplfig_1, path = here("output/figures_paper"))

  ## S2 : smoothed variables, PTB model
PTB_supplfig_2 <- ggarrange(plot_PTB_BW, plot_PTB_mat_age, plot_PTB_seasonality, plot_PTB_SSEP, plot_PTB_altitude,
                          labels = c("A", "B", "C", "D", "E"),
                          ncol = 3, nrow = 2) 
PTB_supplfig_2
ggsave("PTB_supplfig_2.pdf", PTB_supplfig_2, path = here("output/figures_paper"))
ggsave("PTB_supplfig_2.jpg", PTB_supplfig_2, path = here("output/figures_paper"))

  ## S3 : smoothed variables, stillbirth model
SB_supplfig_3 <- ggarrange(plot_SB_GA,  plot_SB_mat_age,  plot_SB_month,  plot_SB_SSEP, plot_SB_altitude,
                           labels = c("A", "B", "C", "D", "E"),
                           ncol = 3, nrow = 2) 
SB_supplfig_3
ggsave("SB_supplfig_3.pdf", SB_supplfig_3, path = here("output/figures_paper"))
ggsave("SB_supplfig_3.jpg", SB_supplfig_3, path = here("output/figures_paper"))







# trying out plots
(plot_PTB | ggplot_PTB_SEP) + (ggplot_PTB_nationality |ggplot_PTB_Language_region)  
test <- (plot_PTB | ggplot_PTB_SEP + ggplot_PTB_nationality +ggplot_PTB_Language_region  )+ plot_layout(nrow = 2, byrow = FALSE)
ggsave("test.pdf", test, path = here("output/stratification/graphs"))
test <- (plot_PTB | ggplot_PTB_SEP + ggplot_PTB_nationality +ggplot_PTB_Language_region  )+ plot_layout(nrow = 2, byrow = FALSE)
test
test <- (plot_PTB + ggplot_PTB_SEP) | (ggplot_PTB_nationality + ggplot_PTB_Language_region  )+ plot_layout(nrow = 2, byrow = FALSE)

(plot_PTB | ggplot_PTB_SEP) + (ggplot_PTB_nationality | ggplot_PTB_Language_region  ) + plot_layout(nrow = 2, byrow = FALSE)

# 2*2 as square
plot_PTB + ggplot_PTB_SEP + ggplot_PTB_nationality+ ggplot_PTB_Language_region
(plot_PTB / (ggplot_PTB_SEP + ggplot_PTB_nationality+ ggplot_PTB_Language_region)+ plot_layout(nrow = 2, byrow = FALSE))
