# Results, main figures
## Figure 1.1, birthweight outcome model, main model and stratified by SEP/mat nationality/lang. region
# UNADJUSTED FOR GESTATIONAL AGE
BW_figure1_unadj_GA <- ggarrange(plot_BW_univ_unadj_GA,
                                 plot_BW_sex,
                                 plot_BW_SEP,
                                 plot_BW_language_region,
                                 plot_BW_nationality,
                                 labels = c("A", "B", "C", "D", "E"),
                                 ncol = 2, nrow = 3)
BW_figure1_unadj_GA


#DIRECTLY SAVE PDF 8*12 if 2*2 (2 col 2 rows)
# Or save 17*8 (3 col 2 rows)
# Or 12*12 (2 rows 3 columns)
# ggsave("BW_figure1_unadj_GA.pdf", BW_figure1_unadj_GA, 
#        path = here("output/figures_paper"))
# ggsave("BW_figure1_unadj_GA.jpg", BW_figure1_unadj_GA, path = here("output/figures_paper"))



## Figure 2, Preterm birth outcome model, main model and stratified by SEP/mat nationality/lang. region
## UNADJUSTED FOR BW
PTB_figure2_unadj_GA_sex <- ggarrange(plot_PTB_univ_unadj_BW,
                                 plot_PTB_sex,
                                 ggplot_PTB_SEP2,
                                 ggplot_PTB_Language_region2,
                                 ggplot_PTB_nationality2,
                                 labels = c("A", "B", "C", "D", "E"),
                                 ncol = 2, nrow = 3)
PTB_figure2_unadj_GA_sex
#DIRECTLY SAVE PDF 8*12
# ggsave("PTB_figure2_unadj_for_BW.pdf", PTB_figure2_2, path = here("output/figures_paper"))
# ggsave("PTB_figure2_unadj_for_BW.jpg", PTB_figure2_2, path = here("output/figures_paper"))



## Figure 3, stillbirth model (no stratification)
plot_SB_univ
#DIRECTLY SAVE PDF 8*12
# ggsave("SB_figure3.pdf", plot_SB_univ, path = here("output/figures_paper"))

# Appendix
  ## S1 : smoothed variables, BW main model


## S1.1 : smoothed variables, BW main model WITHOUT ADJUSTING for GA 
BW_supplfig_1_unadj_GA <- ggarrange(plot_BW_main_mat_age, plot_BW_main_month,
                                    labels = c("A", "B"),
                                    ncol = 2, nrow = 1) 
BW_supplfig_1_unadj_GA
# directly save 9*4.5
# ggsave("BW_supplfig_1_unadj_GA.pdf", BW_supplfig_1_unadj_GA, path = here("output/figures_paper"))
# ggsave("BW_supplfig_1_unadj_GA.jpg", BW_supplfig_1_unadj_GA, path = here("output/figures_paper"))

## S2 : smoothed variables, PTB main model WITHOUT ADJUSTING for BW
PTB_supplfig_2_2 <- ggarrange(plot_PTB_main_mat_age, plot_PTB_main_seasonality,
                              labels = c("A", "B"),
                              ncol = 2, nrow = 1) 
PTB_supplfig_2_2
# directly save 9*4.5
# ggsave("PTB_supplfig_2_unadj_for_BW.pdf", PTB_supplfig_2_2, path = here("output/figures_paper"))
# ggsave("PTB_supplfig_2_unadj_for_BW.jpg", PTB_supplfig_2_2, path = here("output/figures_paper"))


## S3 : smoothed variables, stillbirth main model
# with mat nationality category 1 and
# WITHOUT ADJUSTING for GESTATIONAL AGE
SB_supplfig_3_simple_unadj_GA <- ggarrange(plot_SB_main_mat_age, 
                                           plot_SB_main_month,
                                           labels = c("A", "B"),
                                           ncol = 2, nrow = 1) 
SB_supplfig_3_simple_unadj_GA
# directly save 9*4.5
# ggsave("SB_supplfig_3_simple_unadj_GA.pdf", SB_supplfig_3_simple_unadj_GA, path = here("output/figures_paper"))
# ggsave("SB_supplfig_3_simple_unadj_GA.jpg", SB_supplfig_3_simple_unadj_GA, path = here("output/figures_paper"))


## Models with flu instead of Great Recession
BW_supplfig_4_flu <- ggarrange(plot_BW_mat_age_flu, plot_BW_month_flu,
                                    labels = c("A", "B"),
                                    ncol = 2, nrow = 1) 
BW_supplfig_4_flu

PTB_supplfig_5_flu <- ggarrange(plot_PTB_flu_mat_age, plot_PTB_flu_seasonality,
                              labels = c("A", "B"),
                              ncol = 2, nrow = 1) 
PTB_supplfig_5_flu

SB_supplfig_6flu <- ggarrange(plot_SB_flu_mat_age, 
                              plot_SB_flu_month,
                              labels = c("A", "B"),
                              ncol = 2, nrow = 1) 
SB_supplfig_6flu


## Sensitivity analyses: first parities only
BW_supplfig_7_prim <- ggarrange(plot_BW_prim_mat_age, plot_BW_prim_month,
                               labels = c("A", "B"),
                               ncol = 2, nrow = 1) 
BW_supplfig_7_prim

PTB_supplfig_8_prim <- ggarrange(plot_PTB_prim_mat_age, plot_PTB_prim_seasonality,
                                labels = c("A", "B"),
                                ncol = 2, nrow = 1) 
PTB_supplfig_8_prim