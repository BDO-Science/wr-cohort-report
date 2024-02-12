


report_year = 2021
wytype = "Critical"
Sys.setenv(TZ='America/Los_Angeles')

wlk_start = as.Date("2012-11-15")
sxs_start = as.Date("2015-06-15")
start = as.Date(paste0(report_year-10, "-01-01"))
end = as.Date(paste0(report_year, "-12-31"))
end2 = as.Date(paste0(report_year+1, "-06-30"))

theme_plots <- theme_bw() + theme(axis.text = element_text(size = 12),
                                  strip.text = element_text(size = 12),
                                  legend.text = element_text(size = 12),
                                  axis.title = element_text(size = 13),
                                  axis.title.x = element_blank(),
                                  legend.title = element_blank(),
                                  legend.position = "top",
                                  axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))
