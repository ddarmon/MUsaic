library(MUsaic)

load('ex02-34.rda')

df = `ex02-34`

df.clean = columns_to_factors(df)

gf_dhistogram(~ response, data = df.clean) %>% gf_facet_wrap(~ label, ncol = 1)

load('ta24-01.rda')

df = `ta24-01`

head(df)

df.clean = columns_to_factors(df)

head(df.clean)

gf_histogram(~ response | label, data = df.clean)
gf_boxplot(~ response | label, data = df.clean)
gf_boxplot(~ response, col = ~label, data = df.clean)
gf_dens(~ response, col = ~ label, data = df.clean) %>% gf_rugx(~ response, col = ~ label, data = df.clean)
ggsave('density_plot.pdf')

gf_violin(response ~ label, data = df.clean)
ggsave('violin_plot.pdf')
