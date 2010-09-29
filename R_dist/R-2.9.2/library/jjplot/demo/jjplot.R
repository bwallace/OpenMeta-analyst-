### Simple bar plot example with a color scale ###
df <- data.frame(x = 1:50,
                 y = rnorm(50))

jjplot(x, y, data = df,
       fill = y,
       jjplot.bar(col = "black"))

### Line plot of the same data, annotated with mean ## #
jjplot(x, y, data = df,
       jjplot.line(),
       jjplot.point(),
       jjplot.fun.y(mean),
       jjplot.hline(lty = "dashed", col = "red"))

### Box plot example ###
df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 state.x77)

jjplot(region, Income, data = df,
       fill = region,
       jjplot.group(jjplot.quantile(),
                    by = region),
       jjplot.box())

### Same data, faceted by sub-scatterplots instead of boxplots ###
jjplot(Income, Murder, data = df,
       color = region,
       facet.y = region,
       jjplot.identity(),
       jjplot.point(),
       jjplot.fit(),
       jjplot.abline())


### Bar plot example using the table statistic ###
df <- data.frame(x = sample(factor(LETTERS[1:10]), 100, replace=TRUE))
jjplot(x, data = df,
       ylab = "count",
       jjplot.table(),
       jjplot.bar(width = 0.5))

### Heatmap-style scatter plot ###
require("reshape")
df <- data.frame(state = rownames(state.x77),
                 region = state.region,
                 t((t(state.x77) - colMeans(state.x77)) /
                   apply(state.x77, 2, sd)))

melted <- melt(df, id.vars = c("state", "region"))

jjplot(variable, state, data = melted,
       alpha = 1.0, fill = value,
       ylab = "", xlab = "",
       jjplot.point(pch = 22, size = 2))


### Example of the jitter statistic to make a pseudo box-plot ###
df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- c(-6, -2, 2, 4) * df$x + rnorm(10000)

jjplot(x, f, data = df,
       alpha = 0.10, color = f,
       jjplot.jitter(yfactor = 1, xfactor=1),
       jjplot.point())

### Using grouping and statistics to create best-fit lines for each factor ###
jjplot(x + 2, y, data = df,
       alpha = 0.10, color = f,
       jjplot.point(),
       jjplot.group(jjplot.fit(), by = f),
       jjplot.abline(),
       jjplot.fun.y(mean),
       jjplot.hline(lty = "dashed"))

### CCDF of a heavy tailed distribution.
df <- data.frame(x=rlnorm(1000,2,2.5))
jjplot(x, data = df,
       jjplot.ccdf(density=TRUE),
       jjplot.point(),
       log='xy')

### Histogram of the same distribution.
jjplot(x, data = df,
       jjplot.hist(align="left", density=FALSE),
       jjplot.bar(),
       log = 'x')

