### Performance comparison between ggplot2 and jjplot ###
### Note that these depend heavily on the plotting device! ### 

df <- data.frame(x = rnorm(10000) + (1:4) * 1,
                 f = factor(c('A', 'B', 'C', 'D')))
df$y <- 1:4 * df$x + rnorm(10000)

system.time(jjplot(x, f, data = df,
                   alpha = 0.10, color = f,
                   jjplot.jitter(yfactor = 1, xfactor=1),
                   jjplot.point()))

system.time(jjplot(x + 2, y, data = df,
                   alpha = 0.10, color = f,
                   jjplot.point(),
                   jjplot.facet(jjplot.fit(), facet = f),
                   jjplot.abline(),
                   jjplot.fun.y(mean),
                   jjplot.hline(lty = "dashed")))

system.time(print(qplot(x, f, data = df,
                        alpha = I(0.1), colour =f,
                        geom = "jitter")))

system.time(print(qplot(x + 2, y, data = df,
                        alpha = I(0.1), colour = f) +
                  geom_smooth(method = "lm") +
                  geom_hline(aes(yintercept = mean(y)))))      
