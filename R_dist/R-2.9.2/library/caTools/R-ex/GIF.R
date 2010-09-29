### Name: read.gif & write.gif
### Title: Read and Write Images in GIF format
### Aliases: read.gif write.gif
### Keywords: file

### ** Examples

# visual comparison between image and plot
write.gif( volcano, "volcano.gif", col=terrain.colors, flip=TRUE, 
           scale="always", comment="Maunga Whau Volcano")
y = read.gif("volcano.gif", verbose=TRUE, flip=TRUE)
image(y$image, col=y$col, main=y$comment, asp=1)
# browseURL("file://volcano.gif")  # inspect GIF file on your hard disk

# test reading & writing
col = heat.colors(256) # choose colormap
trn = 222              # set transparent color
com = "Hello World"    # imbed comment in the file
write.gif( volcano, "volcano.gif", col=col, transparent=trn, comment=com)
y = read.gif("volcano.gif")
stopifnot(volcano==y$image, col==y$col, trn==y$transparent, com==y$comment)
# browseURL("file://volcano.gif") # inspect GIF file on your hard disk

# create simple animated GIF (using image function in a loop is very rough,
# but only way I know of displaying 'animation" in R)
x <- y <- seq(-4*pi, 4*pi, len=200)
r <- sqrt(outer(x^2, y^2, "+"))
image = array(0, c(200, 200, 10))
for(i in 1:10) image[,,i] = cos(r-(2*pi*i/10))/(r^.25)
write.gif(image, "wave.gif", col="rainbow")
y = read.gif("wave.gif")
for(i in 1:10) image(y$image[,,i], col=y$col, breaks=(0:256)-0.5, asp=1)
# browseURL("file://wave.gif") # inspect GIF file on your hard disk

# Another neat animation of Mandelbrot Set
jet.colors = colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F",
             "yellow", "#FF7F00", "red", "#7F0000")) # define "jet" palette
m = 400
C = complex( real=rep(seq(-1.8,0.6, length.out=m), each=m ), 
             imag=rep(seq(-1.2,1.2, length.out=m),      m ) )
C = matrix(C,m,m)
Z = 0
X = array(0, c(m,m,20))
for (k in 1:20) {
  Z = Z^2+C
  X[,,k] = exp(-abs(Z))
}
image(X[,,k], col=jet.colors(256))
write.gif(X, "Mandelbrot.gif", col=jet.colors, delay=100)
# browseURL("file://Mandelbrot.gif") # inspect GIF file on your hard disk
file.remove("wave.gif", "volcano.gif", "Mandelbrot.gif")

# Display interesting images from the web
## Not run: 
##D url = "http://www.ngdc.noaa.gov/seg/cdroms/ged_iib/datasets/b12/gifs/eccnv.gif"
##D y = read.gif(url, verbose=TRUE, flip=TRUE)
##D image(y$image, col=y$col, breaks=(0:length(y$col))-0.5, asp=1,
##D            main="January Potential Evapotranspiration mm/mo")
##D url = "http://www.ngdc.noaa.gov/seg/cdroms/ged_iib/datasets/b01/gifs/fvvcode.gif"
##D y = read.gif(url, flip=TRUE)
##D y$col[y$transparent+1] = NA # mark transparent color in R way
##D image(y$image, col=y$col[1:87], breaks=(0:87)-0.5, asp=1,
##D            main="Vegetation Types")
##D url = "http://talc.geo.umn.edu/people/grads/hasba002/erosion_vids/run2/r2_dems_5fps(8color).gif"
##D y = read.gif(url, verbose=TRUE, flip=TRUE)
##D for(i in 2:dim(y$image)[3]) 
##D   image(y$image[,,i], col=y$col, breaks=(0:length(y$col))-0.5,
##D             asp=1, main="Erosion in Drainage Basins")
## End(Not run)



