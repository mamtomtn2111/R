library(scatterplot3d)
library(gplot2)

with(mtcars,{
  scatterplot3d(
    x = disp,
    y = wt,
    z = mpg,
    main = "3D-main",
    color = "blue",
    pch = 19,
    type = "h",
    xlab = "displacement",
    ylab= "weight",
    zlab = "Miles/Galon"
  )
})

with(mtcars,{
   s3d <- scatterplot3d(
     x = disp,
     y = wt,
     z = mpg,
     main = "3D Scatterplot Example 3",
     color = "blue",
     pch = 19,
     type = "h",
     xlab = "displacement (cu. in.)",
     ylab= "weight (1b/1000)",
     zlab = "Miles/ (US)Galon"
   )
   # Convert 3-D coords to 2D projection
   s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
   text(s3d.coords$x,
        s3d.coords$y,
        labels = row.names(mtcars),
        cex = .5,
        pos = 3)
})

mylist1 = c(c(1,2,3),c(4,5,6,1))
print(mylist1[7])

mtcars$pcolor[mtcars$cyl == 4] <- "red"
mtcars$pcolor[mtcars$cyl == 6] <- "blue"
mtcars$pcolor[mtcars$cyl == 8] <- "darkgreen"
with(mtcars,{
  s3d <- scatterplot3d(
    x = disp,
    y = wt,
    z = mpg,
    color = pcolor,
    pch = 19,
    type = "h",
    lty.hplot = 2,
    scale.y = .75,
    main = "3-D Scatterplot Example 4",
    xlab = "Dicplacement (cu. in.)",
    ylab = "weight (1b/1000)",
    zlab = "Miles/(US) gallon")
  s3d.coords <- s3d$xyz.convert(disp, wt, mpg)
  text(s3d.coords$x,
       s3d.coords$y,
       labels = row.names(mtcars),
       cex = .5,
       pos = 3)
  
  legend(#location
    "topright",
    inset = .05,
    bty= "n",
    cex = .5,
    title = "number of Cyclinders",
    c("4","6","8"),
    fill = c("red","blue","darkgreen"))
})
