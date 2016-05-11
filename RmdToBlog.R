library(knitr)
library(rgl)


knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(fig.path='../images/blogpost-')
# knit_hooks$set(webgl = hook_webgl)

blog_dir <- ""
blog_date <- "2016-05-11"
input <- "Generating Benchmark Data Sets for Subspace Outlier Detection.Rmd"

if(!file.exists("_posts")){
  dir.create("_posts")
}

outFile <- knit(input, 
     output = paste0("./_posts/", sub(".Rmd$", "", basename(input)), ".md")
)

file.rename(outFile, paste0(blog_dir, "_posts/", blog_date, "-", sub(".Rmd$", "", basename(input)), ".md"))
images <- list.files("../images/")
images
sapply(images, function(x) file.rename(paste0("../images/", x), paste0(blog_dir, "images/", x)))

