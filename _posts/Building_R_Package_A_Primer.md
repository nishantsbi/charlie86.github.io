---
layout: post
title: Building R package from scratch:A beginner’s tutorial
subtitle: Very basics of how to be a R package creator
---

The purpose of this blog is to demonstrate the basics of creating an R package for the purposes of having reusable functions and objects with easy access. In this blog, I will take you through the following few basics steps of creating a package.

# Building_R_Package_A_Primer
`r Sys.Date()`  

## Developing R package

As a Data Science engineer, one of the most fundamental contributions you can make is to create and distribute software that implements the methods you develop.

The purpose of this document is to demonstrate the basics of creating an R package for the purposes of having reusable functions and objects with easy access. In this document, I will take you through the following few basics steps of creating a package.

## Why develop an R package

'Cause you know, you do what your advisor says and stuff.

But there are some real reasons to write software as a data scientist that I think are critically important:

1. You probably got into Analytics to have an impact. One of the most direct and quantifiable ways to have an impact on the world is to write software that other scientists, educators, and statisticians use. If you write a research paper with no software the chance of impacting the world is dramatically reduced.

2. Software is the new publication. I couldn't name one paper written by a graduate student (other than mine) in the last 2-3 years. But I could tell you about tons of software packages written by students/postdocs that I use. It is the number one way to get your name out there in the Analytics community.

3. If you write a software package you need for yourself, you will
save yourself tons of time in sourcing scripts, and remembering where all your code/functions are.Most importantly might be that creating an R package is building something. It is something you can point to and say, "I made that". Leaving aside all the tangible benefits to your career, the profession, etc. it is maybe the most gratifying feeling you get when working on research.

### When to start writing an R package

As soon as you have 2 functions.

Why 2? After you have more than one function it starts to get easy to lose track of what your functions do, it starts to be tempting to name your functions `foo` or `tempfunction` or some other such nonsense. You are also tempted to put all of the functions in one file and just source it. That was what I did whwn i started my first analytics project, which ended up being an epically comical set of about +2,000 lines of code in one R file. Ask my project lead about it sometime, he probably is still laughing about it.

### Naming your package

The first step in creating your R package is to give it a name. Hadley has some ideas about it. Here are our rules:

+ Make it googleable - check by googling it. 
+ Make sure there is no Bioconductor/CRAN package with the same name.No underscores, dashes or any other special characters/numbers   
+ Make it all lower case - people hate having to figure out caps in names of packages.   
+ Make it memorable; if you want serious people to use it don't be too cute.  
+ Make it as short as you possibly can while staying googleable.

### Versioning your package

The format of the version number will always be `x.y.z` . When you start any new package the version number should be `0.1.0` . Every time you make any change public ???e.g., push to GitHub??? you should increase z in the version number. If you are making local commits but not making them public to other people you don't need to increase z . You should stay in version `0.1.z` basically up until you are ready to submit to Bioconductor ???or CRAN??? for release.


## Six Steps to creating an R package  

1. Start a new Project / Package in RStudio
2. Update the DESCRIPTION file
3. Save and document data
4. Write Vignettes
5. Write and document functions
6. Document and build your package!

Some steps Not covered here....One can refer the references given at the end

## Install these packages!  

Once you've installed R and RStudio, make sure you have the latest versions of the following packages:

Installed by running the following:  

install.packages("knitr")   
install.packages("rmarkdown")   
install.packages("devtools")   
install.packages("rmdformats")   
install.packages("roxygen2")    


## Start a new package / project  

Now you're ready to get started on your package. You'll start by opening a new project in RStudio - this project will essentially be your new package. To create your new project (aka package), do the following steps:  

1. Create a new project in R Studio (File - New Project - New Directory - R Package)
  + Give your project a name (I'll call mine `Nishanttestpackage`) and associate it with a directory on your computer. 
  
![1](/img/posts/packagecreation/1.png)

![2](/img/posts/packagecreation/2.png)

![3](/img/posts/packagecreation/3.png) 

  + Open the project.  
  
![4](/img/posts/packagecreation/4.png) 

2. RStudio created a new folder on your computer with the project name. Navigate to the new folder you created and add the following folders if not present(by right click---new folder):  

/data (This is where you will store all of your .RData files)  
/R (This folder only contains .R files. This is where you will store all of your documentation files, function files, and miscellaneous R code)   
/inst (This folder contains any miscellaneous files you want to include in your package (e.g.;pdfs, images))   
/man (This folder contains compiled documentation files generated by Roxygen (e.g.; when you run devtools::document()). You should never edit files here manually)) 

An R project is simply a directory with specific subfolders and a DESCRIPTION file. Here is how your package folder should look (my package is called `Nishanttestpackage`)

![5](/img/posts/packagecreation/5.png) 

## The DESCRIPTION file  

[Click here for a longer Guide to DESCRIPTION files](http://r-pkgs.had.co.nz/description.html) 

Next youâll update the DESCRIPTION file for your package. Every R package must have a DESCRIPTION file that contains basic information about your package (e.g.; title, author, description etc.). You should update these by hand. Here are some of the main arguments:

+ Package: The name of your package. Donât change this.   
+ Title: A short (one sentence) description of your package.     
+ Description: A longer (1 paragraph) description of your package and what it does.    
+ Imports: The names of any other packages (separated by commas) that your package requires to work. For example, if your package includes functions from the BayesFactor package, you should include this here. If your package is stored on GitHub or CRAN, R will automatically install these packages on the userâs computer when they install your package. There are additional fields you can add (like URL (to include websites))

Here is a simple DESCRIPTION FILE

![6](/img/posts/packagecreation/6.png) 

## Save and document data

Ok! Now itâs time to include data in your package. Youâre probably used to storing data as Excel, SPSS, or .txt files. However, in creating packages, weâll store all of our data as R objects (e.g.; dataframes, matrices, lists, vectors) .RData files. .RData files efficiently store lots of R objects with minimal space.

**Read in the a text file with data from Study 1 of a priming study**

iris_df<â read.csv(iris_df,header = T)

You can load as many data objects as you want.   

2. Save the data object x as an .RData file in the data/ folder of your package using `save(OBJECT, file = âdata/OBJECT.RDataâ)`

**Save the data object as an .RData file in the data folder**  

save(iris_df, file = "data/iris_df.RData")  

After you do this, you should see a new file called `iris_df.RData` in the data folder in your package. 

![7](/img/posts/packagecreation/7.png) 

Putting many objects into one .RData file: You donât need to restrict yourself to one object for each .RData file. You can store as many objects in an .RData file as youâd like for eg:

save(df1, df2, df3, df4, file = "data/data_all.RData")


## Documenting data 

Next we will create documentation files for each of the data objects (usually dataframes) youâve saved in one (or more) .RData files. Documenting files are used to generate help menus for objects â which will tell the user what the data objects mean and how to use them.

For example, to see the help menu for the ChickWeight dataset (a dataset about chicken weights stored in R.), run the following:  

`help(ChickWeight)`

You should create a documentation file for each important data object (usually dataframes) stored in your .RData files.

![8](/img/posts/packagecreation/8.png) 


Here are some notes on creating this documentation file:  

+ Youâll notice that the comments are with #âinstead of the standard #, this type of comments is specific for package documentation. If you want to include a ârealâ comment, do so with the standard # AFTER the #â   
+ Include the name of the object at the end of the file in quotation marks (without the #â commenting)  
+ The examples at the bottom should all be executable R code. However, you donât need to include examples if you donât want to.

2. Repeat for every important data object in your package that you want to fully document. That is, every important data object should have its own _doc.R file in your R folder.

### Document and build

Now youâre ready to put all the elements of your package together! Weâll do this in two steps. First, weâll run the `devtools::document()` function. This function will convert all of your documentation files into more technical formats that R understands better. Second, weâll uset the `devtools::build()` function to
compile all of your package code into a single file.

Create documentation files with document()  

1. Execute the document() function (from the devtools package) to create an `x.Rd` file from your `x_doc.R` in your /man folder.

 # If devtools is not installed, install it first with   
 # install.packages('devtools')   
 # Create an OBJECT.Rd file   
 
 devtools::document() # run this code and it creates `mysummary.Rd` file in man folder
 
![9](/img/posts/packagecreation/9.png) 

### Build your package with build()  

Build your pckage in to a single R package file (.tar.gz) file with the 

devtools::build() function:

![10](/img/posts/packagecreation/10.png) 

Once you run this code, you should see a new .tar.gz file in your project. This file contains your final package!

![11](/img/posts/packagecreation/11.png) 

## Executing/testing time:Installing the created package

Great! Youâve successfully created a new R package! However, to access it, you need to install the package. To do this, just use the install.packages() command and put the file location as the main argument. Youâll also include two extra arguments that tell R that you are not installing the package from CRAN.

install.packages(pkgs = "Nishanttestpackage_0.1.0.tar.gz",
repos = NULL, # Tells R not to try to get the package from CRAN
type = "source" # Type of package is source
)

Assuming that now you are running a diiferent project.Just save the package tar.gz file in the working directory and execute the above functioninstall.packages() as shown below in the image:

![12](/img/posts/packagecreation/12.png) 

Then load the package as usual:

`library(Nishanttestpackage)`

If you want to see the datasets in the package run the following:

`data(package="Nishanttestpackage")`

![13](/img/posts/packagecreation/13.png) 

To load the datasets in the package:

`data("iris_df")` #loading the dataset in R    
`head(iris_df)`

To see the function help page/description:

`?mysummary`

![14](/img/posts/packagecreation/14.png) 

Invoking the function:

`mysummary(iris_df$Sepal.Length)`  # using the datset from this package itself   
Median= 5.8    
MAD= 1.03782   
$center   
[1] 5.8    

$spread   
[1] 1.03782   

`mysummary(iris_df$Sepal.Length,npar=FALSE,print=FALSE)` # testing the other parameters of the function

$center   
[1] 5.843333   

$spread   
[1] 0.8280661    

## References

1. [R package development - the Leek group way!](https://github.com/jtleek/rpackages)

2. [R packages ebook by Hadley Wickham](http://r-pkgs.had.co.nz/)

3. [Writing an R package from scratch](https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/)
