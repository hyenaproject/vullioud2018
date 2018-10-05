# Vullioud_2018

This is the GitHub repository that hosts the R package ```vullioud2018```.


## What is this R package about?

This package aims at documenting how we obtained the
results for the paper entitled "Social support drives female dominance in the spotted hyaena" by Colin Vullioud,
Eve Davidian, Bettina Wachter, François Rousset, Alexandre Courtiol & Oliver P. Höner (accepted).

The only planned updates are updates that will be necessary to maintain the compatibility with other packages, so that our code won't break.


## How to explore the sources of the package?

To see our source code, you simply browse the files above within GitHub.


## How to install the package?

First make sure that the R package ```drat``` is installed on your system.
If that is not the case, install the R package by simply typing (inside your R Console):

```{r}
install.packages("drat")
```

Once the R package ```drat``` is installed, you can simply install our packages by typing (inside your R Console):

```{r}
drat::addRepo("hyenaproject")
install.packages("vullioud2018")
```


## How to use the package?

Once the package is installed, you can simply use it by typing (inside your R Console):

```{r}
library("vullioud2018")
```

To get the instructions on how to continue after that type (inside your R Console):
```{r}
?vullioud2018
```

If you have any problem, feel free to contact us: hyenaproject@gmail.com
