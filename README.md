# aria
An R package implementing an idiosyncratic Stan workflow. 

Install via `remotes::install_github('mike-lawrence/aria/aria')`

## Why "aria"?
Where [Stan](https://mc-stan.org/) is eponymous for [Stanislaw Ulam](https://en.wikipedia.org/wiki/Stanislaw_Ulam), this package was inspired by [Gelman, 2021](https://statmodeling.stat.columbia.edu/2021/02/09/maybe-we-shouldve-called-it-arianna/) to pay homage to [Arianna Rosenbluth](https://en.wikipedia.org/wiki/Arianna_W._Rosenbluth), with liberties taken to employ a shorter sobriquet that happens also to connote the fact that this is a [solo](https://en.wikipedia.org/wiki/Aria) project of mine.

## Why use aria?
You probably you shouldn't, not yet at least. That is, this package is still in it's development infancy, focused first on feature implementation and only secondarily aiming for things like cross-platform reliability and ability to handle all possible workflow scenarios. I like to think of this package as a kind of workflow proof-of-concept, permitting the agile implementation of my ideas; once I have things I think are worthwhile to others, I'll work on advocating their inclusion in the more popular stability-focused packages.

### Implemented features
* Use of `stanc3` for syntax checking in RStudio (automatically enabled on package load; see `?aria::enable_rstudio_syntax_compile`)
* Option to trigger model compilation on save with a `\\compile:1` string at the top of your Stan file
* Smart compilation whereby the saved model is compared to a stored (in a folder called `aria`) representation and compilation only proceeds if *functional* changes to the code have been made. So go ahead and add comments and modify whitespace without fear that they will cause unnecessary model recompilation.
* Background sampling, whereby sampling does not block the main R process, enabling you to work on other things during sampling without creating a new session. If using RStudio and using defaults to `aria::monitor()`, the sampling progress is visible in the RStudio Jobs pane. 

### Features under development
* Diagnostics-driven sampling, whereby the model performance is monitored on a variety of criteria (divergences encountered, rhats, ESS; also standard sample-count as well as wall-time) and terminates only when those criteria are met.
* Nicer progress indicators, including the above noted performance metrics and estimated time remaining, all appearing using the RStudio Jobs interface.
* During-sampling redirection of output to an [ArviZ](https://arviz-devs.github.io/arviz/)-compliant file format, enabling faster post-sampling access to the output data as well as during-sampling monitoring of the posterior (i.e. in addition to the diagnostics above, the posterior for your parameters).
* Resuming sampling of unexpectedly-terminated chains.
* Data-Model stamped sampling, warning the user when they are re-sampling a data-and-model combination that already has posterior results anywhere in the working directory (achieved by quickly checking a header in any netCDF files found).  

## Examples
```r
library(aria) #loads aria & enables enhanced "Check on Save" in RStudio

# work on your stan file in rstudio; when passing syntax checks, put "\\compile:1" at the top (no quotes) and it will compile next time you save.

#sample using a compiled model & data
#   Note data-first & functional-programming-oriented design
#   Note we pass the path to the Stan code; aria will go find the exe
sample( data = my_data, code_path = 'stan/my_mod.stan' )

#sample returns NULL invisibly but launches sampling in the background with an RStudio Job to monitor the progress.

```
