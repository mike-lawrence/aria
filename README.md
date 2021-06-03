# aria
An R package implementing an idiosyncratic Stan workflow.

Install via `remotes::install_github('mike-lawrence/aria/aria')`

## Why "aria"?
Where [Stan](https://mc-stan.org/) is eponymous for [Stanislaw Ulam](https://en.wikipedia.org/wiki/Stanislaw_Ulam), this package was inspired by [Gelman, 2021](https://statmodeling.stat.columbia.edu/2021/02/09/maybe-we-shouldve-called-it-arianna/) to pay homage to [Arianna Rosenbluth](https://en.wikipedia.org/wiki/Arianna_W._Rosenbluth), with liberties taken to employ a shorter sobriquet that happens also to connote the fact that this is a [solo](https://en.wikipedia.org/wiki/Aria) project of mine.

## Why use aria?
You probably shouldn't, not yet at least. That is, this package is still in it's development infancy, focused first on feature implementation and only secondarily aiming for things like cross-platform reliability and ability to handle all possible workflow scenarios. I like to think of this package as a kind of workflow proof-of-concept, permitting the agile implementation of my ideas; once I have things I think are worthwhile to others, I'll work on advocating their inclusion in the more popular stability-focused packages.

### Implemented features
* Use of `stanc3` for syntax checking in RStudio (automatically enabled on package load; see `?aria::enable_rstudio_syntax_compile`)
* Option to trigger model compilation on save with a `\\compile:1` string at the top of your Stan file
* Smart compilation whereby the saved model is compared to a stored (in a folder called `aria`) representation and compilation only proceeds if *functional* changes to the code have been made (n.b. including to any includes!). So go ahead and add comments and modify whitespace without fear that they will cause unnecessary model recompilation.
* Both compilation and sampling occur in background processes with outputs/progress monitored by an RStudio Job.
* Automatic check for runtime errors at compilation using a special debugging exe and dummy data.
* Automatic check for runtime errors at the outset of sampling using a special debugging exe and the real data
* If no runtime errors are encountered, use of a performance-tuned exe for sampling 
* Data are cached for faster start of sampling when the same data are sampled repeatedly


### Features under development
* Nicer progress indicators, including the diagnostics and estimated time remaining, all appearing using the RStudio Jobs interface.
* During-sampling redirection of output to an [ArviZ](https://arviz-devs.github.io/arviz/)-compliant file format, enabling faster post-sampling access to the output data as well as during-sampling monitoring of diagnostics & posterior samples.
* Diagnostics-driven sampling, whereby the model performance is monitored on a variety of criteria (divergences encountered, rhats, ESS; also standard sample-count as well as wall-time) and terminates only when those criteria are met.
* Resuming sampling of unexpectedly-terminated chains.

## Examples
```r
library(aria) #loads aria & enables enhanced "Check on Save" in RStudio

# work on your stan file in rstudio; when passing syntax checks, put "\\compile:1" at the top (no quotes) and it will compile next time you save.

# compose a posterior given data and model
#   Note data-first & functional-programming-oriented design
#   Note we pass the path to the Stan code; aria will go find the exe
aria::compose( 
	data = my_data
	, code_path = 'stan/my_mod.stan' 
	, out_path = 'sampled/my_data_my_mod_out.qs'
)

#sample returns NULL invisibly but launches sampling in the background with an RStudio Job to monitor the progress.

#when complete, retrieve the posterior via:
post = aria::coda('sampled/my_data_my_mod_out.qs')
```
