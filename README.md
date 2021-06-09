# aria
An R package implementing an idiosyncratic Stan workflow.

Install via `remotes::install_github('mike-lawrence/aria/aria')`

## Why "aria"?
Where [Stan](https://mc-stan.org/) is eponymous for [Stanislaw Ulam](https://en.wikipedia.org/wiki/Stanislaw_Ulam), this package was inspired by [Gelman, 2021](https://statmodeling.stat.columbia.edu/2021/02/09/maybe-we-shouldve-called-it-arianna/) to pay homage to [Arianna Rosenbluth](https://en.wikipedia.org/wiki/Arianna_W._Rosenbluth), with liberties taken to employ a shorter sobriquet that happens also to connote the fact that this is a [solo](https://en.wikipedia.org/wiki/Aria) project of mine.

## Why use aria?
You probably shouldn't, not yet at least. That is, this package is still in it's development infancy, focused first on feature implementation and only secondarily aiming for things like cross-platform reliability and ability to handle all possible workflow scenarios. I like to think of this package as a kind of workflow proof-of-concept, permitting the agile implementation of my ideas; once I have things I think are worthwhile to others, I'll work on advocating their inclusion in the more popular stability-focused packages.

### Implemented features
* Use of `stanc3` for syntax checking in RStudio (automatically enabled on package load; see `?aria::enable_rstudio_syntax_compile`)
* Option to trigger model compilation on save with a `//aria: compile=1` string at the top of your Stan file
* Smart compilation whereby the saved model is compared to a stored (in a folder called `aria`) representation and compilation only proceeds if *functional* changes to the code have been made (n.b. including to any includes!). So go ahead and add comments and modify whitespace without fear that they will cause unnecessary model recompilation.
* Both compilation and sampling occur in background processes with outputs/progress monitored by an RStudio Job.
* Automatic check for runtime errors at compilation using a special debugging exe and dummy data.
* Automatic check for runtime errors at the outset of sampling using a special debugging exe and the real data
* If no runtime errors are encountered, use of a performance-tuned exe for sampling 
* Data are cached for faster start of sampling when the same data are sampled repeatedly
* Nicer progress indicators including estimated time remaining and parsimonious display of any important error messages. 

### Features under development
* Progress indication including diagnostics
* During-sampling redirection of output to an [ArviZ](https://arviz-devs.github.io/arviz/)-compliant file format, enabling faster post-sampling access to the output data as well as during-sampling monitoring of diagnostics & posterior samples.
* Diagnostics-driven sampling, whereby the model performance is monitored on a variety of criteria (divergences encountered, rhats, ESS; also standard sample-count as well as wall-time) and terminates only when those criteria are met.
* Resuming sampling of unexpectedly-terminated chains.
* When compiling performance exe: Moving transformed parameters to model block and removing generated quantities entirely. This yields slightly less compute/write time at the cost of requiring a subsequent `aria::generate_quantities()` run.
* `aria::generate_quantities()`, which extracts the quantities that would have been computed/saved by the code as written but moved/removed from the performance exe, puts them all in the generated quantities, compiles and runs with just the post-warmup (and possibly thinned) samples. 

### Glaring omissions
* cross-platform support; `aria` currently *should* run on unix-like systems (Linux, MacOS, WSL) but certainly won't work on Windows yet. 
* handle within-chain parallelizing compile arguments; this *should* be easy, I just never use these, being on a meagre 4-core myself 
* tests; 😬

## How to use aria
When first opening a project, run:
```r
aria::enable_rstudio_syntax_compile()
```
This enables the enhanced "Check on Save" in RStudio. You can then work on your stan file in rstudio, saving as you go to obtain syntax checks from the stanc3 syntax checker. 

When you're ready to compile, add`//aria: compile=1` at the top of the file and save, at which point an RStudio Job will launch to show you progress of a multi-step compilation. By default, compilation will involve:

 1. Syntax check
 2. Compiling a "debugging" executable
 3. Running a "debug check" using this executable and automatically generated synthetic data
 4. Compiling a "performance" executable for use in sampling

At this point you might hear auditory feedback on successful completion or failure. To suppress these sounds, run:
```r
options('aria_sotto_vocce'=TRUE)
```
There are other comments you can put in your stan file to modify the syntax-checking and compilation process away from the defaults enumerated above:


* `//aria: syntax_ignore = c('has no priors.',...)` 

This line tells the syntax checker to ignore any Syntax Warning obtained from the check that includes any string included in the character vector expressed to the right of the `=`; in this example, warnings containing the string `'has no priors'` will be skipped along with any others included in the standin `...` string (don't literally put `...` though; that's there just to illustrate that you can specify multiple strings). This option only really makes sense to need when you're trying to compile (syntax errors block compilation) and have already verified that the warning is not taking into account your more advanced Stan code.


* `//aria: compile_debug=0` 

A value of `0` *prevents* the default compilation of a "debug" exe. Recommended that you not use this unless you're really pressed for time.


*  `//aria: run_debug=0` 

A value of `0` *prevents* the default running of the debug exe (if compiled) using auto-generated synthetic data. For use with models that have complicated structure that foils the synthetic data generator and yield nan's in the target. 


Now that your model is compiled, you can use `aria::compose()` to sample:
```r
# compose a posterior given data and model
#   Note data-first & functional-programming-oriented design
#   Note we pass the path to the Stan code; aria will go find the exe
aria::compose( 
	data = my_data
	, code_path = 'stan/my_mod.stan' 
	, out_path = 'sampled/my_data_my_mod_out.qs'
)
```
This will return NULL invisibly but launch sampling in the background with an RStudio Job to monitor the progress. When sampling is complete the output is saved in the file supplied as the value of the `out_path` argument to `aria::compose()`. At present aria uses the `qs` [package](https://github.com/traversc/qs) to save output and thus I tend to add a `.qs` suffix to the `out_path` file. (I'll soon be transitioning `aria` to `NETCDF4`-based storage, and when that's done I'll update these instructions with any consequences thereof.)

To read the output, use `aria::coda()`:
```r
post = aria::coda('sampled/my_data_my_mod_out.qs')
```

The object returned from `aria::coda()` is a tibble of the samples with columns denoting the chain, iteration, and a `warmup` column denoting those iterations from the warmup phase. Meta-data is stored in a number of attributes:
```r
attributes(post)
```

Finally, here's some code to run some diagnostics and summarize the posterior of each variable:
```r
library(tidyverse)

#toss warmup
post = filter(post,!warmup)

# Check treedepth, divergences, & rebfmi
(
	post
	%>% filter(!warmup)
	%>% group_by(chain)
	%>% summarise(
		max_treedepth = max(treedepth__)
		, num_divergent = sum(divergent__)
		, rebfmi = var(energy__)/(sum(diff(energy__)^2)/n())
	)
)

# gather summary for core parameters (inc. rhat & ess)
(
	post
	%>% select(-(warmup:energy__))
	%>% pivot_longer(
		cols = c(-chain,-iteration)
		, names_to = 'variable'
	)
	%>% group_by(variable)
	%>% summarise(
		rhat = posterior::rhat(matrix(value,ncol=length(unique(chain))))
		, ess_bulk = posterior::ess_bulk(matrix(value,ncol=length(unique(chain))))
		, ess_tail = posterior::ess_tail(matrix(value,ncol=length(unique(chain))))
		, as_tibble(t(posterior::quantile2(value,c(.1,.25,.5,.75,.9))))
		, .groups = 'drop'
	)
) ->
	fit_summary

# check the range of rhat & ess
(
	fit_summary
	%>% select(rhat,contains('ess'))
	%>% summary()
)

```
