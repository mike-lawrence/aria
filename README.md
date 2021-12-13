# Note: 
Development for this project is on pause, partly due to personal time constraints, partly because I'm aiming to transition the core out of RStudio Jobs to a python-based background-process framework that should be more environment agnostic (ex. could be run in RStudio or VSCode)

# aria
An R package implementing an idiosyncratic Stan workflow.

Install via:
```r
remotes::install_github('mike-lawrence/aria/aria')
```
(Yup, that's an extra `/aria` relative to what you might be used to in terms of `install_github()` argument strings)

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
* Progress indication including diagnostics
* During-sampling redirection of output to an [ArviZ](https://arviz-devs.github.io/arviz/)-compliant file format, enabling faster post-sampling access to the output data as well as during-sampling monitoring of diagnostics & posterior samples.

### Features under development
* Diagnostics-driven sampling, whereby the model performance is monitored on a variety of criteria (divergences encountered, rhats, ESS; also standard sample-count as well as wall-time) and terminates only when those criteria are met.
* Resuming sampling of unexpectedly-terminated chains.
* When compiling performance exe: Moving transformed parameters to model block and removing generated quantities entirely. This yields slightly less compute/write time at the cost of requiring a subsequent `aria::generate_quantities()` run.
* `aria::generate_quantities()`, which extracts the quantities that would have been computed/saved by the code as written but moved/removed from the performance exe, puts them all in the generated quantities, compiles and runs with just the post-warmup (and possibly thinned) samples. 
* Automated SBC by extracting user-supplied parameters (and their down-stream dependencies; requires models be written in generative order) and placing them in GQ with `_rng` functions replacing their priors. 

### Glaring omissions
* cross-platform support; `aria` currently *should* run on unix-like systems (Linux, MacOS, WSL) but certainly won't work on Windows yet. 
* handle within-chain parallelizing compile arguments; this *should* be easy, I just never use these, being on a meagre 4-core myself 
* tests; 😬

## How to use aria
`aria` uses features that were introduced in cmstan 2.27.0, so if you haven't grabbed that yet, you need to run:
```r
cmdstanr::install_cmdstan(version='2.27.0')
```

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
#   Note:
#     - pipe-compatible data-first arguments
#     - we pass the path to the Stan code; aria will go find the exe
aria::compose( 
	data = my_data
	, code_path = 'stan/my_mod.stan' 
	, out_path = 'sampled/my_data_my_mod_out.nc'
)
```
This will return NULL invisibly but launch sampling in the background with an RStudio Job to monitor the progress, including during-sampling diagnostics. During sampling, the CSVs generated by cmdstan are parsed and output is stored in a NetCDF4 file (hence the `.nc` extension in the above example). There is also some information (timing, messages from cmdstan) stored in a file at `aria/marginalia.qs` that can be viewed via:
```R
aria::marginalia()
```
The output of sampling can be accessed via:
```R
post = aria::coda( out_path = 'sampled/my_data_my_mod_out.nc')
```
Which initializes an R6 object with pointers to the pertinent internals of the NetCDF4 file. Ultimately I'll be working toward making said internals compliant with the [InferenceData spec](https://arviz-devs.github.io/arviz/getting_started/XarrayforArviZ.html), but for now you can view what's there via:
```R
post$nc_info()
```
And get [rvar](https://mc-stan.org/posterior/articles/rvar.html) representations via:
```R
post$draws() #all variables
post$draws('mu') #just mu
post$draws(variables=c('mu','sigma')) #mu & sigma
post$draws(groups='parameters') #just the parameters (i.e. no TP or GQ)

#plays-well with posterior:
post$draws('mu') %>% posterior::summarise_draws()
```
Finally, here's some code to run some diagnostics and summarize the posterior of each variable:
```r
library(tidyverse)


# Check treedepth, divergences, & rebfmi
(
	post$draws(group='sample_stats')
	%>% posterior::as_draws_df()
	%>% group_by(.chain)
	%>% summarise(
		max_treedepth = max(treedepth)
		, num_divergent = sum(divergent)
		, rebfmi = var(energy)/(sum(diff(energy)^2)/n()) #n.b. reciprocal of typical EBFMI, so bigger=bad, like rhat
	)
)

# gather summary for core parameters (inc. rhat & ess)
(
	post$draws(groups='parameters')
	%>% posterior::summarise_draws()
) ->
	fit_summary

# check the range of rhat & ess
(
	fit_summary
	%>% select(rhat,contains('ess'))
	%>% summary()
)

```

And here's a nice diagnostics-and-quantiles viz that is unrelated to this package but I like and wanted to share (code below):
![image](https://user-images.githubusercontent.com/150781/121684614-4c103300-ca95-11eb-932d-3c74ae5b6502.png)

```r
(
	fit_summary
	%>% filter(str_starts(variable,fixed('z_m.')))
	%>% ggplot()
	+ geom_hline(yintercept = 0)
	+ geom_linerange(
		mapping = aes(
			x = variable
			, ymin = q2.5
			, ymax = q97.5
			, colour = ess_tail
		)
	)
	+ geom_linerange(
		mapping = aes(
			x = variable
			, ymin = q25
			, ymax = q75
			, colour = ess_bulk
		)
		, size = 3
	)
	+ geom_point(
		mapping = aes(
			x = variable
			, y = q50
			, fill = rhat
		)
		, shape = 21
		, size = 2
	)
	+ coord_flip()
	+ scale_color_gradient(
		high = 'white'
		, low = scales::muted('red')
	)
	+ scale_fill_gradient(
		low = 'white'
		, high = scales::muted('red')
	)
	+ labs(
		y = 'Mean'
		, colour = 'ESS'
		, fill = 'Rhat'
	)
	+ theme(
		panel.background = element_rect(fill='grey50')
		, panel.grid = element_blank()
	)
)
```
