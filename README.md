# aria
An R package implementing an idiosyncratic Stan workflow.

## Why "aria"?
Where Stan is eponymous for [Stanislaw Ulam](https://en.wikipedia.org/wiki/Stanislaw_Ulam), this package was inspired by [Gelman, 2021](https://statmodeling.stat.columbia.edu/2021/02/09/maybe-we-shouldve-called-it-arianna/) to pay homage to [Arianna Rosenbluth](https://en.wikipedia.org/wiki/Arianna_W._Rosenbluth), with liberties taken to employ a shorter sobriquet that happens also to connote the fact that this is a [solo](https://en.wikipedia.org/wiki/Aria) project of mine.

## Why use aria?
Maybe you shouldn't. Yet at least. That is, this package is still in it's development infancy, focused first on feature implementation and only secondarily aiming for things like cross-platform reliability and ability to handle all possible workflow scenarios.

That said, if you're a more advanced Stan user that's familiar with [cmdstanr](https://github.com/stan-dev/cmdstanr), you might appreciate the following existing and planned features `aria` provides.

### Implemented features
* Use of `stanc3` for syntax checking in RStudio
* Option to trigger model compilation on save with a `\\compile=1` string at the top of your Stan file
* Smart compilation whereby the saved model is compared to a stored (in `stan_tmp`) representation and compilation only proceeds if *functional* changes to the code have been made. So go ahead and add comments and modify whitespace without fear that they will cause unnecessary model recompilation.

### Features under development
* Background sampling, whereby sampling does not block the main R process, enabling you to work on other things during sampling without creating a new session.
* Diagnostics-driven sampling, whereby the model performance is monitored on a variety of criteria (divergences encountered,rhats,ESS) and terminates only when those criteria are met.
* Nicer progress indicators, including the above noted performance metrics and estimated time remaining.
* During-sampling redirection of output to an [ArviZ](https://arviz-devs.github.io/arviz/)-compliant file format, enabling faster post-sampling access to the output data as well as during-sampling monitoring of the posterior (i.e. in addition to the diagnostics above, the posterior for your parameters).
