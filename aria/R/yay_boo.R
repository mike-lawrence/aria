yay = function(){
	if(!getOption('aria_sotto_vocce')){
		beepr::beep(system.file("sounds/tada.wav", package="aria"))
	}
}
boo = function(){
	if(!getOption('aria_sotto_vocce')){
		beepr::beep(system.file("sounds/critical_stop.wav", package="aria"))
	}
}
