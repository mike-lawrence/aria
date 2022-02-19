digests_equal = function(new,old){
	new_digest = digest::digest(new,algo='xxhash64')
	old_digest = digest::digest(old,algo='xxhash64')
	return(old_digest==new_digest)
}
