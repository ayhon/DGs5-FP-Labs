#!/usr/bin/env sh
usage(){
	echo -e "Usage:\n\t$0 lab-name num"
}
[ -n "$1" ] && [ -n "$2" ] \
	&& git clone -b $1 git@gitlab.epfl.ch:lamp/students-repositories-fall-2021/cs210-flealsan.git $2-$1 \
	|| usage
