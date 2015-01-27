#!/bin/sh
#Given a data label, a directory and an output file, this script will
#find all .root files in the given directory and place them in a plain
#text file specified as the output file. Unless the absulute path is given
#for the output file, it is placed in the same directory as the data.

if [ $# = 3 ]
	then
	if [ -d $2 ]
		then
		cd $2
		rm $3
		touch $3
		echo "$1" | cat  >> $3
		ls -1 $2 |
		while read file
			do
			if [ -f $file ]
				then
				suffix=${file:(-5)}
				if [ "$suffix" == ".root" ]
					then
						#echo "conditions met!"
						path="$PWD/$file"
						echo "$path" | cat >> $3
				fi
			fi
		done
	fi
	else
	echo "Please enter 3 arguments, a data label, a directory and an output file"
fi
cd -
