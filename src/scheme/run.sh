if [ "$#" -ne 1 ]; then
	echo "Usage: bash run.sh <program.scm>"
	exit 1
fi

SCHEME=scheme
$SCHEME --optimize-level 3 --program $1
