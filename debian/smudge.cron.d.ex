#
# Regular cron jobs for the smudge package
#
0 4	* * *	root	[ -x /usr/bin/smudge_maintenance ] && /usr/bin/smudge_maintenance
