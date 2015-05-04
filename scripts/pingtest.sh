#!/bin/bash

# https://linuxexpresso.wordpress.com/2009/12/20/conky-ip-monitor/
# use with ${execi 10 ~/.pingtest.sh XXX.XXX.XXX.XXX}
if ping -c 1 -W 2 $1 > /dev/null; then
echo "Up"
else
echo "Down"
fi
