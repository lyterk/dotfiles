#!/usr/bin/env zsh

pin=$(pass Amzn/Midway_PIN)
read "token?Enter token: "

vpn_echo_string="5\nkllyter\n$pin$token\ny"

# echo $vpn_echo_string | /opt/cisco/anyconnect/bin/vpn -s connect sea-h-orca.amazon.com

mwinit_echo_string="$pin\n$token\n"
echo $mwinit_echo_string | mwinit -o
