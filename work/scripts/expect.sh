#!/usr/bin/env expect

set token [lindex $argv 0]

puts "$token"
set pin [exec /usr/bin/pass Amzn/Midway_PIN]
# set password [exec /usr/bin/pass Amazon/User_Passwords/current]

set mwinit_pin_request "PIN for $env(USER)"
set mwinit_token_request "Press the button on your"

spawn /usr/bin/mwinit -o
expect $mwinit_pin_request
send "$pin\n"
expect $mwinit_token_request
send "$token\n"

expect "$"
send_user ""
