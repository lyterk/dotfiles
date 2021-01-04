#!/usr/bin/env expect

set pin [exec /usr/bin/pass Amzn/Midway_PIN]
# set password [exec /usr/bin/pass Amazon/User_Passwords/current]

send_user -- "Enter token: "
expect_user -re "(.*)\n"
send_user "\n"
stty echo
set token $expect_out(1,string)
send_user -- $token

# set mwinit_pin_request "PIN for $env(USER)"
# set mwinit_token_request "Press the button on your"

# spawn /usr/bin/mwinit -o
# expect $mwinit_pin_request
# send "$pin\n"
# expect $mwinit_token_request
# send "$token\n"

expect "$"
send_user
