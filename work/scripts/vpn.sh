#!/usr/bin/env zsh

begin_dir=$(pwd)

# NOTE Run these while not connected to VPN
cd ~/.password-store
git pull origin mainline
git push origin mainline

cd ~/dotfiles
git pull origin mainline
git push origin mainline

cd $begin_dir

export pin=$(pass Amzn/Midway_PIN)
read "token?Enter token: "

vpn_echo_string="5\nkllyter\n$pin$token\ny"

echo $vpn_echo_string | /opt/cisco/anyconnect/bin/vpn -s connect sea-h-orca.amazon.com

# mwinit_echo_string="$pin\n$token\n"
# echo $mwinit_echo_string | mwinit -o

~/dotfiles/work/scripts/expect.sh
