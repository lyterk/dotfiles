#!/usr/bin/env zsh

begin_dir=$(pwd)

# read -rs "ssh_password?Enter SSH password: "
# echo $ssh_password > $HOME/.temp_pass

# # NOTE Run these while not connected to VPN
# cd ~/.password-store
# echo $ssh_password | git pull --rebase origin mainline
# echo "Pull done"
# echo $ssh_password | git push origin mainline
# echo "Push done"

# cd ~/dotfiles
# echo $ssh_password | git pull --rebase origin mainline
# echo "Pull done"
# echo $ssh_password | git push origin mainline
# echo "Push done"

cd $begin_dir

export password=$(pass Amzn/User_Passwords/current)

export pin=$(pass Amzn/Midway_PIN)
read "token?Enter token: "

vpn_echo_string="5\nkllyter\n$pin$token\ny"

echo $vpn_echo_string | /opt/cisco/anyconnect/bin/vpn -s connect sea-h-orca.amazon.com

echo $password | kinit -f
# mwinit_echo_string="$pin\n$token\n"
# echo $mwinit_echo_string | mwinit -o

~/dotfiles/work/scripts/expect.sh $token
