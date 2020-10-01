function amivpn
    set site (curl --silent https://am.i.mullvad.net/json | jq -r '.mullvad_exit_ip_hostname')
    if [ $site = "null" ]
       set_color red
       echo "Not connected to VPN"
    else
       set_color 32CD32
       echo (echo $site | sed -e 's/-wireguard//')
    end
end
