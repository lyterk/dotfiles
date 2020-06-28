dotfiles_dir="/home/kev/.dotfiles/home"
import os
import requests

outfile = os.path.expanduser("~/.dotfiles/home/scripts/rm2svg")

url = "https://github.com/reHackable/maxio/raw/master/tools/rM2svg"
response = requests.get(url).text

try:
    with open(outfile, "w") as f:
        f.write(response)
except Exception as e:
    print(e)
