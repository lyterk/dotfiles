import os
import subprocess


def mailpasswd(account):
    path = os.path.expanduser("~/.passwords/{account}.txt.gpg".format(account=account))
    return subprocess.check_output(["gpg", "--quiet", "--batch", "-d", path]).strip()
